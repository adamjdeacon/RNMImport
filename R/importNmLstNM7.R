# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################


#' This function parses
#' @param methodTextBlock 
#' @title Parse report file #METH text block
#' @return 
#' @author fgochez
#' @export
.importMethodBlock <- function(methodTextBlock)
{
	blockResult <- list(  )
	blockResult$method <- attr(methodTextBlock, "method.name")
	
	# retrieve objective function value
	
	objFinalLine <- grep(methodTextBlock, pattern = "#OBJV", value = TRUE)
	objFinalValueLoc <- gregexpr(objFinalLine, pattern = "-{0,1}[0-9\\.]+")
	
	objFinalValue <- as.numeric( substr(objFinalLine, 
					start = objFinalValueLoc[[1]], 
					stop = objFinalValueLoc[[1]] + attr(objFinalValueLoc[[1]], "match.length") - 1 ) )
	
	blockResult$Objective.Final <- objFinalValue
	methodTextBlockSectioned <- sectionMethodBlock(methodTextBlock)
	blockResult$FinalEstimates <- .importNmLstEstimates(methodTextBlockSectioned$"FINAL PARAMETER ESTIMATE")
	blockResult$StandardError           <- .importNmLstEstimates( methodTextBlockSectioned$"STANDARD ERROR OF ESTIMATE" )
	
	blockResult$CovarianceMatrix <- .importNmLstMatrix( methodTextBlockSectioned$"COVARIANCE MATRIX OF ESTIMATE" )
	blockResult$CorrelationMatrix       <- .importNmLstMatrix( methodTextBlockSectioned$"CORRELATION MATRIX OF ESTIMATE" )

	blockResult$InverseCovarianceMatrix <- .importNmLstMatrix( methodTextBlockSectioned$"INVERSE COVARIANCE MATRIX OF ESTIMATE" )
	
	blockResult
}

#' Parses the contents of a NONMEM 7 report file into a list of parsed components 
#' (see the design info for details on the return structure)
#' @param content [C,+] The report file's text. 
#' @title Import NONMEM 7 report file
#' @return A list with various parsed components of the report file.
#' @author fgochez

importNmReport.NM7 <- function( content )
{
	if( is.null(content) )  {
		RNMImportWarning(paste("Contents of the list file", fileName, "were empty or read incorrectly"))
		return(NULL)
	}
	
	result <- list(Raw = content)
	content <- cleanReportContents(content)
	
	# Capture the version info - this should not be repeated for each problem
	versionInfo <- nmVersion( content )
	# for NONMEM 7, it seems that the version info is stored in the form 7.MINOR.X.  This we must further manipulate
	# the string to obtain major and minor versions
	versionInfoSplit <- strsplit(versionInfo, split = "\\.")[[1]]
	version <- "VII"
	level <- paste(versionInfoSplit[2], versionInfoSplit[3], sep = ".")
	result$VersionInfo <- c("Version" = version, "Level" = level)
	
	partitionedContent <- .reportPartitionedByProblems(content)
	
	# each problem should have its own set of results
	
	problemResults <- vector(mode = "list", length = length(partitionedContent))
	for(i in seq_along(problemResults))
	{
		currentProb <- partitionedContent[[i]]
		# check for the presence of  SIMULATION STEP PERFORMED
		simStep <- any(regexMatches("SIMULATION STEP PERFORMED", txt= currentProb))
		# check for value of objective function
		objFun <- any(regexMatches("MINIMUM VALUE OF OBJECTIVE FUNCTION", txt = currentProb))
		# simulation + model
		if(simStep & objFun)
		{	
		#	RNMImportStop("Simulations + fitting problems for NONMEM 7 not yet imported")
			logMessage(log = "stdReport", "Appears to be a simulation+modelling problem\n")
			problemResults[[i]] <- importNmLstSimModel.NM7(currentProb, NA)
		}
		# only data simulation, no fit step
		else if(simStep & !objFun)
		{	
			RNMImportWarning( "This is a simulation without modelling step, will only return raw contents\n", match.call() )
			problemResults[[i]] <- character(0)
		}
		else
		{
			logMessage(log = "stdReport", "Appears to be a standard model\n")
			problemResults[[i]] <- .importNmLstBasicProb.NM7(currentProb)

		}
	}
	result$problemResults <- problemResults
	result
}

#' Parses the results of a single BASIC MODEL
#' @param contents character vector of text for a single problem statement.
#' @title Import basic problem report results 
#' @return a list containing final estimates, number of individuals, etc. for the problem 
#' @author fgochez
#' @keywords

.importNmLstBasicProb.NM7 <- function(contents)
{
	# extract number of records and individuals
	outList <- list() 
	outList$nRecords     <- colonPop( contents, "TOT\\. NO\\. OF OBS RECS"   , inPlace = FALSE, numeric = TRUE )$op.out
	outList$nIndividuals <- colonPop( contents, "TOT\\. NO\\. OF INDIVIDUALS", inPlace = FALSE, numeric = TRUE )$op.out
	
	methodBlocks <- partitionMethods(contents)
	methodResults <- lapply( methodBlocks, .importMethodBlock)
	outList$MethodResults <- methodResults
	### Find the sections of the list file
	# lstList <- sectionLst( contents )
	
	
	
	### Extract iteration information
	# outList$Iter <- .importNmLstIter( lstList[["MONITORING OF SEARCH"]])
	
	outList
	
}

#' Imports the contents of the report file of a SimModel type problem compiled with NONMEM 7
#' @param [C,+] contents Contents of the report file for a single problem
#' @param [N, 1] numSub Number of sub-problems in the problem 
#' @title import NONMEM7 simulation+model fit problem report contents
#' @return 
#' @author fgochez
#' @keywords

importNmLstSimModel.NM7 <- function(contents, numSub = NA)
{
	contents <- cleanReportContents(contents)
	if(is.na(numSub))
	{
		# find all lines of the form
		#PROBLEM NO.:         1    SUBPROBLEM NO.:      N
		subprobLines <- grep(contents, pattern = "PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*SUBPROBLEM NO\\.\\:[[:blank:]]*[0-9]+[[:blank:]]*$")
		# if there is only one sub-problem, then the above line will not appear, hence the need for the following
		# logic 
		numSub <- if(length(subprobLines) >= 1) length(subprobLines) else 1
		
	}
#	if(numSub > 0)
#	{
	outList <- list()       
	
	# outList$VersionInfo  <- nmVersion( contents ) 
	
	# extract the number of records and individuals in the data
	outList$nRecords     <- colonPop( contents, "TOT\\. NO\\. OF OBS RECS"   , inPlace = FALSE, numeric = TRUE )$op.out
	outList$nIndividuals <- colonPop( contents, "TOT\\. NO\\. OF INDIVIDUALS", inPlace = FALSE, numeric = TRUE )$op.out
	# split off the part of the control file that has the subproblems
	# This line will look as follows (N is any integer greater than 1):
	#PROBLEM NO.:         N     SUBPROBLEM NO.:      1
	subprobStartLine <- 
			grep(contents,
					pattern = "PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*SUBPROBLEM NO\\.\\:[[:blank:]]*1[[:blank:]]*$" )
	if(length(subprobStartLine) == 0 || length(subprobStartLine) > 1 )
		RNMImportStop("Not able to locate first subproblem in simulation lst file\n", call=match.call())
	subprobContents <- tail(contents, -subprobStartLine + 1)
	# cut the subproblems into chunks
	subprobContents <- partitionLstSubproblems(subprobContents)
	
	# HACK ALERT
	# the last subproblem does not terminate with a "1" section break for some reason.  This causes
	# importing to fail for the last subproblem, so we "force" "1" to be the last line.
	subprobContents[[length(subprobContents)]] <- c(subprobContents[[length(subprobContents)]], "1")	
	# import the contents of the individual sub-problems
	subprobStatements <- lapply(subprobContents, .importSubProbNM7)
	
	# get all objective function values for each method and sub-problem.  Structure these into a matrix
	# with one row for each sub-problem, and one column for each method
	
	numMethods <- length(subprobStatements[[1]]$MethodResults)
	numThetas <- length(subprobStatements[[1]]$MethodResults[[1]]$FinalEstimates$THETA)
	omegaDim <- dim(subprobStatements[[1]]$MethodResults[[1]]$FinalEstimates$OMEGA)
	
	objectiveMatrix <- matrix(NA, nrow = numSub, ncol = numMethods)
	
	# thetas will be structured into a 3d arrays, with one row for for each method, 
	# and one column for each theta,  
	# and one sub-matrix for each simulation problems
	
	thetaArray <- array( NA, dim = c(numThetas, numMethods, numSub ) )
	
	# omegas and sigmas will be structured into lists of 3d arrays, with a submatrix for each method result, and one list
	# element for each subproblem
	omegaList <- vector(mode = "list", length = numSub )
	sigmaList <- vector(mode = "list", length = numSub )
	# extract all estimates and objective function values
	
	methodLabels <- paste("METHOD", 1:numMethods, sep = "")
	for(i in seq_along(subprobStatements) )
	{
		objectiveFinals <- sapply(subprobStatements[[i]]$MethodResults, "[[", "Objective.Final" )
		objectiveMatrix[i,] <- objectiveFinals
		thetaFinals <- t(sapply(subprobStatements[[i]]$MethodResults, 
						function(x) x$FinalEstimates$THETA))
		thetaArray[,,i] <- thetaFinals
		
		omegaFinals <- lapply(subprobStatements[[i]]$MethodResults, function(x) x$FinalEstimates$OMEGA)
		omegaFinals <- arrayFromMatrixList(omegaFinals, methodLabels)
		sigmaFinals <- lapply(subprobStatements[[i]]$MethodResults, function(x) x$FinalEstimates$SIGMA)
		sigmaFinals <- arrayFromMatrixList(sigmaFinals, methodLabels)
		
		sigmaList[[i]] <- sigmaFinals
		omegaList[[i]] <- omegaFinals
	}
	
	dimnames(objectiveMatrix) <- list(paste("sim", 1:numSub, sep = ""), paste("METH", seq_along(subprobStatements[[1]]$MethodResults), sep ="") )
	
	# now insert the thetas into a matrix with one row for each subproblem. 
	# thetas <- t(sapply(subprobStatements, function(x) x$THETA))
	
	simLabels <- paste("sim", sep = "", 1:numSub)
	
	dimnames(thetaArray) <- list(methodLabels, names(subprobStatements[[1]]$MethodResults[[1]]$FinalEstimates$THETA)  ,simLabels)
	# give the list elements the names of the simulations from which they came
	names(omegaList) <- simLabels
	names(sigmaList) <- simLabels
	
	outList$FinalEstimates <- list(THETA = thetaArray, 
			OMEGA = omegaList, SIGMA = sigmaList)
	outList$Objective.Minimum <- objectiveMatrix
	
	
#	}
#	else
#	{
#		# if there is only one subproblem, then the file looks virtually identical to a basic model list file
#		outList <- .importNmLstBasicProbNM7(contents)
#		
#		numThetas <- length(outList$MethodResults[[1]]$FinalEstimates$THETA)
#		
#		# however,we need to change the dimensions of some returned data
#		# outList$FinalEstimates$THETA <- matrix(outList$FinalEstimates$THETA, 
#		# 		nrow = 1, dimnames = list("sim1", names(outList$FinalEstimates$THETA)))
#		thetaFinals <- t(sapply(outList$MethodResults, 
#						function(x) x$FinalEstimates$THETA))
#		thetaArray <- array()
#		
#		# outList$FinalEstimates$THETA <-
#		
#		x <- outList$FinalEstimates$OMEGA
#		dim(x) <- c(dim(x), 1)
#		dimnames(x) <- c(dimnames(outList$FinalEstimates$OMEGA), list("sim1"))
#		outList$FinalEstimates$OMEGA <- x
#		
#		x <- outList$FinalEstimates$SIGMA
#		dim(x) <- c(dim(x), 1)
#		dimnames(x) <- c(dimnames(outList$FinalEstimates$SIGMA), list("sim1"))
#		outList$FinalEstimates$SIGMA <- x
#		
#		
#		names(outList$Objective.Minimum) <- "sim1"
#		
#	}
	outList	
}

# internal function meant to be used internally by importNmLstSimModel ONLY
.importSubProbNM7 <- function(txt)
{
	outList <- list() 
	methodBlocks <- partitionMethods(txt)
	methodResults <- lapply( methodBlocks, .importMethodBlock)
	outList$MethodResults <- methodResults
	### Find the sections of the list file
	# lstList <- sectionLst( contents )
	

	
	### Extract iteration information
	# outList$Iter <- .importNmLstIter( lstList[["MONITORING OF SEARCH"]])
	
	outList
}

