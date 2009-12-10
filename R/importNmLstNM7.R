# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################


.importMethodBlock <- function(methodTextBlock)
{
	blockResult <- list(  )
	blockResult$method <- attr(methodTextBlock, "method.name")
	
	# retrieve objective function value
	
	objFinalLine <- grep(methodTextBlock, pattern = "#OBJV", value = TRUE)
	objFinalValueLoc <- gregexpr(objFinalLine, pattern = "[0-9\\.]+")
	
	objFinalValue <- as.numeric( substr(objFinalLine, 
					start = objFinalValueLoc[[1]], 
					stop = objFinalValueLoc[[1]] + attr(objFinalValueLoc[[1]], "match.length") - 1 ) )
	
	blockResult$Objective.Final <- objFinalValue
	methodTextBlockSectioned <- sectionMethodBlock(methodTextBlock)
	blockResult$FinalEstimates <- .importNmLstEstimates(methodTextBlockSectioned[["FINAL PARAMETER ESTIMATE"]])
	blockResult$StandardError           <- .importNmLstEstimates( methodTextBlockSectioned[["STANDARD ERROR OF ESTIMATE"]] )
	
	blockResult$CovarianceMatrix <- .importNmLstMatrix( methodTextBlockSectioned[["COVARIANCE MATRIX OF ESTIMATE"        ]] )
	blockResult$CorrelationMatrix       <- .importNmLstMatrix( methodTextBlockSectioned[["CORRELATION MATRIX OF ESTIMATE"       ]] )

	blockResult$InverseCovarianceMatrix <- .importNmLstMatrix( methodTextBlockSectioned[["INVERSE COVARIANCE MATRIX OF ESTIMATE"]] )
	
	blockResult
}

importNmReport.NM7 <- function( fileName, path = NULL )
{
	logMessage("Importing the lst file " %pst% fileName %pst% "\n", logName = "highLevelParse")
	# use getNmPath if necessary
	path <- processPath(path)
	
	logMessage(log = "stdReport", "importNmReport: No control statements found, attempting to deduce problem type...\n")
	content <- scanFile(.getFile(fileName, path) )	
	# if content is NULL, return NULL
	
	if( is.null(content) )  {
		RNMImportWarning(paste("Contents of the list file", fileName, "were empty or read incorrectly"))
		return(NULL)
	}
	
	result <- list(Raw = content)
	content <- cleanReportContents(content)
	
	# Capture the version info - this should not be repeated for each problem
	result$VersionInfo <- nmVersion( content )
	
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
			RNMImportStop("Simulations + fitting problems for NONMEM 7 not yet imported")
			logMessage(log = "stdReport", "Appears to be a simulation+modelling problem\n")
			problemResults[[i]] <- importNmLstSimModel(currentProb, NA)
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
			problemResults[[i]] <- .importNmLstBasicProb.NM7(content)

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
