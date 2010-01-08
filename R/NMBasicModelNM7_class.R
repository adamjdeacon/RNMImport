# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################

validity.NMBasicModelNM7 <- function(object)
{
	TRUE
}

setClass(
		"NMBasicModelNM7", 
		representation("NMProblem",
				parameterIterations = "ANY",
				objectiveFinal = "numeric",
				methodInfo = "matrix",
				thetaInitial = "matrix", sigmaInitial = "array", omegaInitial = "array", 
				thetaFinal = "list", sigmaFinal = "list",
				omegaFinal = "list",
				thetaStderr = "list", sigmaStderr = "list",
				omegaStderr = "list",
				ETAShrinkage = "numeric",
				EPSShrinkage = "numeric",
				parameterCovMatrices = "list",
				parameterCorMatrices = "list",
				minInfo = "character"),
		validity = validity.NMBasicModelNM7
)

#' Constructs an NMBasicModel object from the control statements and output list statements that apply to it.
#' Meant to be used from within importNm
#' @title NMBasicModel constructor
#' @param controlStatements [list] A list of control file statements for this particular problem  
#' @param path [C,1] path parameter, passed directly to importModelData and importModelOutputTables
#' @param lstContents [list] contents of an lst file that apply to this problem
#' @param versionInfo [C, +] numeric vector that holds
#' @return An NMBasicModel object holding the problem information
#' @author fgochez

NMBasicModelNM7 <- function(controlStatements, path, reportContents, dropInputColumns = FALSE, 
		versionInfo = c("major" = "VII", "minor" = 1))
{
	inData <- try(importModelData(dataStatement = controlStatements$Data,inputStatement = controlStatements$Input, path = path,
					dropCols = dropInputColumns))
	# if we could not read data file for some reason, continue anyway
	if(inherits(inData, "try-error"))
	{
		msg <- paste("Could not import data file.  Error generated was:",
				inData, "\nWill continue importing other components\n")
		inData <- data.frame()
	} # end if(inherits(inData, "try-error"))
	
	# import output tables if the $TABLE statement is present, else outdata is empty
	outTables <- if(!is.null(controlStatements$Table)) 
				importModelOutputTables( tableStatement = controlStatements$Table, path = path ) 
			else
				data.frame()
	
	# need to know how many rows the data has, handle FIRSTONLY case here
	if(inherits(outTables, "list")) nOutDataRows <- max(sapply(outTables, nrow))
	else nOutDataRows <- nrow(outTables)
	nInDataRows <- nrow(inData)
	if(nInDataRows != nOutDataRows)
		RNMImportWarning("Number of rows of output data does not match the number of rows of input data!!\n", match.call())
	
	
	with(reportContents,
			{
				
				# check for the covariance/correlation matrices
				covMatrices <- lapply(MethodResults, "[[", "CovarianceMatrix")
				corMatrices <- lapply(MethodResults, "[[", "CorrelationMatrix")
				
				# grab parameter initial values
				thetaInitial <- t(controlStatements$Theta)
				
				# these may be missing in the control statements, so try to extract them from the reportContents
				omegaInitial <- if(!is.null(controlStatements$Omega)) controlStatements$Omega  else  MethodResults[[1]]$initialEstimates$OMEGA
				
				# grab dimensions of omega final estimates
				omegaDim <- dim(MethodResults[[1]]$FinalEstimates$OMEGA)
				
				# if no initial omega, fall back on a defualt set of names
				
				if(is.null(omegaInitial)) {
					omegaInitial <- matrix(NA, nrow = omegaDim[1], ncol = omegaDim[2])
					omegaDimNames <- list(paste( "OMEGA", 1:omegaDim[1], sep = "" ), paste( "OMEGA", 1:omegaDim[2], sep = "" ))
				}
				
				else omegaDimNames <- dimnames(omegaInitial)
				
				sigmaInitial <- controlStatements$Sigma
				if(is.null(sigmaInitial)) sigmaInitial <- matrix()
				rownames(thetaInitial) <- c("lowerBound", "initial", "upperBound")
				
				# get standard errors
				stdErrors <- lapply(MethodResults, "[[", "StandardError")
				
				# extract lists of final estimates by method
				
				thetaFinal <- lapply(MethodResults, function(x) x$FinalEstimates$THETA)
				omegaFinal <- lapply(MethodResults, function(x) x$FinalEstimates$OMEGA)
				sigmaFinals <- lapply(MethodResult, function(x) x$FinalEstimates$SIGMA)					
				
			#	colnames(thetaFinal) <- colnames(thetaInitial)
				
				objectiveFinal <- lapply(MethodResults, "[[", "Objective.Final")
				methodUsed <- lapply(MethodResults, "[[", "method")
				
				objectiveFinal <- 
				# create the object
				new("NMBasicModel", parameterIterations = NULL, 
						problemStatement = controlStatements$Prob,
						objectiveFinal = Objective.Minimum, 
						parameterCovMatrix = covMatrix,
						parameterCorMatrix = corMatrix,
						thetaInitial = thetaInitial,
						sigmaInitial = sigmaInitial,
						omegaInitial = omegaInitial,					
						thetaFinal = thetaFinal,
						sigmaFinal = sigmaFinal, omegaFinal = omegaFinal,			
						additionalVars = as.data.frame(matrix(ncol = 0, nrow = max( nOutDataRows, nInDataRows ))),
						inputData = inData, 
						outputData = outTables, 
						controlStatements = controlStatements,
						reportStatements = reportContents,
						minInfo = unlist(attr(reportContents$Iter, "min.info")),
						nmVersionMajor = versionInfo["major"],
						nmVersionMinor = as.numeric(versionInfo["minor"])) 
				
			} ) # end with(reportContents)
	
}

