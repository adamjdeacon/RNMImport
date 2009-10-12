# $Rev$
# $LastChangedDate$

validity.NMProblem <- function(object)
{
	TRUE
}

#' A virtual class that represents 
#' @slot problemStatement The contents of the $PROB statement
#' @slot controlStatements A list of parsed sections of the control file
#' @slot reportStatements A list of parsed sections of the output report file 
#' @slot inputData A data.frame of the input data, if available (otherwise an empty data.frame) 
#' @slot outputData Aggregation of the output data
#' @slot additionalVars A data.frame of additional variables created by the user
#' @author fgochez 

setClass("NMProblem", representation("VIRTUAL", 
				problemStatement = "character",
				controlStatements = "list", 
				reportStatements = "list", 
				inputData = "data.frame", outputData = "ANY", additionalVars = "data.frame"),
		validity = validity.NMProblem)

validity.NMBasicModel <- function(object)
{

	# Check for list due to the possibility of output tables with FIRSTONLY option
	test1 <- inherits(object@outputData, "data.frame") | inherits(object@outputData, "list") 
	test2 <- all(diag(object@omegaFinal[,, "estimates"]) >= 0)
	test3 <- nrow(object@additionalVars) == 0 || (nrow(object@additionalVars) %% nrow(object@inputData)) == 0
	test4 <- ncol(object@thetaInitial) == ncol(object@thetaFinal)

	if(!test1) return("Output data is not a data.frame or list\n")
	if(length(object@objectiveFinal) < 1)
		return("Objective function final value not present!")
	if(length(object@thetaFinal) < 1)
		return("No THETA estimates found!")
	if(all(dim(object@omegaFinal) == 0))
		return("No OMEGA estimates found!")

	TRUE
}

#' This class holds the information for a standard single NONMEM problem  
#' @slot parameterIterations A data.frame of the iteration of each parameter estimate, if available
#' @slot objectiveFinal The numeric value of the objective function minimum
#' @slot thetaFinal Final estimates of the "thetas", together with the standard errors, if available (as a matrix with 1 or 2 rows)
#' @slot sigmaFinal Final estimates of the "sigmas", together with the standard errors, if available (as an array  with 1 or 2 matrices)
#' @slot omegaFinal Final estimates of the "omegas", together with the standard errors, if available (as an array  with 1 or 2 matrices) 
#' @slot parameterCovMatrix The variance-covariance of the parameter estimators, if available
#' @slot minInfo A string describing the status of the objective function-minimization
#' @author fgochez

setClass(
		"NMBasicModel", 
		representation("NMProblem",
			parameterIterations = "ANY",
			objectiveFinal = "numeric", 
			thetaInitial = "matrix", sigmaInitial = "array", omegaInitial = "array", 
			thetaFinal = "matrix", sigmaFinal = "array",
			omegaFinal = "array", 	 
			parameterCovMatrix = "matrix",
			parameterCorMatrix = "matrix",
			minInfo = "character"),validity = validity.NMBasicModel
	)

#' Constructs an NMBasicModel object from the control statements and output list statements that apply to it.
#' Meant to be used from within importNm
#' @title NMBasicModel constructor
#' @param controlStatements [list] A list of control file statements for this particular problem  
#' @param path [C,1] path parameter, passed directly to importModelData and importModelOutputTables
#' @param lstContents [list] contents of an lst file that apply to this problem
#' @return An NMBasicModel object holding the problem information
#' @author fgochez

NMBasicModel <- function(controlStatements, path, reportContents, dropInputColumns = FALSE)
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
	# now create the class
	# TODO: The following is too complex, simplify in future releases
	with(reportContents,
			{
				# check for the covariance/correlation matrices
				covMatrix <- if(!is.null(reportContents$CovarianceMatrix)) CovarianceMatrix else matrix()
				corMatrix <- if(!is.null(reportContents$CorrelationMatrix)) CorrelationMatrix else matrix()
				thetaInitial <- t(controlStatements$Theta)
				omegaInitial <- controlStatements$Omega
				sigmaInitial <- controlStatements$Sigma
				if(is.null(sigmaInitial)) sigmaInitial <- matrix()
				rownames(thetaInitial) <- c("upperBound", "initial", "lowerBound")
				
				# if standard errors are available in the lst file,store them with the "XXXFinal" slots
				if(!is.null(reportContents$StandardError))
				{
					
					thetaFinal <-  rbind(StandardError$THETA, FinalEstimates$THETA )
					
					rownames(thetaFinal) <- c("standardErrors","estimates")
					# TODO: Initial omega statements can be omitted - thus the dimension of the omegaInitial and
					# omega final will be different in this case.  The following logic must be updated to deal with this
					#.  See mantis issue 1207
					omegaDim <- dim(FinalEstimates$OMEGA)
					# if no initial omega, fall back on a defualt set of names
					if(is.null(omegaInitial)) {
						omegaInitial <- matrix(NA, nrow = omegaDim[1], ncol = omegaDim[2])
						omegaDimNames <- list(paste( "OMEGA", 1:omegaDim[1], sep = "" ), paste( "OMEGA", 1:omegaDim[2], sep = "" ))
					}
					else omegaDimNames <- dimnames(omegaInitial)
					omegaFinal <- array(dim = c(omegaDim, 2), 
							dimnames = c(omegaDimNames, list(c("estimates", "standardErrors"))))
					omegaFinal[,,"estimates"] <- FinalEstimates$OMEGA
					omegaFinal[,,"standardErrors"] <- StandardError$OMEGA

					sigmaDim <- dim(FinalEstimates$SIGMA)
					# SIGMA can be omitted
					if(is.null(sigmaDim))
					{
						sigmaFinal <- array(dim = c(0,0, 2), dimnames = list(NULL,NULL,
										c("estimates", "standardErrors")))
					}
					else
					{
						sigmaFinal <- array(dim = c(sigmaDim, 2), dimnames = c(dimnames(sigmaInitial),
							list(c("estimates", "standardErrors"))))
						sigmaFinal[,,"estimates"] <- FinalEstimates$SIGMA
						sigmaFinal[,,"standardErrors"] <- StandardError$SIGMA
					}
				}
				else
				{
					thetaFinal <- matrix(FinalEstimates$THETA, nrow = 1, dimnames = list( "estimates" , NULL ))
					
					omegaFinal <- array(FinalEstimates$OMEGA, dim = c(dim(FinalEstimates$OMEGA), 1),
						dimnames = c(dimnames(omegaInitial), list("estimates")))
					
					sigmaDim <- dim(FinalEstimates$SIGMA)					
					if(is.null(sigmaDim))
						sigmaFinal <- array(dim = c(0,0,1), dimnames = list(NULL, NULL, "estimates"))
					else
						sigmaFinal <- array(FinalEstimates$SIGMA, dim = c(dim(FinalEstimates$SIGMA), 1),
					  	dimnames = c(dimnames(sigmaInitial), list("estimates")))
				}
				colnames(thetaFinal) <- colnames(thetaInitial)

				# create the object
				new("NMBasicModel", parameterIterations = reportContents$Iter, 
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
						minInfo = unlist(attr(reportContents$Iter, "min.info")))
			} ) # end with(reportContents)
	
}
