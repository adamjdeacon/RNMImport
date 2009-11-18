# $Rev$
# $LastChangedDate$

validity.NMSimModel <- function(object)
{
	test1 <- object@numSimulations == dim(object@thetaFinals)[1] 
	test1 <- test1 & dim(object@omegaFinal)[3] == dim(object@sigmaFinal)[3] 
	test1 <- test1 & length(object@objectiveFinal)  
	if(!test1)
		return("Incompability between number of simulations and dimension of one of the parameter estimates")
	TRUE
}

#' This class holds the results of NONMEM problems that both simulate data and fit the model during each simulation
#' (that is, models with a $SIM statement in the control file but no "ONLY" keyword).
#' @slot numSimulations Number of simulations generated
#' @slot objectiveFinal Vector of final values of the objective functions
#' @slot thetaFinal Final estimates of the "thetas", stored as a matrix with one row for each simulation
#' @slot omegaFinal Final estimates of the "omegas", stored as a 3-d array with one matrix for each simulation
#' @slot sigmaFinal Final estimates of the "sigmas", stored as a 3-d array with one matrix for each simulation
#' @slot thetaInitial Initial values of thetas
#' @slot omegaInitial Initial values of omegas
#' @slot sigmaInitial Initial values of sigmas
#' @slot seeds Values of seeds used for random-number generation
#' @author fgochez
#' @export

setClass("NMSimModel", representation("NMProblem", numSimulations = "numeric" ,
				thetaFinal = "matrix", objectiveFinal = "numeric",
				omegaFinal = "array", sigmaFinal = "array", 
				# thetaStdError = "vector", omegaStdError = "matrix",igmaStdError = "matrix", 
				thetaInitial = "vector", 
				omegaInitial = "matrix", sigmaInitial = "matrix", seeds = "numeric"
			))
		

#' Constructor for the NMSimModel class
#' @param controlStatements Set of parsed control statements 
#' @param path Path where the run is located
#' @param reportContents Parsed contents of the report file
#' @return Newly constructed object 
#' @author fgochez

NMSimModel <- function(controlStatements, path, reportContents, versionInfo = c("major" = "VI", "minor" = 0))
{
	inData <- try(importModelData(dataStatement = controlStatements$Data,inputStatement = controlStatements$Input, path = path))
	# if we could not read data file for some reason, continue anyway
	if(inherits(inData, "try-error"))
	{
		msg <- paste("Could not import data file.  Error generated was:",
				inData, "\nWill continue importing other components\n")
		RNMImportWarning(msg)
		inData <- data.frame()
	} # end if(inherits(inData, "try-error"))
	outTables <- if(!is.null(controlStatements$Table)) 
					importModelOutputTables( tableStatement = controlStatements$Table, path = path ) 
				else
					data.frame()
	# if the output tables are a "list", then there was a FIRSTONLY statment, or for some other reason
	# the number of rows of all of the output tables were not equivalent
	if(inherits(outTables, "list")) nDataRows <- max(sapply(outTables, nrow))
	else nDataRows <- nrow(outTables)
	seeds <- as.numeric(ifelse(controlStatements$Sim[c("Seed1", "Seed2")] == -1, NA,	
					controlStatements$Sim[c("Seed1", "Seed2")]))
	nSim <- as.numeric(controlStatements$Sim["nSub"])
	with(reportContents, 
		{	
			# check how many simulations there are.  If only one, the minimum of the objective funciton is stored
			# differently
			
			
			if(nSim == 1)
				objectiveFinal <- Objective.Minimum
			else
				objectiveFinal <- FinalEstimates$Objective.Minimum
			
			omegaFinal <- FinalEstimates$OMEGA
			# Use parameter labels for names, if any were supplied
			dimnames(omegaFinal)[1:2] <- dimnames(controlStatements$Omega)
			thetaFinal <- FinalEstimates$THETA
			colnames(thetaFinal) <- names(controlStatements$Theta[,"Est"])
			sigmaFinal <- FinalEstimates$SIGMA
			if(!is.null(sigmaFinal))
			{
				dimnames(sigmaFinal)[1:2] <- dimnames(controlStatements$Sigma)
			}
			else
				sigmaFinal <- array(c(0,0, nSim))
			
			new("NMSimModel", numSimulations = nSim, 
					seeds = seeds, inputData = inData, outputData = outTables, controlStatements = 
							controlStatements, problemStatement = controlStatements$Problem,
					thetaInitial = controlStatements$Theta[,"Est"], 
					omegaInitial = controlStatements$Omega, 
					sigmaInitial = controlStatements$Sigma,
					omegaFinal = omegaFinal,
					sigmaFinal = sigmaFinal,
					thetaFinal = thetaFinal,
					objectiveFinal = objectiveFinal,
					additionalVars = as.data.frame(matrix(ncol = 0, nrow = nDataRows)), 
					nmVersionMajor = versionInfo["major"],
					nmVersionMinor = as.numeric(versionInfo["minor"]))
		})
}
