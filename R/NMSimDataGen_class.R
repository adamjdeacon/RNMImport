# $Rev$
# $LastChangedDate$


validity.NMSimDataGen <- function(object)
{
	if(object@numSimulations < 1)
		return("Number of simulations is less than 1!")
	if(length(object@thetaInitial) < 1)
		return("No initial thetas!")
	TRUE
}

#' This class handles NONMEM models that have a $SIM statement but only simulate
#' data without fitting a model
#' @slot numSimulations Number of simulations/subproblems 
#' @slot seeds Seeds used for the random number generator
#' @slot problemStatement $PROBLEM statement 
#' @slot thetaInitial Fixed/initial theta values used to generate data
#' @slot omegaInitial Fixed/initial omega values used to generate data
#' @slot sigmaInitial Fixed/initial sigma values used to generate data
#' @author fgochez

setClass(
		"NMSimDataGen", 
		representation(
				"NMProblem", numSimulations = "numeric",	seeds = "numeric", 
				thetaInitial = "vector", omegaInitial = "matrix", sigmaInitial = "matrix"
		),validity = validity.NMSimDataGen
)

#' Constructor for NMSimDataGen
#' @param controlStatements Statements from the control file that apply to this problem 
#' @param path Full path to the files where the problem output is located
#' @param reportContents  
#' @return An NMSimDataGen object
#' @author fgochez

NMSimDataGen <- function(controlStatements, path, reportContents = NULL)
{
	inData <- try(importModelData(dataStatement = controlStatements$Data,
					inputStatement = controlStatements$Input, path = path))
	
	# if we could not read data file for some reason, continue anyway	
	if(inherits(inData, "try-error"))
	{
		msg <- paste("Could not import data file.  Error generated was:", 
				inData$message, "\nWill continue importing other components")
		inData <- data.frame()
	} # end if(inherits(inData, "try-error"))
	
	with(controlStatements , 
	{
			outTables <- importModelOutputTables( tableStatement = controlStatements$Table, 
					path = path )
			if(inherits(outTables, "list")) nDataRows <- max(sapply(outTables, nrow))
			else nDataRows <- nrow(outTables)	
			seeds <- as.numeric(ifelse(Sim[c("Seed1", "Seed2")] == -1, NA,	Sim[c("Seed1", "Seed2")]))
				
				# now extract initial value estimates of parameters:
				
			new("NMSimDataGen", numSimulations = as.numeric(controlStatements$Sim["nSub"]), 
					seeds = seeds, inputData = inData, outputData = outTables, controlStatements = 
							controlStatements, problemStatement = controlStatements$Problem,
					thetaInitial = Theta[,"Est"], omegaInitial = Omega, sigmaInitial = Sigma,
					additionalVars = as.data.frame(matrix(ncol = 0, nrow = nDataRows)))
	})

}