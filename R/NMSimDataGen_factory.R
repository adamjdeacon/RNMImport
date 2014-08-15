
NMSimDataGen <- function(controlStatements, path, reportContents = NULL, 
		versionInfo = c("major" = "VI", "minor" = 0))
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
	# TODO: replace this logic since it should never happen once problems are parsed correctly
	
	.Omega <- if(is.null(controlStatements$Omega)) matrix() else controlStatements$Omega
	.Sigma <- if(is.null(controlStatements$Sigma)) matrix() else controlStatements$Sigma
	
	.Theta <- if(is.null(controlStatements$Theta)) numeric(0) else controlStatements$Theta[,"Est"]
	
	with(controlStatements , 
	{
			outTables <- .importTablesSafely(controlStatements$Table, path = path  )
			if(inherits(outTables, "list")) nDataRows <- max(sapply(outTables, nrow))
			else nDataRows <- nrow(outTables)	
			seeds <- as.numeric(ifelse(Sim[c("Seed1", "Seed2")] == -1, NA,	Sim[c("Seed1", "Seed2")]))
				
				# now extract initial value estimates of parameters:
				
			new("NMSimDataGen", nmVersionMajor = versionInfo["major"],
					nmVersionMinor = as.numeric(versionInfo["minor"]), numSimulations = as.numeric(controlStatements$Sim["nSub"]), 
					seeds = seeds, inputData = inData, outputData = outTables, controlStatements = 
							controlStatements, problemStatement = controlStatements$Problem,
					thetaInitial = .Theta, omegaInitial = .Omega, sigmaInitial = .Sigma,
					additionalVars = as.data.frame(matrix(ncol = 0, nrow = nDataRows)),
					reportStatements = reportContents)
	})

}