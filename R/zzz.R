
# The global environment holding metadata for RNMImport
.RNMImportEnv <- new.env()

# The default
DEFAULTLOGS <- c("stdReport", "detailedReport", "warnings", "highLevelParse", "lowLevelParse")

# The file extensions
FILEEXTENSIONS <- list(control = c("mod", "ctl", "con"), report = c("lst", "out", "sep"), 
					outputTable = c("fit", "tab"), inputData = c("", "txt", "dat"))

##################################################################
# initializeLogs
# initializes the log connections in the package environment to certain default values
# Author: F. Gochez
# Added: Dec 25 2008
# Last modified: Dec 25 2008
##################################################################


initializeLogs <- function()
{
	# create the list of logs
	.RNMImportEnv$logConnections <- vector(mode = "list", length = length(DEFAULTLOGS))
	names(.RNMImportEnv$logConnections) <- DEFAULTLOGS
	for(i in seq_along(DEFAULTLOGS))
		.RNMImportEnv$logConnections[[i]] <- NA
	# only this log is not NA by default, and it will be set to stdout
	.RNMImportEnv$logConnections[["stdReport"]] <- stdout()
}

##################################################################
# initializeVariables
# initializes the default configuration for all NONMEM2 variables in the package environment 
# Author: R. Weeks
# Added: Jan 20 2009
# Last modified: Jan 20 2009
##################################################################

initializeVariables <- function(libName = file.path(system.file(), "../"))
{
	
	fileName <- file.path(libName, "RNMImport", "configdata/NONMEM2_Variables.csv" )
	cat("Full path to configuration file: \n", fileName, "\n")
	defaultConfig <- try(read.table(file = fileName, header = TRUE, sep = ",", stringsAsFactors = FALSE), silent = TRUE)
	if(inherits(defaultConfig, "try-error"))
	{
		RNMImportWarning("Could not import NONMEM variable configuration file.\n")
	}else
	{
		.RNMImportEnv$variables <- defaultConfig
	}
}

##################################################################
# initializeFileNameExtensions
# initializes the default file extensions for NONMEM
# Author: R. Weeks
# Added: Jan 20 2009
# Last modified: Jan 20 2009
##################################################################

initializeFileNameExtensions <- function()
{
	.RNMImportEnv$fileExtensions <- FILEEXTENSIONS
}

##################################################################
# initializeDataPath
# initializes a vector to store user data paths
# Author: R. Weeks
# Added: Jan 20 2009
# Last modified: Jan 20 2009
##################################################################

initializeDataPath <- function()
{
	.RNMImportEnv$dataPath <- vector(mode = "character", length = 0)
}

initializeSubsets <- function()
{
	.RNMImportEnv$subsets <- list("default" = c("MDV != 1", "EVID == 0", "AMT <= 0"), applyOnLoad = TRUE)
}

initializeMiscOptions <- function(libName = file.path(system.file(), "../"))
{
	.RNMImportEnv$unitTestPath <- file.path(libName, "RNMImport/unittests") 
}

.onLoad <- function(libname, pkgname)
{
	initializeLogs()
	initializeVariables(libname)
	initializeFileNameExtensions()
	initializeDataPath()
	initializeSubsets()
	initializeMiscOptions()
#	cat("------------------------------------------------------\n")
#	cat("|\n")
#	cat("|\n") 
#	cat("|To execute unit tests, type runRNMImportTests().\n")
#	cat("|Report will be written to RNMImport_internalunit.html\n")
#	cat("|\n")
#	cat("|\n")
#	cat("------------------------------------------------------\n")
	
}

