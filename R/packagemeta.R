#' Gets a variable name and its associated characteristics.
#' @title Get a variable
#' @param varName A vector of variable names
#' @return A data frame with the label, format and type of each variable name
#' @author rweeks, fgochez
#' @export

# Modification by fgochez, feb 9 2009: Corrected to bring into conformity with design spec

getVarDescription <- function(varName)
{
#	if(!(all(varName %in% .RNMImportEnv$variables[["Variable"]])))
#		RNMImportStop(msg = "Variable name not in original configuration.\n")
	varName <- intersect(varName,.RNMImportEnv$variables[["Variable"]] )
	.RNMImportEnv$variables[.RNMImportEnv$variables[["Variable"]] %in% varName, ]
}

#' Sets new characteristics for a required variable.
#' @title Set new variable
#' @param varName The variable name
#' @param varLabel A new label for the variable. Default is NULL
#' @param varFormat A new format for the variable. Default is NULL
#' @param varType A new type for the variable. Default is NULL
#' @return None
#' @author rweeks
#' @export
setVarDescription <- function(varName, varLabel = NULL, varFormat = NULL, varType = NULL)
{
	if(is.null(varLabel) && is.null(varFormat) && is.null(varType))
		RNMImportStop(msg = "You have not specified any changes to the original configuration.\n")
	if(!is.null(varLabel))
		.RNMImportEnv$variables[["Label"]][.RNMImportEnv$variables[["Variable"]] == varName] <- varLabel
	if(!is.null(varFormat))
		.RNMImportEnv$variables[["Format"]][.RNMImportEnv$variables[["Variable"]] == varName] <- varFormat
	if(!is.null(varType))
		.RNMImportEnv$variables[["VarType"]][.RNMImportEnv$variables[["Variable"]] == varName] <- varType
}

#' Adds a new variable and corresponding characteristics to a global data frame.
#' @title Add new variable
#' @param varName The variable name		
#' @param varLabel The variable label		
#' @param varType The variable type	
#' @param varFormat The variable format. Default is NULL
#' @return None
#' @author rweeks
#' @export
addVarDescription <- function(varName, varLabel, varType, varFormat = NULL)
{
	if(varName %in% .RNMImportEnv$variables[["Variable"]])
		RNMImportStop(msg = "Variable name currently exists.\n")
	newRow <- c(varName, varLabel, ifelse(is.null(varFormat), "", varFormat), varType)
	.RNMImportEnv$variables <- rbind(.RNMImportEnv$variables, newRow)
	.RNMImportEnv$variables <- .RNMImportEnv$variables[order(.RNMImportEnv$variables[["Variable"]]), ]
}

#' Gets the conventional file name extensions used by NONMEM.
#' @title Get file extensions
#' @param parameter The name of the file type, e.g. "control" for a control file
#' @return A vector of allowed file type extensions
#' @author rweeks
#' @export
getNmFileExtensions <- function(parameter)
{
	switch(parameter,
		   	"control" = .RNMImportEnv$fileExtensions[["control"]], 
			"report" = .RNMImportEnv$fileExtensions[["report"]], 
			"table" = .RNMImportEnv$fileExtensions[["outputTable"]], 
			"input" = .RNMImportEnv$fileExtensions[["inputData"]],
			RNMImportStop(msg = "Name of file type not recognised.\n"))
}

#' Sets the allowed file extensions to be used.
#' @title Set file extensions
#' @param parameter The name of the file type, e.g. "list" for a list file
#' @param extension A character vector of new file type extensions
#' @return None
#' @author rweeks
#' @export
setNmFileExtensions <- function(parameter, extension)
{
	if(!(parameter %in% c("control", "report", "table", "input")))
		RNMImportStop(msg = "Name of file type not recognised.\n")
	if(parameter == "control")
		.RNMImportEnv$fileExtensions[["control"]] <- extension
	if(parameter == "report")
		.RNMImportEnv$fileExtensions[["report"]] <- extension
	if(parameter == "table")
		.RNMImportEnv$fileExtensions[["outputTable"]] <- extension
	if(parameter == "input")	
		.RNMImportEnv$fileExtensions[["inputData"]] <- extension
}

#' Gets a path stored globally by the user.
#' @title Get user path
#' @param pathName The path name
#' @return The path
#' @author rweeks
#' @export
getNmPath <- function(pathName)
{
	if(!(pathName %in% names(.RNMImportEnv$dataPath)))
		RNMImportStop(msg = "Path name does not exist!.\n")
	return(.RNMImportEnv$dataPath[[pathName]])
}

#' Sets a new path globally to be used by the user. 
#' @title Set new path
#' @param pathName The path name
#' @param path The path
#' @return None
#' @author rweeks
#' @export
setNmPath <- function(pathName, path)
{
	.RNMImportEnv$dataPath[[pathName]] <- path
}

#' Removes a globally stored user path. 
#' @title Remove user path
#' @param pathName The path name
#' @return None
#' @author rweeks
#' @export
removeNmPath <- function(pathName)
{
	numErase <- (1:length(.RNMImportEnv$dataPath))[pathName == names(.RNMImportEnv$dataPath)]
	.RNMImportEnv$dataPath <- .RNMImportEnv$dataPath[-numErase]
	if(!(length(.RNMImportEnv$dataPath)))
		attributes(.RNMImportEnv$dataPath) <- NULL
}

# TODO: documentation

defaultDataSubset <- function()
{
	return(.RNMImportEnv$subsets$default)
}

setDefaultDataSubset <- function(sub, applyOnLoad)
{
	.RNMImportEnv$subsets$default <- sub
	.RNMImportEnv$subsets$applyOnLoad <- applyOnLoad
}