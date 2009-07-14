# $LastChangedDate$
# $Rev$

#' Retrieved the variance-covariance matrix of the estimators and optionally
#'  the correlation and inverse correlation matrices of the selected NONMEM run
#' @title Returns variance-covariance matrix, if available, from a NONMEM object 
#' @param obj An object of class NMRun or NMBasicModel
#' @param corMatrix TRUE or FALSE, Not implemented yet
#' @param invCorMatrix TRUE or FALSE, Not implemented yet}
#' @param ... 
#' @return A matrix if just the covariance matrix is required, a list of matrices otherwise
#' @author Mango Solutions
#' @keywords

#  Author: F Gochez

getEstimateCov <- function(obj, corMatrix = FALSE, invCorMatrix = FALSE, ...)
{
	RNMImportStop("getEstimateCov not implemented for this class yet\n", match.call())
}

setGeneric("getEstimateCov")

getEstimateCov.NMRun <- function(obj, corMatrix = FALSE, invCorMatrix = FALSE, problemNum = 1)	getEstimateCov(getProblem(obj, problemNum), corMatrix = corMatrix)

setMethod("getEstimateCov", signature(obj = "NMRun"), getEstimateCov.NMRun)

getEstimateCov.NMBasicModel <- function(obj, corMatrix = FALSE, invCorMatrix = FALSE)
{
	if(!corMatrix | all(dim(obj@parameterCorMatrix) == 0)) obj@parameterCovMatrix
	else list("covariance" = obj@parameterCovMatrix, "correlation" = obj@parameterCorMatrix)
	
}

setMethod("getEstimateCov", signature(obj = "NMBasicModel"), getEstimateCov.NMBasicModel)


#' Retrieves the final value of the objective function together with the minimization 
#' info for a NONMEM problem
#' @param obj NMRun, or problem inheriting from NMProblem 
#' @param addMinInfo Logical flag.  Should the minimization info be added if it's available?
#' @param ... Additional parameters passed to other methods, such as problemNum and subProblems
#' @title Retrieve objective function value
#' @return A numeric with the value, with the minimization info as a vector added as an attribute "minInfo"
#' @author fgochez
#' @keywords utils

getObjective <- function(obj, addMinInfo = TRUE, ...)
{
	RNMImportStop("getObjective not implemented for this class type")
}

setGeneric("getObjective")

getObjective.NMRun <- function(obj, addMinInfo=TRUE, subProblems=1, problemNum=1)
{
	getObjective(getProblem(obj, problemNum), addMinInfo, subProblems)
}

setMethod("getObjective", signature(obj="NMRun"), getObjective.NMRun)

getObjective.NMBasicModel <- function(obj, addMinInfo=TRUE, ...)
{
	objective <- obj@objectiveFinal
	if(addMinInfo)
	{
		if(is.null(obj@minInfo))
			RNMImportWarning("Minimization data is missing, will not return\n")
		else
			attr(objective, "minInfo") <- obj@minInfo	
	}
	objective
}

setMethod("getObjective", signature(obj="NMBasicModel"), getObjective.NMBasicModel)

# check that this works

getObjective.NMSimModel <- function(obj, addMinInfo = TRUE, subProblems = 1,...)
{	
	
	obj@objectiveFinal[subProblems]
}

setMethod("getObjective", signature(obj="NMSimModel"), getObjective.NMSimModel)


#' Retrieves information about a run's control and report files as a data.frame
#' @param run Object of class NMRun
#' @title Retrieve file information 
#' @return A data.frame with 2 rows, 1 describing the report file and the other the control file
#' @author fgochez
#' @keywords utility

getFileinfo <- function(run)
{
	assertClass(run, "NMRun")
	rbind("controlFile" = run@controlFileInfo, "reportFile" = run@reportFileInfo)
}

getReporttext <- function(run) 	run@reportText
getControltext <- function(run) run@controlText


#' 
#' @param obj 
#' @param ... 
#' @title
#' @return 
#' @author fgochez
#' @keywords

getControlStatements <- function(obj, ...)
{
	RNMImportStop("This function is not implemented for objects of this class")
}

setGeneric("getControlStatements")

getControlStatements.NMRun <- function(obj, problemNum = 1)
{
	getControlStatements(getProblem(obj, problemNum))
}

setMethod("getControlStatements", signature(obj = "NMRun"), getControlStatements.NMRun)

getControlStatements.NMProblem <- function(obj, ...) 
{
	obj@controlStatements
}

setMethod("getControlStatements", signature(obj = "NMProblem"), getControlStatements.NMProblem)
