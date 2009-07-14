# $Rev$
# $LastChangedDate$

#' Obtain the derived/user-added variables from a NONMEM problem object
#' @title Get added data
#' @param obj An object of class NMRun or extending NMProblem
#' @param ... problemNum for addedData.NMRun
#' @return A data.frame containing the derived variables
#' @details Most of these added variables will probably be created via
#' addDerivedCategorical
#' @author Mango Solutions
#' @keywords utils
#' @export 

# Individual author: F. Gochez

addedData <- function(obj, ...)
{
	RNMImportStop("Not implemented for this class yet!\n")
}

setGeneric("addedData")

addedData.NMRun <-function(obj, problemNum = 1)
{
	addedData(getProblem(obj, problemNum = 1))
}

setMethod("addedData", signature(obj = "NMRun"), addedData.NMRun)

addedData.NMProblem <- function(obj)
{
	obj@additionalVars
}

setMethod("addedData", signature(obj = "NMProblem"), addedData.NMProblem)