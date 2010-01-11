# $LastChangedDate: $
# $Rev: $
# $LastChangedBy: $

# TODO: reduce amount of copy-paste code through refactoring

# parameter-related items that can be extracted

PARAMITEMS <- c("final", "initial", "stderrors")

#' Extracts theta estimates from a NONMEM object
#' @param obj An object of class NMBasicModel, NMRun or NMSimModel
#' @param stdErrors A boolean value that specifies whether standard errors should be returned
#' @param initial A boolean value that specifies whether initial values of theta should be returned
#' @param ... Additional arguments that apply to different classes. These are problemNum which specifies the run required for NMRun
#'    		  and subProblemNum which specifies the simulation required for NMSimModel
#' @return A matrix of named rows for final estimates, initial estimates, standard errors etc. as applicable
#' @author rweeks

getThetas <- function(obj, stdErrors = FALSE, initial = FALSE, ...)
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}
setGeneric("getThetas")

getThetas.NMBasicModel <- function(obj, stdErrors = FALSE, initial = FALSE, returnMode = c("standard", "alternate"),...)
{
	thetas <- obj@thetaFinal
	numRow <- nrow(thetas)
	if(!stdErrors)
		res <- thetas["estimates",, drop = FALSE]
	else
		res <- thetas
	if(initial)
		attr(res, "Initial") <-  obj@thetaInitial
	if(stdErrors && !("standardErrors" %in% rownames(thetas)))
		RNMImportWarning(msg = "No standard errors are available!")
	res
	
}

setMethod("getThetas", signature(obj = "NMBasicModel"), getThetas.NMBasicModel)

getThetas.NMRun <- function(obj, stdErrors = FALSE, initial = FALSE, problemNum = 1, returnMode = c("standard", "alternate"), method = 1)
{
	dat <- getProblem(obj, problemNum)
	thetas <- getThetas(dat, stdErrors, initial, returnMode = returnMode, method = method)
	thetas	
}
setMethod("getThetas", signature(obj = "NMRun"), getThetas.NMRun)

getThetas.NMBasicModelNM7 <- function(obj, stdErrors = FALSE, initial = FALSE, returnMode = c("standard", "alternate"),
			method = 1,...)
{
	methodChosen <- .selectMethod(obj@methodNames, method)
	thetas <- obj@thetaFinal[[methodChosen]]
	numRow <- nrow(thetas)
	if(!stdErrors)
		res <- obj@thetaFinal[[methodChosen]]
	else 
	{
		stdErr <- obj@thetaStderr[[methodChosen]]
		if(is.null(stdErr))
		{
			RNMImportWarning(msg = "No standard errors are available!")
			res <- obj@thetaFinal[[methodChosen]]
		}
		else res <- rbind("estimates" = obj@thetaFinal[[methodChosen]], "standardErrors" = stdErr)
	}
	if(initial)
		attr(res, "Initial") <-  obj@thetaInitial
	res
	
}

setMethod("getThetas", signature(obj = "NMBasicModelNM7"), getThetas.NMBasicModelNM7)

getThetas.NMSimModel <- function(obj, stdErrors = FALSE, initial = FALSE, subProblemNum = 1)
{
	if(stdErrors)
		RNMImportWarning(msg = "No standard errors are available!")
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	thetas <- obj@thetaFinal[subProblemNum,]
	if(initial)
		attr(thetas, "Initial") <- obj@thetaInitial	
	thetas
}
setMethod("getThetas", signature(obj = "NMSimModel"), getThetas.NMSimModel)