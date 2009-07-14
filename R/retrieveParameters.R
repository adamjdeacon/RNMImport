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

getThetas.NMBasicModel <- function(obj, stdErrors = FALSE, initial = FALSE)
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

getThetas.NMRun <- function(obj, stdErrors = FALSE, initial = FALSE, problemNum = 1)
{
	dat <- getProblem(obj, problemNum)
	thetas <- getThetas(dat, stdErrors, initial)
	thetas	
}
setMethod("getThetas", signature(obj = "NMRun"), getThetas.NMRun)

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

#' Extracts omega estimates from a NONMEM object
#' @param obj An object of class NMBasicModel, NMRun or NMSimModel
#' @param stdErrors A boolean value that specifies whether standard errors should be returned
#' @param initial A boolean value that specifies whether initial values of omega should be returned
#' @param ... Additional arguments that apply to different classes. These are problemNum which specifies the run required for NMRun
#'    		  and subProblemNum which specifies the simulation required for NMSimModel
#' @return A 3 dimensional array with a matrix of estimates, and if specified, a matrix of initial values and standard errors
#' @author Mango Solutions <rweeks@mango-solutions.com>

getOmegas <- function(obj, stdErrors = FALSE, initial = FALSE, ...)
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}
setGeneric("getOmegas")

getOmegas.NMBasicModel <- function(obj, stdErrors = FALSE, initial = FALSE)
{
	omegas <- obj@omegaFinal
	thirdDimension <- dim(omegas)[3] 
	if(!stdErrors)
		res <- omegas[,,"estimates", drop = FALSE]
	else
		res <- omegas
	if(initial)
		attr(res, "Initial") <- obj@omegaInitial
	if(stdErrors && thirdDimension == 1 )
		RNMImportWarning(msg = "No standard errors are available!")
	res	
}
setMethod("getOmegas", signature(obj = "NMBasicModel"), getOmegas.NMBasicModel)

getOmegas.NMRun <- function(obj, stdErrors = FALSE, initial = FALSE, problemNum = 1)
{
	dat <- getProblem(obj, problemNum)
	omegas <- getOmegas(dat, stdErrors, initial)
	omegas
}
setMethod("getOmegas", signature(obj = "NMRun"), getOmegas.NMRun)

getOmegas.NMSimModel <- function(obj, stdErrors = FALSE, initial = FALSE, subProblemNum = 1)
{
	if(stdErrors)
		RNMImportWarning(msg = "No standard errors are available!")
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	omegas <- obj@omegaFinal[, , subProblemNum, drop = FALSE]
	if(initial)
		attr(omegas, "Initial") <- obj@omegaInitial
	omegas
}
setMethod("getOmegas", signature(obj = "NMSimModel"), getOmegas.NMSimModel)

#' Extracts sigma estimates from a NONMEM object
#' @param obj An object of class NMBasicModel, NMRun or NMSimModel
#' @param stdErrors A boolean value that specifies whether standard errors should be returned
#' @param initial A boolean value that specifies whether initial values of sigma should be returned
#' @param ... Additional arguments that apply to different classes. These are problemNum which specifies the run required for NMRun
#'    		  and subProblemNum which specifies the simulation required for NMSimModel
#' @return A 3 dimensional array with a matrix of estimates, and if specified, a matrix of initial values and standard errors
#' @author Mango Solutions <rweeks@mango-solutions.com>

getSigmas <- function(obj, stdErrors = FALSE, initial = FALSE, ...)
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}
setGeneric("getSigmas")

getSigmas.NMBasicModel <- function(obj, stdErrors = FALSE, initial = FALSE)
{
	sigmas <- obj@sigmaFinal
	thirdDimension <- dim(sigmas)[3] 
	if(!stdErrors)
		res <- sigmas[,,"estimates", drop = FALSE]
	else
		res <- sigmas
	if(initial)
		attr(res, "Initial") <- obj@sigmaInitial
	if(stdErrors && thirdDimension == 1 )
		RNMImportWarning(msg = "No standard errors are available!")
	res
}
setMethod("getSigmas", signature(obj = "NMBasicModel"), getSigmas.NMBasicModel)

getSigmas.NMRun <- function(obj, stdErrors = FALSE, initial = FALSE, problemNum = 1)
{
	dat <- getProblem(obj, problemNum)
	sigmas <- getSigmas(dat, stdErrors, initial)
	sigmas
}
setMethod("getSigmas", signature(obj = "NMRun"), getSigmas.NMRun)

getSigmas.NMSimModel <- function(obj, stdErrors = FALSE, initial = FALSE, subProblemNum = 1)
{
	if(stdErrors)
		RNMImportWarning(msg = "No standard errors are available!")
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	sigmas <- obj@sigmaFinal[, , subProblemNum, drop = FALSE]
	if(initial)
		attr(sigmas, "Initial") <- obj@sigmaInitial
	sigmas
}
setMethod("getSigmas", signature(obj = "NMSimModel"), getSigmas.NMSimModel)