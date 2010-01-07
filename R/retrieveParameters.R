# $LastChangedDate: $
# $Rev: $
# $LastChangedBy: $

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

getThetas.NMBasicModel <- function(obj, stdErrors = FALSE, initial = FALSE, returnMode = c("standard", "alternate"))
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

getThetas.NMRun <- function(obj, stdErrors = FALSE, initial = FALSE, problemNum = 1, returnMode = c("standard", "alternate"))
{
	dat <- getProblem(obj, problemNum)
	thetas <- getThetas(dat, stdErrors, initial, returnMode = returnMode)
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
#' @param returnMode (NMRun or NMBasicProblem) Modifies how results are returned.  If "standard", a 3d structure is always returned
#'    		  and subProblemNum which specifies the simulation required for NMSimModel
#' @return A 3 dimensional array with a matrix of estimates, and if specified, a matrix of initial values and standard errors
#' @author Mango Solutions <rweeks@mango-solutions.com>

getOmegas <- function(obj, what = "final", ...)
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}
setGeneric("getOmegas")

getOmegas.NMBasicModel <- function(obj, what = "final")
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	omegas <- obj@omegaFinal
	
	# check if the matrices are one-by-one.  If they are, then we will have to coerce to a matrix later on
	
	oneByOne <- all(dim(omegas)[1:2] == c(1,1) )
	finalEstimates <- omegas[,,"estimates", drop = TRUE]
	if(oneByOne) finalEstimates <- matrix(finalEstimates, dimnames = dimnames(omegas)[1:2])
	if("standardErrors" %in% dimnames(omegas)[[3]])
	{
		stdErrors <- omegas[,,"standardErrors", drop = TRUE]
		if(oneByOne) stdErrors <- matrix(stdErrors, dimnames = dimnames(omegas)[1:2])
	}
	initialValues <- obj@omegaInitial
	if(oneByOne) initialValues <- matrix(initialValues, dimnames = dimnames(omegas)[1:2])
	# no valid option selected, thrown an error
	if(length(validWhat) == 0) RNMImportStop("No valid items selected for retrieval!", call = match.call())
	if(length(validWhat) == 1)
	{
		res <- switch(validWhat, 
				"final" = finalEstimates,
				# TODO: if these are length 0, generate an error?
				"initial" = initialValues,
				"stderrors" = {
					if(is.null(stdErrors))
						RNMImportStop("Standard errors not available \n", call = match.call())
					stdErrors
				}
				)
		# this occurs if the omegas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix	
	} # end if length(validWhat) == 1
	else
	{
		res <- list()
		# TODO: check for missing initial values?
		if("initial" %in% validWhat) res$initial.estimates <- initialValues
		if("final" %in% validWhat) res$final.estimates <- finalEstimates
		if("stderrors" %in% validWhat) 
		{
			if(is.null(stdErrors)) RNMImportWarning("Standard errors not available \n")
			else res$standard.errors <- stdErrors
		}
	}
	res
}
setMethod("getOmegas", signature(obj = "NMBasicModel"), getOmegas.NMBasicModel)

getOmegas.NMRun <- function(obj, what = "final", problemNum = 1)
{
	dat <- getProblem(obj, problemNum)
	omegas <- getOmegas(dat, what = what)
	omegas
}
setMethod("getOmegas", signature(obj = "NMRun"), getOmegas.NMRun)

getOmegas.NMSimModel <- function(obj, what = "final", subProblemNum = 1)
{
#	if(stdErrors)
#		RNMImportWarning(msg = "No standard errors are available!")
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	omegas <- obj@omegaFinal[, , subProblemNum, drop = FALSE]
#	if(initial)
		#attr(omegas, "Initial") <- obj@omegaInitial
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

getSigmas <- function(obj, what = "final", ...)
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}
setGeneric("getSigmas")

getSigmas.NMBasicModel <- function(obj, what = "final")
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	sigmas <- obj@sigmaFinal
	oneByOne <- all(dim(sigmas)[1:2] == c(1,1) )
	finalEstimates <- sigmas[,,"estimates", drop = TRUE]
	if(oneByOne) finalEstimates <- matrix(finalEstimates, dimnames = dimnames(sigmas)[1:2])
	if("standardErrors" %in% dimnames(sigmas)[[3]]) 
	{
		stdErrors <- sigmas[,,"standardErrors", drop = TRUE]
		if(oneByOne) stdErrors <- matrix(stdErrors, dimnames = dimnames(sigmas)[1:2])
	}
	
	initialValues <- obj@sigmaInitial
	if(oneByOne) initialValues <- matrix(initialValues, dimnames = dimnames(sigmas)[1:2])
	
	# no valid option selected, thrown an error
	if(length(validWhat) == 0) RNMImportStop("No valid items selected for retrieval!", call = match.call())
	if(length(validWhat) == 1)
	{
		res <- switch(validWhat, 
				"final" = finalEstimates,
				# TODO: if these are length 0, generate an error?
				"initial" = initialValues,
				"stderrors" = {
					if(is.null(stdErrors))
						RNMImportStop("Standard errors not available \n", call = match.call())
					stdErrors
				}
		)
		# this occurs if the sigmas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix
		if(is.null(dim(res))) 
		{ 
			dim(res) <- c(1,1)
			dimnames(res) <- dimnames(sigmas)[1:2]	
		}	
	} # end if length(validWhat) == 1
	else
	{
		res <- list()
		# TODO: check for missing initial values?
		if("initial" %in% validWhat) res$initial.estimates <- initialValues
		if("final" %in% validWhat) res$final.estimates <- finalEstimates
		if("stderrors" %in% validWhat) 
		{
			if(is.null(stdErrors)) RNMImportWarning("Standard errors not available \n")
			else res$standard.errors <- stdErrors
		}
	}
	res
}
setMethod("getSigmas", signature(obj = "NMBasicModel"), getSigmas.NMBasicModel)

getSigmas.NMRun <- function(obj, what = "final", problemNum = 1)
{
	dat <- getProblem(obj, problemNum)
	sigmas <- getSigmas(dat, what = what)
	sigmas
}
setMethod("getSigmas", signature(obj = "NMRun"), getSigmas.NMRun)

getSigmas.NMSimModel <- function(obj, what = "final", subProblemNum = 1)
{
#	if(stdErrors)
#		RNMImportWarning(msg = "No standard errors are available!")
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	sigmas <- obj@sigmaFinal[, , subProblemNum, drop = FALSE]
#	if(initial)
#		attr(sigmas, "Initial") <- obj@sigmaInitial
	sigmas
}

setMethod("getSigmas", signature(obj = "NMSimModel"), getSigmas.NMSimModel)