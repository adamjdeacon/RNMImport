# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################

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

getSigmas.NMBasicModel <- function(obj, what = "final", ...)
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

getSigmas.NMBasicModelNM7 <- function(obj, what = "final", method = 1)
{
	methodChosen <- .selectMethod(obj@methodNames, method)
	sigmas <- obj@sigmaFinal[[methodChosen]]
	
	validWhat <- intersect(what, c(PARAMITEMS, "shrinkage"))
	invalidWhat <- setdiff(what, c(PARAMITEMS, "shrinkage"))
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	# check if the sigma final estimate matrix is 1-by-1.  If so, force it to remain as a matrix
	# even when drop = FALSE tries to make it otherwise
	oneByOne <- all(dim(sigmas)[1:2] == c(1,1) )
	finalEstimates <- sigmas
	if(oneByOne) finalEstimates <- matrix(finalEstimates, dimnames = dimnames(sigmas)[1:2])
	
	if(!is.null(obj@sigmaStderr[[methodChosen]]))
	{
		stdErrors <- obj@sigmaStderr[[methodChosen]]
		if(oneByOne) stdErrors <- matrix(stdErrors, dimnames = dimnames(sigmas)[1:2])
	}
	else stdErrors <- NULL
	shrinkage <- obj@EPSShrinkage[methodChosen,]
	
	# initial value depends on the number of the method 
	
	if(method == 1)
		initialValues <- obj@sigmaInitial
	else initialValues <- obj@sigmaFinal[[methodChosen - 1]]
	
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
				},
				"shrinkage" = shrinkage
		)
		# this occurs if the sigmas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix
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
		if("shrinkage" %in% validWhat)
			res$eps.shrinkage <- shrinkage
	}
	res
}

setMethod("getSigmas", signature(obj = "NMBasicModelNM7"), getSigmas.NMBasicModelNM7)

getSigmas.NMRun <- function(obj, what = "final", problemNum = 1, method = 1)
{
	dat <- getProblem(obj, problemNum)
	sigmas <- getSigmas(dat, what = what, method = method)
	sigmas
}
setMethod("getSigmas", signature(obj = "NMRun"), getSigmas.NMRun)

getSigmas.NMSimModel <- function(obj, what = "final", subProblemNum = 1, ...)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	finalEstimates <- obj@sigmaFinal[, , subProblemNum, drop = FALSE]
	
	if(method == 1)
		initialValues <- obj@sigmaInitial
	else initialValues <- obj@sigmaFinal[[methodChosen - 1]]
	
	
	if(length(validWhat) == 1)
	{
		res <- switch(validWhat, 
				"final" = finalEstimates,
				# TODO: if these are length 0, generate an error?
				"initial" = initial
		)
		# this occurs if the omegas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix	
	} # end if length(validWhat) == 1
	else
		res <- list("initial.estimates" = initial, "final.estimates"  = finalEstimates)
	
	res
}

setMethod("getSigmas", signature(obj = "NMSimModel"), getSigmas.NMSimModel)

getSigmas.NMSimModelNM7 <- function(obj, what = "final", subProblemNum = 1, method = 1, ...)
{
	methodChosen <- .selectMethod(obj@methodNames, method)
	sigmas <- obj@sigmaFinal[[methodChosen]]
	
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	finalEstimates <- sigmas[, , subProblemNum, drop = FALSE]
	initial <- obj@sigmaInitial
	
	if(length(validWhat) == 1)
	{
		res <- switch(validWhat, 
				"final" = finalEstimates,
				# TODO: if these are length 0, generate an error?
				"initial" = initial
		)
		# this occurs if the omegas were a 1x1 matrix to begin with.  We wish to force the returned value to be a matrix	
	} # end if length(validWhat) == 1
	else
		res <- list("initial.estimates" = initial, "final.estimates"  = finalEstimates)
	
	res
}

setMethod("getSigmas", signature(obj = "NMSimModelNM7"), getSigmas.NMSimModelNM7)

getSigmas.NMSimDataGen <- function(obj, what = "initial", ...)
{
	obj@sigmaInitial
}

setMethod("getSigmas", signature(obj = "NMSimDataGen"), getSigmas.NMSimDataGen)