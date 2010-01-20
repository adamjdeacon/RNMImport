# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################

PARAMITEMS <- c("final", "initial", "stderrors")

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

getOmegas.NMBasicModel <- function(obj, what = "final" , ...)
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

#' 
#' @param availableMethods 
#' @param method 
#' @title
#' @return 
#' @author fgochez

.selectMethod <- function(availableMethods, method)
{
	methodChosen <- method[1]
	
	# assume that a method name has been chosen, so match it to the names of methods used
	
	if(class(methodChosen) == "character")
	{
		methodChosen <- match(methodChosen, availableMethods)
		if(length(methodChosen) > 1) methodChosen <- methodChosen[1]
		else if(length(methodChosen) == 0)
			RNMImportStop("Invalid method name selected: " %pst% methodChosen)
	}
	assertClass(methodChosen, "numeric")
	if(methodChosen < 1 | methodChosen > length(availableMethods)) RNMImportStop("Invalid method index")
	methodChosen
}

getOmegas.NMBasicModelNM7 <- function(obj, what = "final", method = 1)
{
	validWhat <- intersect(what, c(PARAMITEMS, "shrinkage"))
	invalidWhat <- setdiff(what, c(PARAMITEMS, "shrinkage"))

	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	
	# select the method
	methodChosen <- .selectMethod(obj@methodNames, method)
	omegas <- obj@omegaFinal[[methodChosen]]
	
	# check if the matrices are one-by-one.  If they are, then we will have to coerce to a matrix later on
	
	oneByOne <- all(dim(omegas)[1:2] == c(1,1) )
	finalEstimates <- omegas
	if(oneByOne) finalEstimates <- matrix(finalEstimates, dimnames = dimnames(omegas)[1:2])
	if(!is.null(obj@omegaStderr[[methodChosen]]))
	{
		stdErrors <- obj@omegaStderr[[methodChosen]]
		if(oneByOne) stdErrors <- matrix(stdErrors, dimnames = dimnames(omegas)[1:2])
	}
	else stdErrors <- NULL
	
	shrinkage <- obj@ETAShrinkage[methodChosen,]
	
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
				},
				"shrinkage" = shrinkage
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
		if("shrinkage" %in% validWhat) res$eta.shrinkage <- shrinkage
	}
	res
}

setMethod("getOmegas", signature(obj = "NMBasicModelNM7"), getOmegas.NMBasicModelNM7)

getOmegas.NMRun <- function(obj, what = "final", problemNum = 1, method = 1)
{
	dat <- getProblem(obj, problemNum)
	omegas <- getOmegas(dat, what = what, method = method)
	omegas
}
setMethod("getOmegas", signature(obj = "NMRun"), getOmegas.NMRun)

getOmegas.NMSimModel <- function(obj, what = "final", subProblemNum = 1,...)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	finalEstimates <- obj@omegaFinal[, , subProblemNum, drop = FALSE]
	initial <- obj@omegaInitial
	
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
setMethod("getOmegas", signature(obj = "NMSimModel"), getOmegas.NMSimModel)

getOmegas.NMSimModelNM7 <- function(obj, what = "final", subProblemNum = 1, method = 1, ...)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	# select the method
	methodChosen <- .selectMethod(obj@methodNames, method)
	omegas <- obj@omegaFinal[[methodChosen]]
	
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	finalEstimates <- omegas[, , subProblemNum, drop = FALSE]
	initial <- obj@omegaInitial
	
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
setMethod("getOmegas", signature(obj = "NMSimModelNM7"), getOmegas.NMSimModelNM7)