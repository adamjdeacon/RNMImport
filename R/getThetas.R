# $LastChangedDate$
# $Rev$
# $LastChangedBy$

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

getThetas <- function(obj, what = "final", ...)
{
	RNMImportStop(msg = "This method is not implemented for this class!")
}
setGeneric("getThetas")

getThetas.NMBasicModel <- function(obj, what = "final",...)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	thetas <- obj@thetaFinal
	numRow <- nrow(thetas)
	
	finalEstimates <- thetas["estimates",, drop = TRUE]

	if("standardErrors" %in% dimnames(thetas)[[1]])
		stdErrors <- thetas["standardErrors",, drop = TRUE]
	else stdErrors <- NULL
	initialValues <- obj@thetaInitial
	
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
		res <- matrix(ncol = ncol(thetas), nrow = 0, dimnames = list(NULL, colnames(thetas)))
		if("initial" %in% validWhat) res <- rbind(res, initialValues )
		if("final" %in% validWhat) res <- rbind(res, "estimates" = finalEstimates)
		if( "stderrors" %in% validWhat )
		{
			if(is.null(stdErrors)) RNMImportWarning("Standard errors not available \n")
			else res <- rbind(res, "standardErrors" = stdErrors)
		}
	}
	res
	
}

setMethod("getThetas", signature(obj = "NMBasicModel"), getThetas.NMBasicModel)

getThetas.NMRun <- function(obj, what = "final", problemNum = 1, method = 1)
{
	dat <- getProblem(obj, problemNum)
	thetas <- getThetas(dat, what = what, method = method)
	thetas	
}
setMethod("getThetas", signature(obj = "NMRun"), getThetas.NMRun)

getThetas.NMBasicModelNM7 <- function(obj, what = "final",	method = 1,...)
{
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	
	if(length(invalidWhat)) RNMImportWarning("Invalid items chosen:" %pst% paste(invalidWhat, collapse = ","))
	
	methodChosen <- .selectMethod(obj@methodNames, method)
	thetas <- obj@thetaFinal[[methodChosen]]
	
	finalEstimates <- thetas 
	stdErrors <- obj@thetaStderr[[methodChosen]]

	initialValues <- obj@thetaInitial
	
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
		res <- matrix(ncol = length(thetas), nrow = 0, dimnames = list(NULL, names(thetas)))
		if("initial" %in% validWhat) res <- rbind(res, initialValues )
		if("final" %in% validWhat) res <- rbind(res, "estimates" = finalEstimates)
		if( "stderrors" %in% validWhat )
		{
			if(is.null(stdErrors)) RNMImportWarning("Standard errors not available \n")
			else res <- rbind(res, "standardErrors" = stdErrors)
		}
	}
	res
	
}

setMethod("getThetas", signature(obj = "NMBasicModelNM7"), getThetas.NMBasicModelNM7)

getThetas.NMSimModel <- function(obj, what = "final", subProblemNum = 1, ...)
{
	
	validWhat <- intersect(what, PARAMITEMS)
	invalidWhat <- setdiff(what, PARAMITEMS)
	if("stderrors" %in% validWhat)
		RNMImportWarning(msg = "No standard errors are available!")
	
	numSimulations <- obj@numSimulations
	if(any(!(subProblemNum %in% 1:numSimulations)))
		RNMImportStop(msg = "Subproblem number is not valid!")	
	
	finalEstimates <- obj@thetaFinal[subProblemNum,]
	
	initial <- obj@thetaInitial
	
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
setMethod("getThetas", signature(obj = "NMSimModel"), getThetas.NMSimModel)