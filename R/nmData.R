# $Rev$
# $LastChangedDate$

#' A generic function that extracts input and output data tables from a NONMEM object into either a single consolidated
#' data.frame or a list.  
#' @param obj An object of class NMRun, or one that inherits from NMProblem
#' @param dataTypes Which type of data should be returned, must be "input" and/or "output"
#' @param returnMode Determines how the data should be returned.  If "singleDF", attempts to return a consolidated data.frame of output
#' and input data, if "DFList" returns a list with seperate input and output data
#' @param ... Additional parameters: problemNum to select the problem if obj is of class NMRun, subProblemNum to select a 
#' set of subproblems for simulation problems
#' @title Extract data from run
#' @return A data.frame or a list, depending on the value of \code{returnMode}
#' @author fgochez
#' @keywords utilities

# TODO: add option to extract from derived data

nmData <- function(obj, dataTypes = c("input", "output") , returnMode = c("singleDF", "DFList"), 
		subset = NULL, ...)
{
	returnMode <- match.arg(returnMode)
	RNMImportStop("This method is not implemented for this class\n")
}

setGeneric("nmData")

#' utility function for determining what subset should be applied to a dataset. 
#' @param obj 
#' @param subset The subset parameter passed to nmData - a logical, NULL, or a character vector
#' @return NULL or a character vector
#' @author fgochez

.getSubset <- function(obj, subset)
{
	if( class(subset) == "logical" ) {
		if(subset[1])
			return(dataSubset( obj ))
		else return(NULL)
	}
	# if not logical, dataSub is just equal to subset
	else
		subset
}

nmData.NMBasicModel <- function(obj, dataTypes = c("input", "output") , returnMode = c("singleDF", "DFList"),
		subset = NULL, ...)
{
	
	dataTypes <- intersect(dataTypes, c("input", "output"))
	
	if(length(dataTypes) == 0)
		RNMImportStop("No valid datatypes selected for retrieval\n", match.call())
	
	returnMode <- match.arg(returnMode)
	
	# check for FIRSTONLY
	if(class(obj@outputData) == "list") {
		if("output" %in% dataTypes) 
			RNMImportWarning("FIRSTONLY output data currently ignored\n")
		outputData <- obj@outputData[["normal.tables"]]
		
	}
	else outputData <- obj@outputData
	# if subset is supplied, handle it and store the result in dataSub
	# check that it is logical, and obtain an appropriate subset if it is
	
	dataSub <- .getSubset(obj, subset)
	
	allData = list("input" = obj@inputData, "output" = outputData)
	
	# only one data.frame to return
	
	if(length(dataTypes) == 1)
		return(applyDataSubset(allData[[dataTypes]], dataSub))
	
	# more than one data type
	if(returnMode == "DFList")
		return(lapply(allData[dataTypes], function(x) applyDataSubset(x, dataSub)))
	
	outData <- allData$output
	inData <- allData$input
	inColumns <- colnames(inData)
	outColumns <- colnames(outData)
#	JJ 20 Aug 2010	
	if(dim(outData)[1]<dim(inData)[1]){
		repinData <- lapply(outData[,'ID'], function(ID, inData, outData){
					nTimes <- length(which(inData[,'ID']==ID))
					rowIn <- outData[which(outData[,'ID']==ID),]
					stuff <- as.data.frame(matrix(as.numeric(rowIn), nrow=nTimes, ncol=length(rowIn), byrow=TRUE), stringsAsFactors=FALSE)
				}, inData, outData
		)
#		browser()
		nod <- names(outData)
		outData <- do.call('rbind', repinData)
		names(outData) <- nod
	}
	# otherwise, bind the data together, taking care to deal with repeated data.
	allColumns <- union(inColumns, outColumns)
	clashingColumns <- intersect(inColumns, outColumns)
	
	# no repeated columns, so just return cbind
	
	if(length(clashingColumns) == 0)
	{
		return(applyDataSubset(cbind(inData, outData), dataSub))
	}
	
	# create names of the form "VAR.INPUT", "VAR.OUPUT" etc. for those columns found in both data sets.
	# Note that a variable name with 
	
	# determine the names unique to both input and output data
	
	uniqueIn <- setdiff(inColumns, clashingColumns)
	uniqueOut <- setdiff(outColumns, clashingColumns)
	
	res <- cbind(outData, inData[uniqueIn])
	clashIn <- inData[ ,clashingColumns, drop = FALSE]
	
	names(clashIn) <- paste(clashingColumns, "INPUT", sep = ".")
	applyDataSubset( cbind(res, clashIn), dataSub )
}

setMethod("nmData", signature(obj = "NMBasicModel"), nmData.NMBasicModel)
setMethod("nmData", signature(obj = "NMBasicModelNM7"), nmData.NMBasicModel)

nmData.NMSim <- function(obj, dataTypes = c("input", "output") , 
		returnMode = c("singleDF", "DFList"),  
		subset = NULL, subProblemNum = NA, stackInput = TRUE)
{
	
	# if subset is supplied, handle it and store the result in dataSub
	# check that it is logical, and obtain an appropriate subset if it is
	
	returnMode <- match.arg(returnMode)
	dataSub <- .getSubset(obj, subset)
	
	dataTypes <- intersect(dataTypes, c("input", "output"))
	
	if(length(dataTypes) == 0)
		RNMImportStop("No valid datatypes selected for retrieval\n", match.call())
	
	inData <- obj@inputData
	returnMode <- match.arg(returnMode)
	
	if(class(obj@outputData) == "list") {
		if("output" %in% dataTypes ) 
			RNMImportWarning("FIRSTONLY output data currently ignored\n")
		outData <- obj@outputData[["normal.tables"]]
		
	}
	else outData <- obj@outputData
	if(is.na(subProblemNum)) subProblemNum = 1:obj@numSimulations
	
	if("output" %in% dataTypes)
	{
		
		# create a simulation number factor
		simNum <- gl(obj@numSimulations, nrow(outData) / obj@numSimulations , ordered = TRUE)
		outData <- cbind(outData, "NSIM" = simNum)
		# extract requested simulations
		if(is.na(subProblemNum)) subProblemNum = 1:obj@numSimulations
		outData <- subset(outData, NSIM %in% subProblemNum)
	}
	# only one data.frame to return
	if(length(dataTypes) == 1)
	{
		res <- if(dataTypes == "input") inData else outData
		res <- applyDataSubset(res, dataSub)
		return(res)
	}
	# more than one data type
	if(returnMode == "DFList"){
		res <- list("input" = inData, "output" = outData)
		# take the subsets as needed
		res <- lapply(res, function(x) applyDataSubset(x, sub = dataSub))
		return(res)
	}
	# if stackInput == TRUE, replicate the input data set so that its number of rows matches
	# the number of rows of the simulated output data set
	
	if(stackInput)
		inData <- do.call(cbind.data.frame, lapply(inData, base:::rep, length(subProblemNum)))
	if(nrow(inData) != nrow(outData))
		RNMImportStop("Amount of simulated output data selected is not compatible with the amount of input data, cannot bind into a single data.frame\n",
				call = match.call())
	
	inColumns <- colnames(inData)
	outColumns <- colnames(outData)
	# otherwise, bind the data together, taking care to deal with repeated data.
	allColumns <- union(inColumns, outColumns)
	clashingColumns <- intersect(inColumns, outColumns)
	# no repeated columns, so just return cbind
	if(length(clashingColumns) == 0)
	{
		return(applyDataSubset(cbind(inData, outData), dataSub))
	}
	# create names of the form "VAR.INPUT", "VAR.OUPUT" etc. for those columns found in both data sets.
	# Note that a variable name with 
	
	# determine the names unique to both input and output data
	uniqueIn <- setdiff(inColumns, clashingColumns)
	uniqueOut <- setdiff(outColumns, clashingColumns)
	res <- cbind(outData, inData[uniqueIn])
	clashIn <- inData[,clashingColumns, drop = FALSE]
	
	names(clashIn) <- paste(clashingColumns, "INPUT", sep = ".")
	
	applyDataSubset(cbind(res, clashIn), dataSub)
	
}

setMethod("nmData", signature(obj = "NMSimDataGen"), nmData.NMSim)
setMethod("nmData", signature(obj = "NMSimModel"), nmData.NMSim)
setMethod("nmData", signature(obj = "NMSimModelNM7"), nmData.NMSim)

nmData.NMRun <- function(obj, dataTypes = c("input", "output") , returnMode = c("singleDF", "DFList"),
		subset = NULL, problemNum = 1, subProblemNum = NA)
{
	returnMode <- match.arg(returnMode)
#	browser()
	prob <- getProblem(obj, problemNum)
	nmData(prob, dataTypes,returnMode, subset = subset, subProblemNum)
}

setMethod("nmData", signature(obj = "NMRun"), nmData.NMRun)