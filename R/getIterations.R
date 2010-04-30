# $LastChangedDate: $
# $LastChangedBy: $
# $Rev: $
# 
# Author: fgochez
###############################################################################

getIterations <- function(obj, problemNum = 1)
{
	RNMImportStop("Not defined for objects of this class")
}

setGeneric("getIterations")

getIterations.NMRun <- function(obj, problemNum = 1)
{
	return(getIterations(getProblem(obj, problemNum = problemNum)))
}

setMethod("getIterations", signature(obj = "NMRun"), getIterations.NMRun)

getIterations.NMProb <- function(obj, problemNum = 1)
{
	obj@parameterIterations
}

setMethod("getIterations", signature(obj = "NMBasicModel"), getIterations.NMProb)
setMethod("getIterations", signature(obj = "NMBasicModelNM7"), getIterations.NMProb)
