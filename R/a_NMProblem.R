# $LastChangedDate: $
# $LastChangedBy: $
# $Rev: $
# 
# Author: fgochez
###############################################################################


validity.NMProblem <- function(object)
{
	TRUE
}

#' @slot problemStatement The contents of the $PROB statement
#' @slot controlStatements A list of parsed sections of the control file
#' @slot reportStatements A list of parsed sections of the output report file 
#' @slot inputData A data.frame of the input data, if available (otherwise an empty data.frame) 
#' @slot outputData Aggregation of the output data
#' @slot additionalVars A data.frame of additional variables created by the user
#' @author fgochez 

setClass("NMProblem", representation("VIRTUAL", 
				problemStatement = "character",
				controlStatements = "list", 
				reportStatements = "list", 
				nmVersionMajor = "character",
				nmVersionMinor = "numeric",
				inputData = "data.frame", outputData = "ANY", additionalVars = "data.frame"),
		validity = validity.NMProblem)

