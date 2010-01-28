
#' Generates a formal warning. This function is meant to provide custom warning generation for this package.
#' @param msg 
#' @param call 
#' @title Generate a warning
#' @return None
#' @author fgochez
#' @keywords error
#' @export

RNMImportWarning <- function(msg = "Warning!\n", call = NULL)
{
	if(!is.null(call))
		cat(paste("In call: ", call[1],"(", paste(call[-1], collapse = ","), ")\n"))
	warning(simpleWarning(msg, call))
}


#' Generates a formal exception, and prints the sequence of calls that generated this expression.  This function
#' is meant to provide custom error handling for this package.
#' @title Generate an exception and message
#' @param msg Message 
#' @param call (optional).  Call information for function that generated exception (i.e. from match.call)
#' @return None
#' @author fgochez
#' @keywords error
#' @export

RNMImportStop <- function(msg = "Error!\n",  call = NULL)
{
	if(!is.null(call))
		cat(paste("In call: ", call[1],"(", paste(call[-1], collapse = ","), ")\n"), file = stderr())
	dump.frames()
	cat(paste(names(head(last.dump, -1)), collapse = " >> \n "), "\n", file = stderr())
	
	stop(simpleError(msg, call))
}

RNMImportStopifnot <-function(condition, msg = NULL, call = NULL)
{
	if(condition) return()
	if(is.null(msg))
		msg <- "The following condition failed: " %pst% deparse(substitute(condition)) 
	RNMImportStop(msg, call = call)
}

##################################################################
# assertClass
# Checks that an object inherits from some class, generates an error if it does not
# Author: F. Gochez
# Added: Dec 25 2008
# Last modified: Dec 25 2008
# parameters :
# @ object - The object whose class you wish to check
# returns - nothing
##################################################################

assertClass <- function(object, targetClass)
{
	if(!inherits(object, targetClass))
	{
		msg <- paste(deparse(substitute(object)), "is not of class", targetClass)
		RNMImportStop(msg)
	}
	invisible(NULL)
}