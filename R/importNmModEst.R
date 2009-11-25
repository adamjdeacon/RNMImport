#
# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# Author: fgochez
###############################################################################


#' Parse $EST statements from a control file and return a list of parsed statements (one element for each $EST) 
#' At the moment, these are just strings, but this will likely change
#' @param txt A vector of strings containing the text of the $EST statements to be parsed 
#' @return A list of parsed statements
#' @author fgochez


.importNmModEst <- function(txt )
{
	section(txt, "EST", oneline = FALSE, stripout = TRUE, glue = TRUE)
}