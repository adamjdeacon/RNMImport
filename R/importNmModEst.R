#
# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# Author: fgochez
###############################################################################


#' Parse $EST statements from a control file and return a list of parsed statements (one element for each $EST) 
#' At the moment, these are just strings, but this will likely change
#' @param txt A vector of strings containing the text of the $EST statements to be parsed 
#' @return A matrix of parsed statements.  Each row of the matrix will correspond to one parsed $EST statement
#' 
#' @author fgochez


.importNmModEst <- function(txt )
{
	estSectionText <- section(txt, "EST", oneline = FALSE, stripout = TRUE, glue = TRUE)
	
	.parseEst <- function(estElement)
	{
		
		x <- equalExpressionPop(estElement, "METHOD", shortcut = TRUE, inPlace = FALSE)
		meth <- x$op.out
		
		x <- equalExpressionPop(x$txt, "FILE", shortcut = TRUE, inPlace = FALSE)
		fileName <- if(!is.null(x$op.out)) x$op.out else ""
		remainingText <- x$txt
		
		c("method" = meth, "file" = fileName, "remainingText" = remainingText)

	}
	
	parsedEstSections <- sapply(estSectionText, .parseEst)
	t(parsedEstSections)
}