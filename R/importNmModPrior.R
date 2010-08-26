###################################################################
# $Rev$
# $LastChangedDate$
# $LastChangedBy: $
###############################################################################

#' Parses the $PRIOR statement of NONMEM 7 control files.  The parsed statement is turned into a named vector
#' @title Parse PRIOR statement
#' @param txt Control file text containing a $PRIOR statement 
#' @param rx 
#' @param fileName  
#' @return A named vector with entries for the prior : nEta, nThp, nEtp, npExp, nTheta.  There will also be an
#' NWPRI attribute which will be true or false depending on whether or not NWPRI is present
#' @author fgochez
#' @export

.importNmModPrior <- function(
		txt = NULL,       
		rx = .getPattern('priors'), 		 
		fileName = NULL
	)
{
	if(is.null(txt)) txt <- scanFile(fileName)
	
	priorText <- section(txt, "PRIOR", as.list = FALSE, stripout = TRUE )
	# extract the comments
	comments <- stripBlanks( commentPop( priorText, inPlace = TRUE ) )
	# check that "NWPRI" is present
	nWpri <- pop(priorText, "NWPRI", inPlace = TRUE)
	# replace commas with blanks so that equalExpressionPop can be used correctly
	priorText <- gsub(priorText, pattern = "\\,", replacement = " ")
	
	# extract the individual elements of the form NTHETA=X, NETA=Y, etc.
	
	nTheta <- as.numeric(equalExpressionPop( priorText, "NTHETA", shortcut = TRUE, inPlace = FALSE)$op.out)
	nEta <- as.numeric(equalExpressionPop(priorText, "NETA", shortcut = TRUE, inPlace = FALSE)$op.out)
	
	nThp <- as.numeric(equalExpressionPop(priorText, "NTHP", shortcut = TRUE, inPlace = FALSE)$op.out)
	nEtp <- as.numeric(equalExpressionPop(priorText, "NETP", shortcut = TRUE, inPlace = FALSE)$op.out)
	npExp <- as.numeric(equalExpressionPop(priorText, "NPEXP", shortcut = TRUE, inPlace = FALSE)$op.out)
	
	# res <- as.numeric(c( nTheta = nTheta, nEta = nEta, nThp = nThp, npExp = npExp, nEtp = nEtp ))
	res <- c( nTheta = nTheta, nEta = nEta, nThp = nThp, npExp = npExp, nEtp = nEtp )
	
	attr(res, "NWPRI") <- nWpri
	res
	
}