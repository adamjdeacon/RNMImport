 
#' Coverts FORTRAN 95 operators in a chunk of code (in a character vector) to
#' @param code Character vector of strings of code whose operators should be converted
#' @title Convert FOTRAN 95 code to FORTRAN 77 code by replacing operators
#' @return converted codes
#' @author Mango Solutions
#' @export

convertFortran95Ops <- function( code )
{
	conversionTable <- cbind("Fortran95" = c("==", "/=", ">=", ">", "<=", "<"), 
			"Fortran77" = c(".EQ.", ".NE.", ".GE.", ".GT.", ".LE.", ".LT."))
	
	convertedCode <- code
	
	for(i in 1:nrow(conversionTable))
		convertedCode <- gsub(convertedCode, pattern = conversionTable[i,"Fortran95"], 
				replacement = conversionTable[i, "Fortran77"] )
	
	convertedCode

}
