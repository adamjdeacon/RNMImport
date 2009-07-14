

#' Renames IPRE to IPRED, IWRE to IWRES and adds absWRES to a NONMEM data table
#' @param df data.frame of NONMEM input or output table data
#' @title Rename certain NONMEM columns
#' @return 
#' @author fgochez
#' @keywords

.deriveNmColumns <- function(df)
{
	dfNames <- names(df)

	names(df) <- replace(dfNames, which(dfNames == "IPRE"), "IPRED")
	names(df) <- replace(dfNames, which(dfNames == "IWRE"), "IWRES")
	if("WRES" %in% dfNames)
		df$absWRES <- abs(df$WRES)
	df
}

