
##################################################################
# .importNmModData
# 
# @ modFile [C, 1] - 
# Returns:  A matrix of descriptor information
##################################################################


#' Parses the $DATA statement of a control fiel from some text 
#' @title Parse $DATA statement
#' @param txt [C,+] - vectors holding control file text 
#' @param modFile The name of the control file from which the text comes
#' @param .extract Logical flag.  If TRUE, assumes
#' @return A matrix of descriptor information
#' @author fgochez
#' @note: Original code by R Francois and others



.importNmModData <- function(txt, modFile, 
		.extract = length(grep("^\\$DAT", txt)) > 0 ){
		
	### import the $DATA section of the control file                              
	x <- if(.extract) section( txt, "DAT", "", strip = TRUE, 
						as.list = TRUE, glue = TRUE) else list( txt )
	
	# internal function, meant to be used within sapply only.  Parses a single line of the $DATA section
	
	.extractDataInfo <- function( dataSec )
	{
		### remove nonmem KEYWORDS that we do not wish to import                    
		dataSec <- killRegex( dataSec,  c("CHECKOUT", "NOOPEN") )
		
		### WIDE or NOWIDE                                                          
		wide <- ynPop( dataSec, "WIDE", default = FALSE, inPlace = TRUE)
		
		### NULL                                                                    
		null <- equalExpressionPop( dataSec, "NULL", absent = "", sep="=", inPlace = TRUE)
		
		### REWIND, NOREWIND                                                        
		rewind <- ynPop( dataSec, "REWIND", default = TRUE, inPlace=TRUE)
		
		### records, may be coded NRECS, NRECORDS, RECS, RECORDS                    
		records <- equalExpressionPop( dataSec, "N?RECO?R?D?S", absent = "", sep="=",inPlace = TRUE)
		
		### hunt for the IGNORE declaration      
		# TODO: ignore section can be repeated (apparently), so we need to "pop" "IGNORE" until there are no more
		ignore <- equalExpressionPop( dataSec, "IGNORE", sep = "[=[:space:]]" , absent = "NONE",inPlace = TRUE)
		
		### hunt for the KEEP declaration                                           
		accept <- equalExpressionPop( dataSec, "ACCEPT", sep = "[=[:space:]]" , absent = "",inPlace = TRUE)
		
		### TRANSLATE                                                               
		translate <- equalExpressionPop( dataSec, "TRANSLATE", absent = "", inPlace = TRUE)
		
		### only the filename should be left at this point                          
		dataSec <- stripBlanks( dataSec )
		# fileName <- .getFilePath( dataaSec  , modFile, debug=debug)
		# TODO: The following line might not be correct
		fileName <- .getFile(dataSec)
		
		c( "File" = fileName, "IG" = ignore, "ACCEPT" = accept, 
				"REWIND" = rewind, "RECORDS" = records, 
				"TRANSLATE" = translate, "NULL" = null )
	}	
	out <- sapply( x, .extractDataInfo)  
	t(out)
}

