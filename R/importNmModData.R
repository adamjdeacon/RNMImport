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
		# this is the regular expression for detecting IGNORE statements (there may be multiple)
		
		ignoreRegexp <- "[[:space:]]+IGNORE[[:space:]]*=[[:space:]]*[\\.[:alnum:]\\(\\)\\@\\#\"']+"
		ignorePos <- gregexpr(dataSec, pattern = ignoreRegexp)
		
		# the call to gregexpr returns starting positions and lengths of matches, so now we must extract the actual strings
		
		ignoreText <- substring(dataSec, ignorePos[[1]], ignorePos[[1]] + attr(ignorePos[[1]], "match.length") - 1)
		
		# now extact the actual ignore tokens
		
		ignoreTokens <- sapply(ignoreText, function(x) equalExpressionPop(x, "IGNORE", sep = "[=[:space:]]", absent = "NONE", inPlace = FALSE)$op.out)
		# strip out quotes and "'" 
		ignoreTokens <- sapply(ignoreTokens, gsub, pattern = "['\"]", replacement = "")
		allIgnore <- paste(ignoreTokens, collapse = ";")
		
		# now delete the IGNORE= declarations from dataSec
		
		dataSec <- gsub(dataSec, pattern = ignoreRegexp, replacement = "")
		
		### hunt for the KEEP declaration                                           
		accept <- equalExpressionPop( dataSec, "ACCEPT", sep = "[=[:space:]]" , absent = "",inPlace = TRUE)
		
		### TRANSLATE                                                               
		translate <- equalExpressionPop( dataSec, "TRANSLATE", absent = "", inPlace = TRUE)
		
		### only the filename should be left at this point                          
		dataSec <- stripBlanks( dataSec )
		# fileName <- .getFilePath( dataaSec  , modFile, debug=debug)
		# TODO: The following line might not be correct
		fileName <- .getFile(dataSec)
		
		c( "File" = fileName, "IG" = allIgnore, "ACCEPT" = accept, 
				"REWIND" = rewind, "RECORDS" = records, 
				"TRANSLATE" = translate, "NULL" = null )
	}	
	out <- sapply( x, .extractDataInfo)  
	t(out)
}

