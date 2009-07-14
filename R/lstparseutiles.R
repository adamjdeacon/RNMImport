# TODO: this function is overly-complex; clean it so that it no longer recurses

#' Partitions the contents of a list file into sections that can then be parsed individually
#' @param fileContents [C,+] Lines of text from a list ifle
#' @param sep (?) passed to .cleanRx
#' @param keep.all (?)
#' @param recurse Should the function be called recursively?  
#' @note Based on code by R. Francois
#' @return A (partially) named list with relevant sections of the lst file.
#' @author fgochez

sectionLst <- function( fileContents, sep, 
		keep.all = FALSE, recurse = TRUE )
{         
	
	### internal function to clean a regex
	.cleanRx <- function(x, sep){
		rx <- sprintf("^%s", sep)
		x <- gsub( rx, "", x)
		negGrep( "^[[:space:]]*$", x , value = TRUE)
	}
	
	if(missing(sep)){
		# sep <- if( fileContents %a~% "^2" ) "2" else "1"
		sep <- if(any(regexMatches(fileContents, rx = "^2") )) "2" else "1"
	}

	if( is.null(fileContents) ) return(NULL)
	
	rx <- sprintf("^%s", sep )
	start <- grep( rx, fileContents )
	appendBefore <- !1 %in% start
	appendAfter <- !length(fileContents) %in% start
	if( appendBefore ) start <- c( 1, start )
	if( appendAfter  ) start <- c( start, length(fileContents)+1 )
	
	len.sta <- length(start) - 1
	li <- list()    
	for( i in 1:len.sta){
		current <- fileContents[seq(from = start[i], to = start[i+1] - 1)]
		current <- .cleanRx( current, sep ) 
		if( sep == "2" &&  recurse ){
			current <- sectionLst( current, "1" )
		}    
		li[[i]] <- current
	}   
	
	if( sep == "1"){  
		### try to find star titles
		if( sep == "1" && len.sta > 2 ) {      
			for(i in seq(along = li) ){
				nmt <- nmTitle( li[[i]] )
				if( !is.null(nmt$title) ){
					li[[i]] <- nmt$text
					names(li)[i] <- nmt$title
				}
				if( length( idx <- grep("MONITORING OF SEARCH", li[[i]])) ){
					names(li)[i] <- "MONITORING OF SEARCH"
					li[[i]] <- li[[i]][-idx]
					li[[i]] <- sectionLst( li[[i]], "0")
				}
			}
		}
		### handle repeated headers  
		headers <- c("INITIAL PARAMETER ESTIMATE", 
				"FINAL PARAMETER ESTIMATE", "STANDARD ERROR OF ESTIMATE", 
				"COVARIANCE MATRIX OF ESTIMATE", "CORRELATION MATRIX OF ESTIMATE", 
				"INVERSE COVARIANCE MATRIX OF ESTIMATE", "ERRORS")
		if( any(names(li) %in% headers)) for( tit in headers ){
				
				nli <- names(li)
				idx <- which( nli == tit )
				if(!length(idx)) next
				current <- idx + 1
				while( current <= length(nli) && is.na(nli[current]) ){
					toappend <- li[[current]]
					plus <- grep( "^\\+", toappend)
					if( length(plus) ){
						toappend <- toappend[-c(1:(plus[1]-2)) ]
					}
					
					li[[idx]] <- c( li[[idx]], toappend )
					li[[current]] <- NULL
					nli <- names(li)
				}
				
			}
	}     
	if( sep == "2" ) {
		if( !keep.all ) {
			# only keep the first subproblem
			newli <- li[[1]]
			if( length(li) > 1 ) newli <- append( newli, li[[2]][-1] )
			newli
		} else {
			header <- li[[1]]
			newli <- lapply( li[-1], append, header, after = 0 )      
			newli
		}
	} else if( length(li) > 1 ) li else li[[1]] 
}

nmTitle <- function( txt ){
	if( is.list(txt)  ) return( list( title = NULL ) )
	
	if(any(regexMatches( txt, rx = "\\*{15,}" )))
	{
		titleText <- grep("\\*{15,}", txt[1:5], value = TRUE )
		titleText <- gsub( "\\*+", "", titleText )
		titleText <- stripBlanks( titleText, remove = TRUE)
		txt <- txt[-(1:5)]
	} 
	else 
	{
		if(any( regexMatches(txt = txt, "THERE ARE ERROR")) )
			titleText <- "ERRORS" 
		else return( list(title= NULL))
	}
	
	list( title = titleText, text = txt)
}


#' Obtains the NONMEM version information from a list file
#' @param lstContents [C,+] Contents of the lst file
#' @return A list with two elements: the version and the level
#' @author fgochez

nmVersion <- function(lstContents)
{
	# find the line where the version information is held
	line <- grep( "NONLINEAR MIXED EFFECTS MODEL PROGRAM", lstContents, fixed = TRUE, value = TRUE )
	
	if( length(line) == 0)
		RNMImportStop("Could not find any version information in the contents of the control file")
	# extract VERSION=
	nmVersion <- equalExpressionPop( line, "VERSION", sep = "[[:space:]]*" )
	nmLevel   <- equalExpressionPop( line, "LEVEL", sep = "[[:space:]]*")
	c( Version = nmVersion, Level = nmLevel)
}


#' 
#' @param contents A character vector of strings holding the lines of subproblem information (ONLY!) for one simulation problem
#' @title
#' @return 
#' @author fgochez
#' @keywords

partitionLstSubproblems <- function(contents)
{
	# NB this handles the sub-problems of 1 problem only
	# look for lines of the form PROBLEM NO.:         1     SUBPROBLEM NO.:      1
	subprobStarts <- grep(contents,	pattern = "PROBLEM NO\\.\\: [[:blank:]]*[0-9][[:blank:]]*SUBPROBLEM NO\\.\\:[[:blank:]]*[0-9]" )
	msg <- length(subprobStarts) %pst% " subproblems found\n"
	logMessage(log = "lowLevelParse", msg)
	
	# now cut along the subproblem starts
	if(length(subprobStarts) == 1)
		return(list(contents))
	subprobEnds <- c(subprobStarts[-1] - 1, length(contents))
	lapply(seq_along(subprobStarts), function(i) contents[subprobStarts[i]:subprobEnds[i]])
}

#' Obtain the minimum value of the the objective function from the correct element of the list returned by sectionLst
#' @param item An element of the list returned by sectionLst
#' @return NULL if item is NULL, minimum value of the objective function otherwise
#' @author fgochez

# TODO: robustify this!

.nmMVOF <- function( item ){
	if( is.null(item)) NULL else as.numeric(gsub("[\\*[:space:]]+", "", item ))
}


#' Retrieves the NONMEM variable names in a set of strings
#' @param txt vector of strings
#' @return A character vector of the NONMEM variable names
#' @author fgochez

.nmVariableNames <- function( txt )
{
	varRx <- "([[:alpha:]]+)[[:space:]]([[:digit:]]+)"
	txt <- gsub( varRx, "\\1\\2", txt )
	txt <- unique( unlist( strsplit( txt, "[\\|[:space:]]" ) ))
	txt <- txt[txt!=""]
	txt
}

#' TODO: implement
#' @param lstContents [C,+] character vector of lines of an "lst" file 
#' @return A list of character vectors that has the list contents split up by problems
#' @author fgochez

partitionLstbyProblems <- function(lstContents)
{
	NULL
}