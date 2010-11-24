# $Rev$
# $LastChangedDate$


#' Read NONMEM dataset removing any "IGNORE" rows and dealing with (possibly repeated) headers 
#' This function is designed to import either output table or raw input data
#' The ignore string can optionally be added to remove rows from the data
#' Any table file headers found are removed from the data - repeated headers are also removed
#' @param file [C,1] - Name of the file
#' @param ignore [C,1] - ignore option, see NM?$DATA[IGNORE]
#' @param accept [C,1] - accept option, see NM?$DATA[ACCEPT]
#' @param translate [C,1] - translate option, see NM?$DATA[TRANSLATE]
#' @param records [CN,1] - number of records, see NM?$DATA[RECORDS]
#' @param null [C,1] - value to replace NA, see NM?$DATA[NULL]
#' @param sep character that seperates fields
#' @return A data.frame of the contents of the tables in the file
#' @author Mango Solutions

# individual author: F. Gochez, based on code by R.Francois and R.Pugh 
# this doesn't cope with a simgle mutilple iteration output simulation table file JJ

readNmData <- function(
		file, ignore = NULL, accept = NULL, translate = NULL,   
		records = NULL, null = NA, sep = NULL, 
		startHandlingSims=.parameters$startHandlingSims
)
{
	
	if( is.character(ignore) && ignore == "" ) ignore <- NULL
	if( is.character(accept) && accept == "" ) accept <- NULL
	if( is.character(translate) && translate == "" ) translate <- NULL
	CALL <- match.call()
	### See how big is this file
	sizeofFile <- file.info(file)[,'size']
	actualFile <- NULL
#	& (regexpr('sim.tab',basename(file))>0)
	if((sizeofFile > startHandlingSims) ){
		cat("going to make the assumption", basename(file), "is a regular file and generated in simulations\n")
		if(length(ignore) > 1) RNMImportStop("readNmData cannot handle IGNORE tokens in files with more than 50000 rows", match.call())
		actualFile <- file
#		now copy 50000 rows into a new file
		file <- tempfile()
		on.exit(unlink(file))
		cat(readLines(actualFile, n=50000), sep='\n', file=file)
		if(missing(sep) || length(sep) > 1){ # then have a guess
			# possSeps <- c("", ",") 
			possSeps <- if( missing(sep)) c("", ",") else sep # see ?read.table for how "" is used
			countFields <- lapply(possSeps, count.fields, file = file)
			maxFields <- sapply(countFields, median)
			whichSep <- (1:length(possSeps))[maxFields == max(maxFields)][1]
			sep <- possSeps[whichSep]
			### find the most appropriate separator
		}
		gc(gcinfo(TRUE))
		someScan <- 
				tryCatch(scanFile( file ), 
						warning=function(e){
							return(simpleError(e), CALL)
						},
						error=function(e){
							return(simpleError(e), CALL)
						}
				)
		tabRows <- grep("TABLE NO\\.", someScan)
		# To define header row, look for proportion of letters in the row
		gsub1 <- gsub("[^[:alpha:]]+", "", someScan)
		gsub2 <- gsub( "[[:space:]]+", "", someScan)
		propScan <- nchar(gsub1) / nchar(gsub2 )
		if( length(tabRows) ) propScan[tabRows] <- 0
		if(any(propScan > 0.5)){
			theHeadRow  <- which.max(propScan)
		} else theHeadRow <- NULL 
		if(!is.null(theHeadRow)) {
			headRows <- which( someScan == someScan[theHeadRow] )
			headRows <- setdiff(headRows, theHeadRow)
		}
		###  Work out rows to skip
		colHeaders <- NULL
		skipRows <- sort(unique(c(headRows, tabRows, theHeadRow)))
#		print(someScan[skipRows])
		#	count the number of columns in remaining rows
		if(length(skipRows)>0){
			itemsPerLine <-sapply(gregexpr('?,?', someScan[-skipRows]), function(X)sum(attr(X, 'match.length')))
		} else {
			itemsPerLine <-sapply(gregexpr('?,?', someScan), function(X)sum(attr(X, 'match.length')))
		}
		allScan <- 
				tryCatch(scanFile( actualFile), 
						warning=function(e){
							return(simpleError(e, CALL))
						},
						error=function(e){
							return(simpleError(e, CALL))
						}
				)
		period <- unique(diff(skipRows))
#		special case of just one simultation
		las <- length(allScan)
		if(length(period)!=2){
			if(period[1]==1){
				period[2] <- las - 1
			} else {
				return(simpleError('Unable to determine periodicity of simulation table', CALL))
			}
		}
		sims <- floor(las/sum(period))
		cat('found', sims, 'simulations\n')
		ss <- seq(from=period[1], length=sims, by=(period[2]+1))
#		skipRows <- sort(c(ss, ss+1))
		
		### apply the header as the names of the data
		if(length(theHeadRow) != 0 && theHeadRow != 0) {
			headLine <- allScan[theHeadRow]
			ignoreMod <- replace(ignore, ignore == "@", "[[:alpha:]@]")
			if(length(ignoreMod) > 0) {
				ignoreMod <- sprintf("^[[:space:]]*%s", ignoreMod)
				rx <- paste(ignoreMod, collapse = "|")
				if(length(grep(headLine, pattern = rx)) > 0)
					headLine <- ""
			}
			
			colHeaders <- .readValues( headLine, sep = sep, what = "character" )
		}
		
		for(skip in 1:length(ss)){
			if(floor(skip/20)*20==skip)
				cat(skip, 'files written\n')
			con <- file(file.path(tempdir(),paste(basename(actualFile), sprintf('%03d', skip), sep='_')),'w' )
			writeLines(allScan[(ss[skip]+2):(ss[skip]+period[2])], con=con, use=TRUE)
			close(con)
		}
		cat(skip, 'files written\n')
		myData <- list()
		for(skip in 1:length(ss)){
			if(floor(skip/20)*20==skip)
				cat(skip, 'files read\n')
			fileName <- file.path(tempdir(),paste(basename(actualFile), sprintf('%03d', skip), sep='_'))
			con <- file(fileName,'r' )
			myData[[skip]] <- try(read.table(file=con, nrows=floor(period[2]*.05)), silent = TRUE )
			close(con)
			#'iter'
			if(length(colHeaders) <= length(myData[[skip]]))  {
				names(myData[[skip]])[seq_along(colHeaders)] <- colHeaders
			}
			attr(myData[[skip]], 'fileName') <- fileName
		}
		cat(skip, 'files read\n')
		
#		length(myData)
#		dim(myData[[1]])
#		attr(myData[[99]],'fileName')
		ret <- do.call('rbind', myData)
		attr(ret, 'fileName') <- basename(actualFile)
		return(ret)
	}
	### find the most appropriate separator
	if(missing(sep) || length(sep) > 1){ # then have a guess
		# possSeps <- c("", ",") 
		possSeps <- if( missing(sep)) c("", ",") else sep # see ?read.table for how "" is used
		countFields <- lapply(possSeps, count.fields, file = file)
		maxFields <- sapply(countFields, median)
		whichSep <- (1:length(possSeps))[maxFields == max(maxFields)][1]
		sep <- possSeps[whichSep]
	}
	
	allScan <- 
			tryCatch(scanFile( file ), 
					warning=function(e){
						return(simpleError(e), CALL)
					},
					error=function(e){
						return(simpleError(e), CALL)
					}
			)
	### Initialise rows to skip:
	#     tabRows    - Rows with "TABLE NO." string in first 10 characters
	#     theHeadRow - The actual header row to use
	#     headRows   - Repeated header rows
	#     igRows     - Rows to ignore (as in, using IGNORE statements for raw data files)
	skipRows <- theHeadRow <- otherHeaderRows <- headRows <- igRows <- numeric(0)
	
	### Work out all the "table*" rows (which we want to drop)
	tabRows <- grep("TABLE NO\\.", allScan)
	
	### Look for table headers
	# To define header row, look for proportion of letters in the row
	gsub1 <- gsub("[^[:alpha:]]+", "", allScan)
	gsub2 <- gsub( "[[:space:]]+", "", allScan )
	propScan <- nchar(gsub1) / nchar(gsub2 )
	if( length(tabRows) ) propScan[tabRows] <- 0
	if(any(propScan > 0.5)){
		theHeadRow  <- which.max(propScan)
	} else theHeadRow <- NULL 
	
	### If we have found a header row, look for other header rows
	if(!is.null(theHeadRow)) {
		headRows <- which( allScan == allScan[theHeadRow] )
		headRows <- setdiff(headRows, theHeadRow)
	}
	###  Work out rows to skip
	colHeaders <- NULL
	
	### Deal with "ignore" string          
	if(any(nchar(ignore) > 1)) RNMImportStop("readNmData cannot handle IGNORE tokens with more than one character", match.call())
	
	ignoreMod <- replace(ignore, ignore == "@", "[[:alpha:]@]")
	if(length(ignoreMod) > 0) {
		ignoreMod <- sprintf("^[[:space:]]*%s", ignoreMod)
		rx <- paste(ignoreMod, collapse = "|")
		igRows <- c(igRows, grep( rx, allScan )) 
	}
	
	### Need to work out which rows we want to omit here
	skipRows <- unique(c(igRows, headRows, tabRows, theHeadRow))
	#	count the number of columns in remaining rows
	
	if(length(skipRows)>0){
		itemsPerLine <-sapply(gregexpr('?,?', allScan[-skipRows]), function(X)sum(attr(X, 'match.length')))
	} else {
		itemsPerLine <-sapply(gregexpr('?,?', allScan), function(X)sum(attr(X, 'match.length')))
	}
	irregular <- integer(0)
	ul <- unique(itemsPerLine)
	if(length(ul)>1){
		RNMImportWarning(paste("readNmData: removing extra columns on lines in",file,'\n'), match.call())
		mul <- min(ul)
		irregular <- which(itemsPerLine!= mul)
		cat('trimming rows:', irregular, '\n')
		ir <- irregular[1]
		for (ir in irregular){
			line <- which(attr(gregexpr('?,?', allScan[-skipRows][ir])[[1]], 'match.length')>0)		
			allScan[-skipRows][ir] <- substring(allScan[-skipRows][ir],1, line[mul + 1] - 1)
		}
	}
	
	dataCall <- list( na.strings = ".", header = FALSE, sep = sep, comment.char = "")
	
	if(!length(skipRows)) {
		dataCall$file <- file
	} else {
		if( length(irregular)==0 & (all(diff(skipRows) == 1) & any(skipRows == 1)) ) { # rows to skip are at the beginning
			dataCall$file <- file
			dataCall$skip <- max(skipRows)
		} else {  # Need to do something with the scanned data - write to a file and retrieve
			tmpFile <- tempfile( )
			write(x=allScan[-skipRows], file=tmpFile)
			dataCall$file <- tmpFile
		}
	}
	myData <- try( do.call( read.table, dataCall ), silent = TRUE )
	try( unlink( tmpFile ), silent = TRUE )     
	
	### apply the header as the names of the data
	if(length(theHeadRow) != 0 && theHeadRow != 0) {
		headLine <- allScan[theHeadRow]
		ignoreMod <- replace(ignore, ignore == "@", "[[:alpha:]@]")
		if(length(ignoreMod > 0)) {
			ignoreMod <- sprintf("^[[:space:]]*%s", ignoreMod)
			rx <- paste(ignoreMod, collapse = "|")
			if(length(grep(headLine, pattern = rx)) > 0)
				headLine <- ""
		}
		
		colHeaders <- .readValues( headLine, sep = sep, what = "character" )
		if(length(colHeaders) <= length(myData))  {
			names(myData)[seq_along(colHeaders)] <- colHeaders
		}
	}
	
	### deal with the list accept declaration
	.readNmData.nmSubset( accept, myData, method = "accept")
	
	### deal with the translate delaration
	.readNmData.nmTranslate( translate, myData )
	
	### deal with the records option
	.readNmData.nmRecords( records, myData)
	return(myData)
}

#' convert lists usch as IGNORE or ACCEPT from a control file (see ?$DATA) into an R subset
#' @param nmCode Extract from "IGNORE" or "ACCEPT" statement to be used for subsetting 
#' @param data A data frame of NONMEM input and/or output data
#' @param method "ignore" or "accept"
#' @param na.keep Logical flag.  Keep data that is has NAs?
#' @param link Token that joins statements together 
#' @return Subsetted data as a data.frame
#' @author fgochez

# Based on code by R. Francois

.readNmData.nmSubset <- function( nmCode, data, 
		method = c("accept", "ignore"), na.keep = FALSE, 
		link = ".OR."  ){
	if( !is.null( nmCode) && any(regexMatches(nmCode, "^\\(")) ) 
	{
		#	nmCode <- convertFortran95Ops(nmCode)
		method <- match.arg( method )
		dname <- deparse(substitute(data))
		nmCode <- gsub( "," , ".OR.", nmCode)   # see ?$DATA
		# if the ignore or accept statement has NONMEM "code", we need to convert this to R subsetting code
		nmCode <- convertNmOperators( nmCode )  # .LT.   <, ...
		nmCode <- sprintf( "(%s)", nmCode )     
		if( method == "ignore" ) nmCode <- sprintf("!(%s)", nmCode)
		subs <- eval(parse(text = nmCode), data)
		subs <- ifelse( is.na(subs), na.keep, subs )
		assign( dname, data[ subs , , drop = FALSE], parent.frame() )
	}
}

### function to deal with the translate option in $DATA
.readNmData.nmTranslate <- function( nmCode, data )
{
	if(!is.null(nmCode) )
	{  
		#	nmCode <- convertFortran95Ops(nmCode)
		dname <- deparse(substitute(data))
		nmCode <- strsplit( nmCode, "," )[[1]]
		left  <- gsub( "/.*$","", nmCode  )       # after the first /
		right <- gsub( "^[^/]*/","", nmCode  )    # before the first /
		for( i in seq(along = nmCode) ){
			data[[ left[i] ]] <- data[[ left[i] ]] / as.numeric( right[i] ) 
		}
		assign( dname, data, parent.frame( ) )
	}  
}      

.readNmData.nmRecords <- function( records, data ){
	if( !is.null(records) && any(records != "") ){
		dname <- deparse(substitute(data))
		### first try to use the records as numeric values
		nrec <- try( as.numeric(records) , silent = TRUE)
		if( !inherits(nrec, "try-error") & !is.na(nrec)){
			assign( dname, head( data, nrec ), parent.frame( ) )
		} else {
			# in this case records refers to a column in the data, and we want to take the rows  whose 
			# records column matches the first element of that column AND appear in a contiguous block
			firstLevel <- data[[records]][1] 
			# use the rle function to find contiguous blocks
			x <- rle(data[[records]] == firstLevel)
			assign(dname, data[1:x$lengths[1],], parent.frame())
			
		}
	}
}