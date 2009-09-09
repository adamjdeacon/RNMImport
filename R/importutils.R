

#' Reads the lines of a file and removes whitespace in the process
#' @param file name of the file to read
#' @param empty.rx regular expression to eliminate from strings read in 
#' @title Read file contents and strip whitespace
#' @return Character vectors of lines of file contents
#' @author fgochez
#' @keywords IO

# Original author: R. Francois

scanFile <- function( file, empty.rx = "^[[:space:]]*$" )
{	
	# check if the file is NULL or can't be opened
	if( is.null(file) || !.canOpen(file) ) 
	{
		RNMImportWarning(paste(file, " is not valid, returning NULL"))	
		return(NULL)
	}
	contents <- scan( file, sep = "\n", what = "character", 
			blank = TRUE, multi = FALSE, quiet = TRUE)
	# remove whitespace in the file content and return
	contents <- negGrep( empty.rx , contents, value = TRUE )
	if( length(contents) > 0) contents else NULL
}

#' Checks whether or not a file can be opened
#' @param path.to.file Full path to the file
#' @title Check if file can be opened
#' @return logical 
#' @note Originally by John James
#' @author fgochez
#' @keywords IO

.canOpen <- function(path.to.file)
{
	
	if(!file.exists(path.to.file))
		return(FALSE)
	info <- file.info(path.to.file)
	
	### directory
	if(info[1,2])
		return(FALSE)
	
	### if zero size assume NOK
	if(info[1,1]==0)
		return(FALSE)
	TRUE
}

#' Retrieves the full path containing some file specified in a relative
#' path. Simply wraps file_path_as_absolute
#' @param x Path to the file 
#' @return String containing absolute path
#' @author fgochez

dirname.abs <- function(x){
	dirname( tools:::file_path_as_absolute(x) )
}

#' Checks whether or not a string is a full path name
#' @param x String to check
#' @title Check if string is full path
#' @return logical, TRUE if x is a path
#' @author fgochez
#' @keywords utils

.isfullpath <- function( x ){
	# x %~% "^(/|\\\\|[[:alpha:]]:)"
	regexMatches(x, "^(/|\\\\|[[:alpha:]]:)" )
}

# ??
#' @param file 
#' @param path 
#' @title
#' @return 
#' @author fgochez
#' @keywords

.isinpath <- function( file, path){
	nchar( file ) > nchar( path ) && substring( file, 1, nchar(path) ) == path
}

#' Takes a file name, a path, and correctly appends them to form a full relative path to the file
#' @param file Filename (possibly including some path data) 
#' @param path A path
#' @param test (?)
#' @param warn (?)
#' @param stop (?)
#' @param remove (?)
#' @return A full relative path containing the file name
#' @author fgochez

# Original author: R. Francois
.getFile <- function( 
		file, 
		path = NULL,
		test = FALSE,
		warn = TRUE, 
		stop = FALSE, 
		remove = TRUE
){
	file <- as.character( file )
	# if file has commas, take it to be a comma-seperated list
	if(any(regexMatches(file, ",")))
		file <- CSLtoVector(file)
	
	## rx to identify if this is a full path file
	if( !is.null(path) ) {
		for( i in seq(along=file)){
			file[i] <- if(.isfullpath(file[i]) || .isinpath(file[i], path)) file[i] else file.path( path, file[i])
		}
	}
	if(test) test <- file.exists( file )
	if( any(test) ){
		if(stop) RNMImportStop(paste("missing files", file[test], sep = "\n\t\t"))
		if(warn) logMessage("Warning: Cannot open file", file[test], "warnings")
		if(remove) file <- file[!test]
	}
	if( !length(file) ) file <- NULL
	file
}

#' "Processes" a path, so that if it is enclosed in brackets, it is taken to be the name of a path
#' to be retrieved by "getNmPath".  Otherwise, just returns the path
#' @param path the path to process.
#' @return Either path unmodified, or path retrieved via getNmPath
#' @author fgochez

processPath <- function(path)
{
	if(any(regexMatches(rx = "\\" %pst% "(", path)) & any(regexMatches(rx = "\\" %pst% ")", path)))
		return(getNmPath( bracketPop(path, inPlace = FALSE)$op.out ))
	path
	
}

#' Checks if a file has a given extentions
#' @param fileName Name of the file to check
#' @param extension Extension of the file to check.  Can have more than one
#' @return whether or not the filename has the given extension 
#' @author fgochez

hasExtension <- function(fileName, extensions)
{
	y <- sapply(casefold(extensions), 
			function(x) { regexMatches(txt = casefold(fileName), rx = paste( "\\.", x, "$", sep = "")) } )
	any(y)
}