# $Rev$
# $LastChangedDate$

#' Imports the input data used in an individual basic NONMEM model based on its $INPUT
#' and $DATA statements
#' @title Import NONMEM input data
#' @param dataStatement [char matrix] - A data statement, as parsed by .importNmModData
#' @param inputStatement [char matrix] - A 2 column matrix describing the $INPUT statement, see .importNmModInput
#' @param dropCols 
#' @param trim Currently unused
#' @param path path where the data files are located
#' @return  A data.frame with the data inside the file
#' @author fgochez

importModelData <- function(
		dataStatement, inputStatement, dropCols = TRUE, trim=FALSE,	path = NULL)
{	
	# TODO: need to handle the case where inputs are read from the previous problem's output 
	
	
	fileName <- dataStatement[,"File"]	
	stopifnot(!is.null(fileName))
		
	fileName <- .getFile(fileName, path = path)
	
	if( !.canOpen( fileName )) 
		RNMImportStop(paste("Unable to open requested data file ", fileName , "\n") )
	 
		# now extract the options from the data statement.  These will be passed

	ignore <- dataStatement[,"IG"]
	accept <- dataStatement[,"ACCEPT"]
	translate <- dataStatement[,"TRANSLATE"]
	records <- dataStatement[,"RECORDS"]
	
	# split out the ignore statement, as individuals tokens (or chunks of code) are seperated by ";"
	ignoreTokens <- unlist(strsplit(ignore, split = ";"))
	# replace "NONE" with "#"
	ignoreTokens <- ifelse(ignoreTokens == "NONE", "#", ignoreTokens)
	
	ignoreCodes <- ignoreTokens[which(nchar(ignoreTokens) > 1)]
	ignoreChars <- ignoreTokens[which(nchar(ignoreTokens) == 1)]
	if(length(ignoreChars) == 0) ignoreChars <- ""
	
	# Call readNmData, which is the workhorse function that actually reads the table

	myData <- readNmData(file = fileName, ignore = ignoreChars, accept = accept, 
			translate = translate, records = records)
		
	# Deal with the case of additional columns in the dataset
	if(nrow(inputStatement) == (ncol(myData) - 1) && all(is.na(myData[, ncol(myData)]))) 
		myData <- myData[,  - length(myData), drop = F]

	# now determine which columns should be dropped in case dropCols = TRUE
	if(dropCols) 
		colsToKeep <- inputStatement[, 1] != "DROP" & inputStatement[, 2] != "DROP" 
	else 
		colsToKeep <- rep(TRUE, nrow(inputStatement))
	myData <- myData[, colsToKeep, drop = FALSE]
	# Calculate columns names
	cNames <- ifelse(inputStatement[, 1] == "DROP", inputStatement[, 2], inputStatement[, 1])[colsToKeep]
	# add periods to column names to avoid duplicates

	while(any( dup <- duplicated(cNames))){
		cNames <- replace(cNames, dup, paste(cNames[dup], ".", sep = ""))
	}
	nDiff <- ncol(myData) - length(cNames)
	# handle columns not present in the $INPUT statement but present in the data file
	if(nDiff != 0)
		RNMImportWarning(paste("\nWarning: Number of columns in datafile (", ncol(myData), ") does not equal number of columns in $INPUT statement (", length(cNames), ")\n", sep = ""))
	if(nDiff > 0)
		cNames <- c(cNames, paste("ExtraCol", 1:nDiff, sep = ""))
	if(nDiff < 0)
		cNames <- cNames[1:ncol(myData)]
	dimnames(myData) <- list(dimnames(myData)[[1]], cNames)
	for(ignoreCode in ignoreCodes)
		.readNmData.nmSubset(data = myData, nmCode = ignoreCode, method = "ignore")
	### Check numeric and missing values
	myData <- .importDataNumeric(myData, missToZero = FALSE)
	
	### Use .deriveNmColumns
	if(!trim)
		myData <- .deriveNmColumns(myData)
	
	myData
	
}