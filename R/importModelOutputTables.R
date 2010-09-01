# $Rev$
# $LastChangedDate$

# TODO: Implement dvLog
# TODO: handle situation where there are more columns in the table file than in the $TABLE statement
###############################################################################

#' Imports the output data used in an individual NONMEM model based on its $TABLE statements 
#' @title Import model tables
#' @param tableStatement tableStatement [char matrix] - A control file table statement, as parsed by .importNmModTables 
#' @param allowFirstOnly [L,1] - Allow the use of FIRSTONLY statements in the $TABLE statement? 
#' @param dvLog currently unused
#' @param trim If TRUE, will _not_ add additional variables such as absWRES to the returned data
#' @param returnFormat How to return data. If "DFList", a list of data.frames, with one for each table file produced.
#' Otherwise, attempts to consolidate all of the (unique) output table variables into a single data.frame, but will return
#' a list of some of the tables had a FIRSTONLY statement, and others did not.
#' @param path Path to the table files.  Can be a path name 
#' @return Returns: Either a list or a data.frame.  A data.frame of all unique output table columns (from all table files)
#' is returned if returnFormat = "singleDF", UNLESS there are both FIRSTONLY tables and non-FIRSTONLY tables, in which
#' case a list of 2 components is returned. 
#' @author fgochez
#' @keywords

importModelOutputTables <- function(
		tableStatement,	allowFirstOnly = TRUE, dvLog = FALSE, trim = FALSE,
		returnFormat = c("singleDF", "DFlist"),	path = NULL
)
{	
	NUMEXPECTEDCOLUMNS <- 5
	FILEFIELD <- "File"
	FIRSTONLYFIELD <- "firstOnly"
	
	APPENDEDCOLUMNS <- c("DV", "PRED", "RES", "WRES") 
	
	logMessage(logName = "detailedReport", "Importing output tables\n")
	returnFormat <- match.arg(returnFormat)
	
	numStatements <- nrow(tableStatement)
	tableList <- vector(mode = "list", length = numStatements)
	
	allColNames <- character(0)
	for(i in 1:numStatements)
	{
		currentTable <- try(readNmData(file = .getFile(tableStatement[i, FILEFIELD], path = path)), 
				silent = TRUE)
		
		# try to read table file, emitting a warning if it fails and continuing to next
		if(inherits(currentTable, "try-error"))
		{
			msg <- paste("Unable to read table file", tableStatement[i, FILEFIELD], "due to error", currentTable, "\n") 
			RNMImportWarning(msg, call = match.call())
			tableList[[i]] <- NA
			next
		} 
		# force to numeric
		currentTable <- .importDataNumeric(currentTable, missToZero = FALSE)
		
		colNames <- CSLtoVector(tableStatement[i,"Columns"])
		# if APPEND is TRUE, then we need to extract the column names from "appendedColumns", and then append them back to the end.
#				# this is necessary because if APPEND is used (which it is by default), NONMEM appears to ignore the presence of DV, WRES, etc. in the
#				# the table statement, and simply adds them to the end of the table on its own regardless of what order they appear in the TABLE statement
		# if APPEND is true, then we need to ext
		if(tableStatement[i, "append"])
		{
			# remove all columns ,but "DV" may be repeated
			colNames <- setdiff(colNames, APPENDEDCOLUMNS)
			colNames <- c(colNames, APPENDEDCOLUMNS)
		}
		newColNames <- setdiff(colNames, allColNames)
		
		
		if(length(newColNames))
		{
			if(length(colNames)> length(colnames(currentTable))){
				currentTable <- 
						cbind(currentTable, 
								data.frame(matrix(NA,
												nrow=dim(currentTable)[1],
												ncol=(length(colNames)-length(colnames(currentTable))))))
			}
			colnames(currentTable)[1:length(colNames)] <- colNames
			
			# use only the non-repeated columns
			currentTable <- currentTable[,newColNames, drop = FALSE]
			# assign as many names from the table statement to currentTable's columns as possible	
			allColNames <- c(newColNames, allColNames)
		}		
		# Now handle FIRSTONLY statement if it is present.  We take unique values of the ID by default		
		# TODO: Make this logic more robust
		if(allowFirstOnly & tableStatement[i, FIRSTONLYFIELD])
		{
			logMessage("Firstonly flag found, subsetting rows", "detailedReport")
			attr(currentTable, FIRSTONLYFIELD) <- TRUE
			
		}
		
		else if(!allowFirstOnly & tableStatement[i, FIRSTONLYFIELD])
			RNMImportStop("FIRSTONLY table detected, yet allowFirstOnly is set to FALSE", match.call() )
		else
		{
			# set an attribute that controls whether or not the table was read via a "FIRSTONLY" statement
			attr(currentTable, FIRSTONLYFIELD) <- FALSE
		}
		tableList[[i]] <- currentTable
	}
	if(returnFormat == "DFList")
		return(tableList)
	else
	{
		if(is.na(tableList[[1]])){
			return(NULL)
			
		} else {
			
			# determine the "FIRSTONLY" tables, as these cannot be bound together with the other ones due to the size difference
			tableStyles <- sapply(tableList, function(x) attr(x, FIRSTONLYFIELD))
			normalTables <- tableList[!tableStyles]
			firstOnlyTables <- tableList[tableStyles]
			
			consolidatedTable <- do.call(cbind, normalTables)
			if(!trim)
				consolidatedTable <- .deriveNmColumns(consolidatedTable)
			# Check if there are both FIRSTONLY and non-FIRSTONLY tables
			if((sum(tableStyles) * sum(!tableStyles) > 0))
			{
				RNMImportWarning("Found tables of both FIRSTONLY and NON-FIRSTONLY type, returning a list")
				return(list("normal.tables" = consolidatedTable, "firstonly.tables" = do.call(cbind, firstOnlyTables)))
			}
			else
				return(consolidatedTable)
		}
	}
}