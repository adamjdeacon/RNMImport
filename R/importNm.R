# $Rev$
# $LastChangedDate$


#' Imports a NONMEM run based on a control file and an output report file.  This function assumes that 
#' all of the input and output data tables are in the same directory as the control file, though the input
#' tables are allowed to be missing.
#' @title Import a NONMEM run
#' @param conFile [C,1] control file name
#' @param reportFile [C,1] The name of the report file.  In the future this parameter will be optional, 
#' in which case importNm will attempt' to deduce the file name on its own based on the name of the conFile
#' and the list file extensions stored in the metadata.
#' @param path The path where the files are located.  Can be the name of a path stored with setNmPath if it
#' is surrounded by round brackets
#' @param dropInputColumns Logical flag.  If TRUE, those columns of the input data flagged for dropping in the 
#' $INPUT statement will not be imported with the run
#' @return An NMRun object that holds the information of the NONMEM run
#' @author fgochez, rweeks
#' @keywords IO
#' @export

importNm <- function(conFile, reportFile = NULL, path = NULL, dropInputColumns = TRUE)
{
	logMessage(logName = "detailedReport", paste("Importing control file", conFile, "\n"))
	#deal with the 	"path" parameter
	if(!is.null(path))
		path <- processPath(path[1])
	
	# get the FULL paths to both files and store them
	fullConFilePath <- tools:::file_path_as_absolute(.getFile(conFile, path))
	conFile <- basename(conFile)
	
	if(!hasExtension(conFile, getNmFileExtensions("control")))
		RNMImportStop("Invalid control file extension for " %pst% conFile, call = match.call())
	
	# if the reportFile was not provided, we will attempt to deduce it.
	if(is.null(reportFile))
	{
		path <- dirname(fullConFilePath)
		conFileVector <- strsplit(conFile, split = "\\.")[[1]]
		allListFiles <- list.files(path)
		whichFile <- sapply(strsplit(allListFiles, split = "\\."), function(x, y)
							{
								#Condition 1 to test the correct file extension
								conOne <- x[length(x)] %in% getNmFileExtensions("report")
								#Condition 2 to test the correct names (multiple .s allowed)
								conTwo <- ifelse(length(x) == length(y), all(x[-length(x)] == y[-length(x)]), FALSE)
								conOne && conTwo
 							}, y = conFileVector)
		reportFile <- allListFiles[whichFile]
		if(!length(reportFile))
			RNMImportStop(msg = "No report file in the directory!")
		if(length(reportFile) > 1)
		{
			RNMImportWarning(msg = "More than one report file. Using the first.")
			reportFile <- reportFile[1]
		}		
	}else{
		
		reportFile <- basename(reportFile)
		path <- dirname(fullConFilePath)
	}
	fullLstFilePath <- tools:::file_path_as_absolute(.getFile(reportFile, path))
	# read the control file contents
	controlContents <- importNmMod(conFile,  path = path)
	# read in the list file contents.  Note that they should only be omitted in the case of a single SIMONLY run
	reportContents <- importNmReport(reportFile, path = path, controlContents)
	probResults <- reportContents$problemResults
	problems <- controlContents$problemContents
	numProblems <- length(problems)
	modelList <- vector(mode = "list", length = numProblems)
		
	# iterate through the problems
	for(i in 1:numProblems)
	{
		controlStatements <- problems[[i]]
		reportStatements <- probResults[[i]]
		# check if there is a simulation statement.  If so, proceed accordingly
		if(!is.null(controlStatements$Sim))
		{			
			# there is a simulation statement, so check if it is a "simulation only" run, or a simulation+model fitting run
			isSimOnly <- controlStatements$Sim["simOnly"] == "TRUE"
			if(isSimOnly)
				modelList[[i]] <- NMSimDataGen(controlStatements, path, NULL)
			else # this is a simulation+fitting problem
				modelList[[i]] <- NMSimModel(controlStatements, path, reportStatements)
		} # end !is.null(controlStatements$Sim)
		else
		{			 			
			modelList[[i]] <- NMBasicModel(controlStatements, path, reportStatements, 
					dropInputColumns = dropInputColumns)
		}
		# set the subset for graphing - note that this should probably be dropped in the future
		# and replaced with the data subset only
	
		graphSubset(modelList[[i]]) <- defaultDataSubset()
		dataSubset(modelList[[i]]) <- defaultDataSubset()
	} # end for(i in 1:numProblems)
	# retrieve basic information on the control and report files, e.g. date of modification, size, etc.
	fileInfo <- file.info(fullConFilePath, fullLstFilePath)
	# obviously these are not directories!
	fileInfo$isdir <- NULL
	fileInfo$fileName <- rownames(fileInfo)
	rownames(fileInfo) <- NULL
	new("NMRun", controlText = controlContents$Raw, 
			controlComments = if(is.null(controlContents$Comments)) character(0) else controlContents$Comments, 
			reportFileInfo = fileInfo[2,],
			controlFileInfo= fileInfo[1,], 
			numProblems = numProblems, problems = modelList,
			reportText = reportContents$Raw)
}
