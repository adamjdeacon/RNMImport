
#' Parses a NONMEM control file, and returns its contents as a list of parsed elements.  These elements
#' will correspond to the actual control statements, e.g. $PK, $THETA, $PROBLEM, etc.  Some of the elements
#' are kept as pure text.
#' @title Parses a NONMEM control file
#' @param fileName Name of the control file
#' @param path (optional) path to the control file, can be a stored path enclosed in round brackets
#' @title Import a control file
#' @return A list describing the control file contents
#' @author Francisco Gochez <fgochez@mango-solutions.com>
#' @keywords IO

importNmMod <- function(
		fileName = NULL, path = NULL)
{	
	# log messages
	logMessage(logName = "stdReport", paste("Importing file", fileName, "\n"))
	
	path <- processPath(path)
	### import the file or just read the character vector on textFile             
	fileContents <- scanFile(.getFile(fileName, path))
	# check if the file contents were returned as NULL, and return NULL
	
	if( is.null(fileContents) ) 	
		RNMImportStop(paste("Contents of the file", fileName, "are empty \n"), match.call())
	
	# remove lines that only contains spaces and semicolons
	fileContents <- negGrep( "^[;[:space:]]+$", fileContents, value = TRUE)
	
	#Strip out the whitespace from the beginning of each line.  This avoids many potential problems
	fileContents <- killRegex(fileContents, "^[[:blank:]]*") 
	
	# remove comments.  Currently they are not used anywhere, but might facilitate certain operations
	comments <-  commentPop(fileContents, inPlace = FALSE)$op.out
	
	# split the text by problem		
	problemTexts <- partitionByProblem(fileContents)
	
	numProblems <- length(problemTexts)
	
	msg <- paste(fileName, "has", numProblems, "problems")
	logMessage(logName = "highLevelParse", msg)
	
	# must have at least one problem
	if( numProblems == 0){
		RNMImportStop( paste("Cannot find $PROBLEM statement in control file", fileName), match.call() ) 
	}
		
	
	### create output list                                                        
	outList <- list(Raw = fileContents, Comments = comments)
	
	### Now we begin iteratively importing each problem	

	problemContents <- vector(mode = "list", length = numProblems)
	
	for(i in seq_along(problemTexts))
	{
		# retrieve a version of the text without comments
		poppedTxt <- commentPop(problemTexts[[i]], inPlace = FALSE)$txt
		prob <- list()	
		# find the existent sections in the current problem
		titles <- sectionTitles(poppedTxt)	
		# deal with THETAs
		
		prob$Theta <- if( "THE" %in% titles | "THT" %in% titles ) .importNmModTheta( problemTexts[[i]] )
		prob$Omega <- if("OME" %in% titles ) .importNmModOmega( problemTexts[[i]], component = "OMEGA")
		prob$Sigma <- if( "SIG" %in% titles ) .importNmModOmega( problemTexts[[i]], component = "SIGMA" )
	
		# extract any raw FORTRAN code
		prob$Script <- fortranPop(poppedTxt, inPlace = TRUE)
		
		# now extract the $PROB statement   
		prob$Problem <-  section(poppedTxt, "PRO", "", strip = TRUE, as.list = FALSE, glue = TRUE, clean = TRUE)
		# extract $TABLE
		prob$Tables <- if( "TAB" %in% titles ) .importNmModTables( poppedTxt) 
		# extract $SUB
		prob$Subroutine <- if( "SUB" %in% titles ) .importNmModSub( poppedTxt)
		# $INPUT statement
		prob$Input <- if( "INP" %in% titles ) .importNmModInput( poppedTxt )
		prob$Data <- if( "DAT" %in% titles ) .importNmModData( poppedTxt, fileName )
		prob$Sim <- if("SIM" %in% titles ) .importNmModSim( poppedTxt )
		
		# From now on, simply extract raw text for the other sections
		
		### extract the PK model                                                      
		prob$PK <- section( poppedTxt, "PK", "", as.list = FALSE, strip = TRUE)
		
		### extract the PRED model                                                    
		prob$PRED <- section(poppedTxt, "PRED", "", as.list = FALSE, strip = TRUE)
		
		### extract the Model                                                         
		prob$Model <- section( poppedTxt, "MOD", "", as.list = FALSE, strip = TRUE)
		
		### extract the Error statements                                              
		prob$Error <- section( poppedTxt, "ERR", "", as.list = FALSE, strip = TRUE)
		
		### extract the Mix statements                                                
		prob$Mix <- section( poppedTxt, "MIX", "", as.list = FALSE,
				strip = TRUE, clean = TRUE)
		
		### extract the EST statements                                                
		prob$Estimates <- section( poppedTxt, "EST", "", glue = TRUE, 
				as.list = FALSE, strip = TRUE, clean = TRUE)
		
		### extract the COV statements                                                
		prob$Cov <- section( poppedTxt, "COV", "", glue = TRUE,  
				as.list = FALSE, strip = TRUE, clean = TRUE)
		problemContents[[i]] <- prob
	}
	outList$controlFile <- .getFile(fileName, path)
	outList$problemContents <- problemContents 
	outList 
	
	
}

