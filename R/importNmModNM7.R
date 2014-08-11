#
# $LastChangedDate: 2009-12-01 21:02:43 +0000 (Tue, 01 Dec 2009) $
# $LastChangedBy: fgochez $
# $Rev: 13729 $
#
# Author: fgochez
###############################################################################



.importNmModSingleProblemNM7 <- function(contents, fileName)
{

	# retrieve a version of the text without comments
	poppedTxt <- commentPop(contents, inPlace = FALSE)$txt
	prob <- list()	
	# find the existent sections in the current problem
	titles <- sectionTitles(poppedTxt)	
	
	if("PRI" %in% titles)
	{	
		logMessage(logName = "lowLevelParse", "$PRIOR found \n")
		prob$Prior <- .importNmModPrior( contents )
		if(attr(prob$Prior, "NWPRI") == TRUE)
			RNMImportStop("$PRIOR NWPRI statement detected.  Importing of this is not supported, so will halt.\n", match.call())
		
		# handle case with priors here, else ignore
	}
	
	# deal with parameters
	
	prob$Theta <- if( "THE" %in% titles | "THT" %in% titles ) .importNmModTheta( contents )
	prob$Omega <- if("OME" %in% titles ) .importNmModOmega( contents, component = "OMEGA")
	prob$Sigma <- if( "SIG" %in% titles ) .importNmModOmega( contents, component = "SIGMA" )
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
	# TODO: the fact that fileName was not passed down here was not generating any problems.  Why?
	prob$Data <- if( "DAT" %in% titles ) .importNmModData( poppedTxt, fileName )
	prob$Sim <- if("SIM" %in% titles ) .importNmModSim( poppedTxt )
	
	
	# From now on, simply extract raw text for the other sections
	
	### extract the PK model                                                      
	prob$PK <- section( poppedTxt, "PK", "", as.list = FALSE, stripout = TRUE)
	
	### extract the PRED model                                                    
	prob$PRED <- section(poppedTxt, "PRED", "", as.list = FALSE, stripout = TRUE)
	
	### extract the Model                                                         
	prob$Model <- section( poppedTxt, "MOD", "", as.list = FALSE, stripout = TRUE)
	
	### extract the Error statements                                              
	prob$Error <- section( poppedTxt, "ERR", "", as.list = FALSE, stripout = TRUE)
	
	### extract the Mix statements                                                
	prob$Mix <- section( poppedTxt, "MIX", "", as.list = FALSE,
			stripout = TRUE, clean = TRUE)
	
	### extract the EST statements                                                
	
	if("EST" %in% titles) prob$Estimates <- .importNmModEst(poppedTxt)
	
	### extract the COV statements                                                
	prob$Cov <- section( poppedTxt, "COV", "", glue = TRUE,  
			as.list = FALSE, stripout = TRUE, clean = TRUE)
	prob
}