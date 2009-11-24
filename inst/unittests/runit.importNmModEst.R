# $Rev$
# $LastChangedDate$
# $LastChangedBy$
# Author: fgochez
###############################################################################

# tests .importNmModEst function

test.importNmModEst <- function()
{
	estText <- c(
	"$EST METHOD=ITS INTERACTION FILE=wexample7.EXT   NITER=100 PRINT=5 NOABORT SIGL=8 CTYPE=3 CITER=10",
	"NOPRIOR=1 CALPHA=0.05 NSIG=2",
	"$EST METHOD=SAEM INTERACTION NBURN=200 NITER=300 SIGL=8 ISAMPLE=2 PRINT=10 SEED=1556678 CTYPE=3",
	"CITER=10 CALPHA=0.05 NOPRIOR=1", 
	"ISAMPLE=1 ISAMPLE_M1=1 ISAMPLE_M2=1 ISAMPLE_M3=1",
	"$EST METHOD=IMP  INTERACTION EONLY=1 NITER=2 ISAMPLE=300 PRINT=1 SIGL=10 NOPRIOR=1",
	"$EST METHOD=BAYES INTERACTION FILE=wexample7.txt NBURN=500 NITER=500 PRINT=100 CTYPE=3 CITER=10",
	"CALPHA=0.05 NOPRIOR=0",
	"$EST METHOD=COND INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 PRINT=5 NOABORT NOPRIOR=1",
	"FILE=wexample7.EXT" )

	estResult <- RNMImport:::.importNmModEst(estText)
	
	# check that 5 seperate lines of EST are extracted
	
	checkEquals(length(estResult), 5, " correct number of $EST statements obtained" )
	
	estTextSingleLines <- c( 
		"METHOD=ITS INTERACTION FILE=wexample7.EXT   NITER=100 PRINT=5 NOABORT SIGL=8 CTYPE=3 CITER=10 NOPRIOR=1 CALPHA=0.05 NSIG=2",
		"METHOD=SAEM INTERACTION NBURN=200 NITER=300 SIGL=8 ISAMPLE=2 PRINT=10 SEED=1556678 CTYPE=3 CITER=10 CALPHA=0.05 NOPRIOR=1 ISAMPLE=1 ISAMPLE_M1=1 ISAMPLE_M2=1 ISAMPLE_M3=1",
		"METHOD=IMP  INTERACTION EONLY=1 NITER=2 ISAMPLE=300 PRINT=1 SIGL=10 NOPRIOR=1",
		"METHOD=BAYES INTERACTION FILE=wexample7.txt NBURN=500 NITER=500 PRINT=100 CTYPE=3 CITER=10",
		"METHOD=COND INTERACTION MAXEVAL=9999 NSIG=3 SIGL=10 PRINT=5 NOABORT NOPRIOR=1"
		)
	# check that the EST statement is parsed into multiple lines of text
	checkEquals( unlist(estResult), estTextSingleLines, msg = " EST statement parsed into multiple lines of text" )
}

