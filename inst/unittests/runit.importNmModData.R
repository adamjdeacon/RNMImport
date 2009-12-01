# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################

# tests .importNmModData

test.importNmModData <- function()
{
	DATACOLNAMES <- c("File", "IG", "ACCEPT", "REWIND", "RECORDS", "TRANSLATE", "NULL")
	
	# multiple IGNORE= statements (issue )
	
	dataStatement1 <- "$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)"
	dataTest1 <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)")
	dataExpected1 <- matrix(c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",     
			REWIND="TRUE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest1,dataExpected1, msg = " |Mutliple IGNORE= now handled" )
	
	dataStatement2 <- "$DATA data3.dat IGNORE=@"
	dataTest2 <- RNMImport:::.importNmModData(dataStatement2)
	dataExpected2 <- matrix(c( File="data3.dat", IG="@", ACCEPT="",     
					REWIND="TRUE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest2,dataExpected2, msg = " |IGNORE=@ correct" )
	
	
	
	
}