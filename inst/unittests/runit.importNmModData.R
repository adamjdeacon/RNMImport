# $LastChangedDate: 2009-12-16 15:14:40 +0000 (Wed, 16 Dec 2009) $
# $LastChangedBy: fgochez $
# $Rev: 14126 $
# 
# Author: fgochez
###############################################################################

# tests .importNmModData

test.importNmModData <- function()
{
	DATACOLNAMES <- c("File", "IG", "ACCEPT", "REWIND", "RECORDS", "TRANSLATE", "NULL")
	.importNmModData <- RNMImport:::.importNmModData
	# multiple IGNORE= statements (issue )
	
	dataStatement1 <- "$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)"
	dataTest1 <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)")
	dataExpected1 <- matrix(c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",     
			REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest1,dataExpected1, msg = " |Mutliple IGNORE= now handled" )
	
	dataStatement2 <- "$DATA data3.dat IGNORE=@"
	dataTest2 <- RNMImport:::.importNmModData(dataStatement2)
	dataExpected2 <- matrix(c( File="data3.dat", IG="@", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest2,dataExpected2, msg = " |IGNORE=@ correct" )
	
	dataStatement3 <- "$DATA data3.dat IGNORE='C'"
	dataTest3 <- RNMImport:::.importNmModData(dataStatement3)
	dataExpected3 <- matrix(c( File="data3.dat", IG="C", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest3,dataExpected3, msg = " |IGNORE='C' correct" )
	
	dataStatement4 <- "$DATA data3.dat IGNORE=\"C\""
	dataTest4 <- RNMImport:::.importNmModData(dataStatement4)
	checkEquals( dataTest4,dataExpected3, msg = " |IGNORE=\"C\" correct" )
	
	dataStatement5 <- "$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ.1)"
	dataTest5 <- .importNmModData(dataStatement5)
	dataExpected5 <- matrix(c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest5,dataExpected5, msg = " |Multiple IGNORE= with IGNORE=\"I\" is correct" )
	
	# check IGNORE statement with "=" instead of .EQ.
	
	dataStatement6 <- "$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME=1)"
	dataTest6 <- .importNmModData(dataStatement6)
	dataExpected6 <- matrix(c( File="data3.dat", IG="I;(TIME=1)", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	
	checkEquals( dataTest6, dataExpected6, msg = " |= in code in IGNORE=(code) returned correctly" )
	
	# check compound IGNORE= statement
	
	dataStatement7 <- "$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ.1,DV.LT.2.01)"
	dataTest7 <- .importNmModData(dataStatement7)
	dataExpected7 <- matrix(c( File="data3.dat", IG="I;(TIME.EQ.1,DV.LT.2.01)", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	
	checkEquals( dataTest7, dataExpected7, msg = " |compound code in IGNORE=(code) returned correctly" )
			
	# check REWIND default value
	
	dataStatement8 <- "$DATA data3.dat REWIND IGNORE=I ACCEPT=(TIME.NE.1)"
	dataExpected8 <- matrix(c( File="data3.dat", IG="I", ACCEPT="(TIME.NE.1)",     
				REWIND="TRUE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	dataTest8 <- .importNmModData(dataStatement8)
	checkEquals( dataTest8, dataExpected8, msg = " | ACCEPT and REWIND are correct " )
	
	dataStatement9 <- "$DATA data3.dat NOREWIND IGNORE=I ACCEPT=(TIME.NE.1)"
	dataExpected9 <- matrix(c( File="data3.dat", IG="I", ACCEPT="(TIME.NE.1)",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	dataTest9 <- .importNmModData(dataStatement9)
	checkEquals( dataTest9, dataExpected9, msg = " | ACCEPT and REWIND are correct " )
	
}