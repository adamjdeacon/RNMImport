# SVN revision: $Rev$
# Date of last change: $LastChangedDate$
# Last changed by: $LastChangedBy$
# 
# Original author: fgochez
# Copyright Mango Solutions, Chippenham, UK
###############################################################################

# tests .importNmModData

test.importNmModData <- function()
{
	DATACOLNAMES <- c("File", "IG", "ACCEPT", "REWIND", "RECORDS", "TRANSLATE", "NULL")
	.importNmModData <- RNMImport:::.importNmModData
	
	dataStatements <- 
		c(
			"$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)", #1
			"$DATA data3.dat IGNORE=@", #2
			"$DATA data3.dat IGNORE='C'", #3 
			"$DATA data3.dat IGNORE=\"C\"", #4
			"$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ.1)", #5
			"$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME=1)", #6
			"$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ.1,DV.LT.2.01)", #7
			"$DATA data3.dat REWIND IGNORE=I ACCEPT=(TIME.NE.1)", #8
			"$DATA data3.dat NOREWIND IGNORE=I ACCEPT=(TIME.NE.1)", #9
			"$DATA data3.dat IGN=@", #10
			"$DATA data3.dat IGN=I IGN=(TIME.EQ.1)", #11
			"$DATA data3.dat IGN=I IGNORE=(TIME.EQ.1)", #12
			"$DATA data3.dat IGN=I IGNORE(TIME.EQ.1)", #13
			"$DATA data3.dat IGNORE=I IGN(TIME.EQ.1)", #14
			"$DATA data3.dat", #15
			"$DATA data3.dat IGNORE=I IGN(TIME .EQ. 1)", # 16 issue 4961
			"$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ. 1,DV .LT.2.01)", #17
			"$DATA data3.dat IGN(DV .LT.2.01)" #18
			
		)
	
	dataExpected <-
		list(
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",   #1
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="@", ACCEPT="",     #2
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="C", ACCEPT="",     #3
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="C", ACCEPT="",     #4
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="", #5     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME=1)", ACCEPT="",    #6
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1,DV.LT.2.01)", ACCEPT="", # 7     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I", ACCEPT="(TIME.NE.1)",  #8 
				REWIND="TRUE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I", ACCEPT="(TIME.NE.1)",  #9 
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="@", ACCEPT="",     #10
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",   #11  
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="(TIME.EQ.1);I", ACCEPT="",   #12
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="(TIME.EQ.1);I", ACCEPT="", #13   
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="", #14
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),			
			c( File="data3.dat", IG="NONE", ACCEPT="", #15     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",   #16
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="I;(TIME.EQ.1,DV.LT.2.01)", ACCEPT="", #17     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ),
			c( File="data3.dat", IG="(DV.LT.2.01)", ACCEPT="",     #18
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= "" )	
		)
		
	allDataTests <- t(sapply(dataStatements, .importNmModData))
	rownames(allDataTests) <- NULL
	allDataExpected <- do.call(rbind, dataExpected)
	colnames(allDataExpected) <- colnames(allDataTests)
	
	checkEquals(allDataTests, allDataExpected, msg = " all inputs and outputs as expected")
	# multiple IGNORE= statements (issue )
	
	# code below is deprecated, will remove later
	return()
	dataStatement1 <- "$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)"
	dataTest1 <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)")
	dataExpected1 <- matrix(c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",     
			REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest1,dataExpected1, msg = " |Mutliple IGNORE= now handled" )
## #	
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
	
#	# check IGNORE statement with "=" instead of .EQ.
## #	
	dataStatement6 <- "$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME=1)"
	dataTest6 <- .importNmModData(dataStatement6)
	dataExpected6 <- matrix(c( File="data3.dat", IG="I;(TIME=1)", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
## #	
	checkEquals( dataTest6, dataExpected6, msg = " |= in code in IGNORE=(code) returned correctly" )
## #	
#	# check compound IGNORE= statement
## #	
	dataStatement7 <- "$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ.1,DV.LT.2.01)"
	dataTest7 <- .importNmModData(dataStatement7)
	dataExpected7 <- matrix(c( File="data3.dat", IG="I;(TIME.EQ.1,DV.LT.2.01)", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
# #	
	checkEquals( dataTest7, dataExpected7, msg = " |compound code in IGNORE=(code) returned correctly" )
## #			
#	# check REWIND default value
	
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
## #	
 	# check that IGNORE can be replaced with IGN
	
	dataStatement10 <- "$DATA data3.dat IGN=@"
	dataTest10 <- RNMImport:::.importNmModData(dataStatement10)
	dataExpected10 <- matrix(c( File="data3.dat", IG="@", ACCEPT="",     
 			REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest10,dataExpected10, msg = " |IGN=@ correct" )
## #	
#	# second check (multiple ignore)
	
	dataStatement11 <- "$DATA data3.dat IGN=I IGN=(TIME.EQ.1)"
	dataTest11 <- RNMImport:::.importNmModData(dataStatement11)
	dataExpected11 <- matrix(c( File="data3.dat", IG="I;(TIME.EQ.1)", ACCEPT="",     
				REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	checkEquals( dataTest11,dataExpected11, msg = " |IGN= now handled" )
#	
#	# mixed IGN and IGNORE

	dataStatement12 <- "$DATA data3.dat IGN=I IGNORE=(TIME.EQ.1)"
	dataTest12 <- RNMImport:::.importNmModData(dataStatement12)
	dataExpected12 <- matrix(c( File="data3.dat", IG="(TIME.EQ.1);I", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES ))
	checkEquals( dataTest12,dataExpected12, msg = " |Mutliple IGN= now handled" )

	# now check that we can do IGNORE(code) or IGN(code)

	dataStatement13 <- "$DATA data3.dat IGN=I IGNORE(TIME.EQ.1)"
	dataTest13 <- RNMImport:::.importNmModData(dataStatement13)
	checkEquals( dataTest13,dataExpected12, msg = " | IGNORE(code) now handled" )
	
	dataStatement14 <- "$DATA data3.dat IGNORE=I IGN(TIME.EQ.1)"
	dataTest14 <- RNMImport:::.importNmModData(dataStatement14)
	checkEquals( dataTest14,dataExpected11, msg = " | IGN(code) also handled" )
	
	# check that blank no ignore statements give "NONE"
	dataStatement15 <- "$DATA data3.dat"
	dataTest15 <- RNMImport:::.importNmModData(dataStatement15)
	
	dataExpected15 <- matrix(c( File="data3.dat", IG="NONE", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES ))
	
	checkEquals( dataTest15,dataExpected15, msg = " | no IGNORE returns NONE" )
	
	# issue 4691: need to eliminate whitespace around operators (e.g. TIME .EQ. 1)
	
	dataStatement16 <- "$DATA data3.dat IGNORE=I IGN(TIME .EQ. 1)"
	dataTest16 <- RNMImport:::.importNmModData(dataStatement16)
	checkEquals( dataTest16,dataExpected11, msg = " | IGN(code) has whitespace stripped" )
	
	dataStatement17 <- "$DATA data3.dat IGNORE=\"I\" IGNORE=(TIME.EQ. 1,DV .LT.2.01)"
	dataTest17 <- .importNmModData(dataStatement17)
	dataExpected17 <- matrix(c( File="data3.dat", IG="I;(TIME.EQ.1,DV.LT.2.01)", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	
	checkEquals( dataTest17, dataExpected17, msg = " |compound code in IGNORE=(code) has whitespace stripped" )
	
	dataStatement18 <- "$DATA data3.dat IGN(DV .LT.2.01)"
	dataTest18 <- .importNmModData(dataStatement17)
	dataExpected18 <- matrix(c( File="data3.dat", IG="I;(DV.LT.2.01)", ACCEPT="",     
					REWIND="FALSE", RECORDS="",TRANSLATE= "", NULL= ""  ), nrow = 1, dimnames = list(NULL,DATACOLNAMES )) 
	
	checkEquals( dataTest18, dataExpected18, msg = " |compound code in IGNORE=(code) has whitespace stripped" )
	
	
}