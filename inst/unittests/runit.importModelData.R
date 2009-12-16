# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################

# test the importModelData function

test.importModelData <- function()
{
	# first test: checks that multiple IGNORE= are handled correctly
	testDir <- file.path(unitTestPath, "testdata")
	# x <- readNmData(file.path(testDir, "data3"))
	
	dataStatement <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)")
	
	inputStatement <- RNMImport:::.importNmModInput("$INPUT AMT TIME DV")
	
	testInput1 <- as.matrix(RNMImport:::importModelData(dataStatement, inputStatement, path = testDir))
	rownames(testInput1) <- NULL
	# checkEquals(testInput$TIME, 0, .27)
	checkEquals(testInput1, cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA), 
					TIME = c(0,.27,0.52,1.92,3.5,5.02,9,12),
					DV = c(NA,1.71,7.91,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=c and IGNORE=list work correctly at the same time")
	# second test: 
	dataStatement2 <- RNMImport:::.importNmModData("$DATA data4.dat")
	inputStatement2 <- RNMImport:::.importNmModInput("$INPUT AMT TIME DV")
	
	testInput2 <- as.matrix(RNMImport:::importModelData(dataStatement2, inputStatement2, path = testDir))
	rownames(testInput2) <- NULL
	checkEquals( testInput2,  cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA,NA), 
					TIME = c(0,.27,0.52,1,1.92,3.5,5.02,9,12),
					DV = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=# by default!" )
	
	# third tests: IGNORE=list used alone
	
	dataStatement3 <- RNMImport:::.importNmModData("$DATA data3 IGNORE=(TIME.LT.1)")
	inputStatement3 <- RNMImport:::.importNmModInput("$INPUT AMT TIME DV")
	
	testInput3 <- as.matrix( RNMImport:::importModelData(dataStatement3, inputStatement3, path = testDir) )
	rownames(testInput3) <- NULL

	checkEquals( testInput3,
			cbind(AMT = rep(NA, 8), 
					TIME = c(1,1.92,3.5,5.02,7.03,9,12,24.3),
					DV = c(8.31,8.33,6.85,6.08,5.40,4.55,3.01,0.9)), 
			msg = " |IGNORE=code alone works as expected" )
	# test IGNORE = 'C'
	dataStatement4 <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE='I'")
	testInput4 <-  as.matrix( RNMImport:::importModelData(dataStatement4, inputStatement, path = testDir) )
	rownames(testInput4) <- NULL
	checkEquals( testInput4,  cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA,NA), 
					TIME = c(0,.27,0.52,1,1.92,3.5,5.02,9,12),
					DV = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE='I' same as IGNORE=I " )
	
	dataStatement5 <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE=\"I\"")
	testInput5 <-  as.matrix( RNMImport:::importModelData(dataStatement5, inputStatement, path = testDir) )
	rownames(testInput5) <- NULL
	checkEquals( testInput5,  cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA,NA), 
					TIME = c(0,.27,0.52,1,1.92,3.5,5.02,9,12),
					DV = c(NA,1.71,7.91,8.31,8.33,6.85,6.08,4.55,3.01)), msg = " |IGNORE=\"I\" same as IGNORE=I " )
	
}
