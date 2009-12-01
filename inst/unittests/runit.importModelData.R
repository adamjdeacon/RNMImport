# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################

# test the importModelData function

test.importModelData <- function()
{
	testDir <- file.path(unitTestPath, "testdata")
	# x <- readNmData(file.path(testDir, "data3"))
	
	dataStatement <- RNMImport:::.importNmModData("$DATA data3.dat IGNORE=I IGNORE=(TIME.EQ.1)")
	
	inputStatement <- RNMImport:::.importNmModInput("$INPUT AMT TIME DV")
	
	testInput1 <- as.matrix(RNMImport:::importModelData(dataStatement, inputStatement, path = testDir))
	rownames(testInput1) <- NULL
	# checkEquals(testInput$TIME, 0, .27)
	checkEquals(testInput1, cbind(AMT = c(320,NA,NA,NA,NA,NA,NA,NA), 
					TIME = c(0,.27,0.52,1.92,3.5,5.02,9,12),
					DV = c(NA,1.71,7.91,8.33,6.85,6.08,4.55,3.01)))
	
}
