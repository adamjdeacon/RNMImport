# DOSE=AMT TIME CP=DV

test.readNmData <- function()
{
	testDir <- file.path(unitTestPath, "testdata")
	x <- readNmData(file.path(testDir, "data3"))
	checkEquals(dim(x), c(11, 3))
	y <- colMeans(x, na.rm = TRUE)
	checkEquals(y, c(320.0, 5.869091, 5.305), tol = 1e-06, checkNames = FALSE)
	# 320.000000   5.869091   5.305000
	
	x.2 <- readNmData(file.path(testDir, "data3.dat"), ignore = "I")
	checkEquals(dim(x.2), c(9, 3))
	checkEquals(names(x.2), c("AMT", "TIME", "DV"))
	checkEquals(x.2$AMT, x[ -c(8, 11),1])
	
	x.3 <- readNmData(file.path(testDir, "data3"), 
			ignore = "(V2.EQ.12)")
	checkEquals(x[-10,], x.3 )
}
