
test.getObjective <- function()
{
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun") )
	run1 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
			path = "(internalunit)")
	
	obj <- getObjective(run1, addMinInfo = FALSE)
	
	checkEquals(obj, 3228.988)
	checkEquals(getObjective(getProblem(run1), addMinInfo = FALSE), 3228.988 )
	
	obj <- getObjective(run1, addMinInfo = TRUE)
	target <- 3228.988
	attr(target, "minInfo") <- c("minResult" = "SUCCESSFUL", "numEval" = "143", 
			"numSigDigits" = "3.5")
	checkEquals(obj, target)
	
	removeNmPath("internalunit")
	
}
