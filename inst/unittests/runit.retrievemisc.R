
# tests retrieval of objective function
# getObjective

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

# tests the following functions:
# getNmVersion
#

test.getMisc <- function()
{
	run1 <- importNm(conFile = "testdata1notab.ctl", reportFile = "testdata1notab.lst", 
			path = file.path(unitTestPath, "testdata/TestRunNoTab"))
	run2 <- importNm(conFile = "TestData1SIM.con", reportFile = "TestData1SIM.lst", 
			path = file.path(unitTestPath, "testdata/TestSimRun"))
	
	checkEquals( getNmVersion(run1), c(major = "VI", "minor" = "1" ) , " version of run1 is correct")
	checkEquals( getNmVersion(run2), c(major = "VI", "minor" = "1" ) , " version of run2 is correct")
	prob1 <- getProblem(run1)
	prob2 <- getProblem(run2)
	checkEquals( getNmVersion(prob1), c(major = "VI", "minor" = "1" ) , " version of run1 problem is correct")
	checkEquals( getNmVersion(prob2), c(major = "VI", "minor" = "1" ) , " version of run2 problem is correct")
	
}