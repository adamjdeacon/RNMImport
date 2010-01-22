
# tests retrieval of objective function
# getObjective

test.getObjective <- function()
{
	testRuns <- RNMImport:::getInternalTestRuns()
	
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun") )
	run1 <- testRuns$NMBasic
	# NMBasicModel

	checkEquals(getObjective(getProblem(run1), addMinInfo = FALSE), 3228.988 ) 
	
	obj <- getObjective(run1, addMinInfo = TRUE)
	target <- 3228.988
	attr(target, "minInfo") <- c("minResult" = "SUCCESSFUL", "numEval" = "143", 
			"numSigDigits" = "3.5")
	checkEquals(obj, target)
	
	# NMSimModel
	
	run2 <- testRuns$NMSimMod
	prob2 <- getProblem(run2)
	checkEquals(getObjective(run2, subProblems = 1:5), getObjective(prob2, subProblems = 1:5))
	checkEquals(getObjective(prob2, subProblems = c(2, 4)), 
			structure(c(3575.252, 3606.526), .Names = c("sim2","sim4")),
			msg = " |objective functions for simulation problem are correct")
	
	# NMBasicModelNM7
	# TODO: complete this:
	# run3 <- importNm( conFile = "TestData1.ctl", path = path = file.path(unitTestPath, "testdata/Test"), reportFile = "testdata1SIM.LST"  )
	
	# NMSimModelNM7
	
	run3 <- testRuns$NMBasicNM7 
	prob3 <- getProblem(run3)
	
	checkEquals(getObjective(run3, method = 1:2), getObjective(prob3, method = 1:2), msg = " |getobjective the same on run and NMBasicModelNM7" )
	
	objTarget <- c(3335.250, 2339.093)
	attr(objTarget, "minInfo") <- c("OPTIMIZATION COMPLETED", "STOCHASTIC PORTION COMPLETED")
	checkEquals(getObjective(run3, method = 1:2), objTarget)
	checkEquals(getObjective(prob3, method = 1, addMinInfo = FALSE), 3335.250 )
	checkEquals(getObjective(prob3, method = 2, addMinInfo = FALSE), 2339.093 )
	
	removeNmPath("internalunit")
	
}

# tests the following functions:
# getNmVersion
#

test.getMisc <- function()
{
	testRuns <- RNMImport:::getInternalTestRuns()
	
	run1 <- testRuns$NMBasicNotab 
	run2 <- testRuns$NMSimMod	
	
	checkEquals( getNmVersion(run1), c(major = "VI", "minor" = "2" ) , " version of run1 is correct")
	checkEquals( getNmVersion(run2), c(major = "VI", "minor" = "1" ) , " version of run2 is correct")
	prob1 <- getProblem(run1)
	prob2 <- getProblem(run2)
	checkEquals( getNmVersion(prob1), c(major = "VI", "minor" = "2" ) , " version of run1 problem is correct")
	checkEquals( getNmVersion(prob2), c(major = "VI", "minor" = "1" ) , " version of run2 problem is correct")
	
}