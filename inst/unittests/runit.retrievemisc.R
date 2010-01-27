
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
	
	run3 <- testRuns$NMBasicNM7 
	prob3 <- getProblem(run3)
	
	checkEquals(getObjective(run3, method = 1:2), getObjective(prob3, method = 1:2), msg = " |getobjective the same on run and NMBasicModelNM7" )
	
	objTarget <- c(3335.250, 2339.093)
	attr(objTarget, "minInfo") <- c("OPTIMIZATION COMPLETED", "STOCHASTIC PORTION COMPLETED")
	checkEquals(getObjective(run3, method = 1:2), objTarget)
	checkEquals(getObjective(prob3, method = 1, addMinInfo = FALSE), 3335.250 )
	checkEquals(getObjective(prob3, method = 2, addMinInfo = FALSE), 2339.093 )
	
	removeNmPath("internalunit")
	
	# NMSimModelNM7
	
}

# test get estimate cov

test.getEstimateCov <- function()
{
	testRuns <- RNMImport:::getInternalTestRuns()
	
	run1 <- testRuns$NMBasicNotab
	prob1 <- getProblem(run1)
	
	checkEquals(getEstimateCov(run1, corMatrix = TRUE) ,getEstimateCov(run1, corMatrix = TRUE))
	# expected covariance matrices
	expCovMat <- structure(c(0.927, 2.16, 0.0347, 0.00617, 0, 0, 0.00252, 0, 0.0471, 
					0.00176, 2.16, 12.7, 0.18, 0.00892, 0, 0, 0.00903, 0, 0.39, 0.000474, 
					0.0347, 0.18, 0.0474, -0.000661, 0, 0, -0.000648, 0, 0.0657, 
					0.000168, 0.00617, 0.00892, -0.000661, 0.000571, 0, 0, 0.000373, 
					0, -0.000778, 1.59e-05, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
					0, 0, 0, 0, 0, 0, 0, 0.00252, 0.00903, -0.000648, 0.000373, 0, 
					0, 0.000486, 0, -0.000812, 6.95e-06, 0, 0, 0, 0, 0, 0, 0, 0, 
					0, 0, 0.0471, 0.39, 0.0657, -0.000778, 0, 0, -0.000812, 0, 0.119, 
					0.000239, 0.00176, 0.000474, 0.000168, 1.59e-05, 0, 0, 6.95e-06, 
					0, 0.000239, 1.04e-05), .Dim = c(10L, 10L), .Dimnames = list(
					c("TH1", "TH2", "TH3", "OM11", "OM12", "OM13", "OM22", "OM23", 
							"OM33", "SG11"), c("TH1", "TH2", "TH3", "OM11", "OM12", "OM13", 
							"OM22", "OM23", "OM33", "SG11")))
	
	expCorMat <- structure(c(1, 0.629, 0.166, 0.268, 0, 0, 0.119, 0, 0.142, 0.567, 
					0.629, 1, 0.232, 0.105, 0, 0, 0.115, 0, 0.317, 0.0413, 0.166, 
					0.232, 1, -0.127, 0, 0, -0.135, 0, 0.874, 0.24, 0.268, 0.105, 
					-0.127, 1, 0, 0, 0.708, 0, -0.0943, 0.206, 0, 0, 0, 0, 0, 0, 
					0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.119, 0.115, -0.135, 
					0.708, 0, 0, 1, 0, -0.107, 0.0978, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
					0, 0.142, 0.317, 0.874, -0.0943, 0, 0, -0.107, 0, 1, 0.215, 0.567, 
					0.0413, 0.24, 0.206, 0, 0, 0.0978, 0, 0.215, 1), .Dim = c(10L, 
					10L), .Dimnames = list(c("TH1", "TH2", "TH3", "OM11", "OM12", 
							"OM13", "OM22", "OM23", "OM33", "SG11"), c("TH1", "TH2", "TH3", 
							"OM11", "OM12", "OM13", "OM22", "OM23", "OM33", "SG11")))
	
	checkEquals(getEstimateCov(prob1), expCovMat, msg = " |covariance matrix as expected")
	test1 <- getEstimateCov(run1, corMatrix = TRUE)
	
	checkEquals(test1, list("covariance" = expCovMat, "correlation" = expCorMat),
			msg = " | extracting with both")

	# check for appropriate error handling
	
	run2 <- testRuns$NMBasic
	
	checkTrue(is.null(getEstimateCov(run2)), msg = " |NULL returned when no parameter covariance matrix found" )
	
}

# tests the following functions:
# getNmVersion
#

test.getNmVersion <- function()
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