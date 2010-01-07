# $Rev$
# $LastChangedDate$

# simple test case on some local test runs

test.importNm.Basic <- function()
{
	# start with the test run
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun") )
	run1 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
			path = "(internalunit)")
	
	checkEquals( basename(run1@controlFileInfo$fileName), "testdata1.ctl" )
	checkEquals( basename(run1@reportFileInfo$fileName), "testdata1.lst") 
	checkEquals(run1@numProblems, 1)
	checkEquals(length(run1@reportText), 166 )
	checkEquals(length(run1@controlText), 36 )
	
	prob1 <- getProblem(run1)
	
	checkEquals( getThetas(prob1, stdErrors = FALSE), getThetas(run1, stdErrors = FALSE) )
	expThetas <- matrix(c(19.6, 84.6, 1.66), 1, 3 )
	colnames(expThetas) <- paste("THETA", 1:3, sep = "")
	rownames(expThetas) <- c("estimates") 
	checkEquals( getThetas(prob1, stdErrors = FALSE),  expThetas)
	
	"%pst%" <- RNMImport:::"%pst%"
	expOmega <- array(diag(c(.164, .165, 1.3)), c(3,3), dimnames = list( "OMEGA" %pst% 1:3, "OMEGA" %pst% 1:3))
	checkEquals(expOmega, getOmegas(prob1, what = "final"))
	
	expSigma <- array(0.0202, c(1,1), dimnames = list("SIGMA1", "SIGMA1"))
	checkEquals(getSigmas(run1, what = "final"), expSigma)
	
	# check importNm with dropInputColumns = FALSE
	run2 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
				path = "(internalunit)", dropInputColumns = FALSE)
	
	run1.inData <- nmData(run1, dataType = "input")
	run2.inData <- nmData(run2, dataType = "input")
	
	checkTrue(setequal(names(run2.inData), c(names(run1.inData), "RATE")), msg = "Correct columns were dropped")	
	# now try importing a run without output tables
	run3 <- importNm("TestData1notab.ctl", path = system.file(package = "RNMImport", "unittests/testdata/TestRunNoTab"))
	prob3 <- getProblem(run3)
	
	checkException(nmData(prob3), "Output data missing, so there is an incompatibility in data size\n")
	checkEquals(nmData(prob3, dataType = "output"), data.frame())
	
	checkEquals(list(getThetas(prob3), getOmegas(prob3), getSigmas(prob3)), 
			list(getThetas(prob1), getOmegas(prob1), getSigmas(prob1)), "parameter estimates still the same, and available")
	
}

test.importNm.SimModel <- function()
{
	run2 <- importNm(conFile = "../TestSimRun/TestData1SIM.con", path = "(internalunit)")
	prob <- getProblem(run2)
	checkTrue(class(prob) == "NMSimModel")
	checkEquals(prob@numSimulations, 5)
	
	thetas <- getThetas(prob, subProblemNum = 1:5)
	omegas <- getOmegas(prob, subProblemNum = 1:5)
	sigmas <- getSigmas(prob, subProblemNum = 1:5)
	checkEquals(dim(thetas), c(5,3))
	checkEquals(dim(omegas), c(3,3,5))
	checkEquals(dim(sigmas), c(1,1,5))
	
	checkTrue(all(thetas["sim3",] == c(18, 108, 1.24)), 
			msg = "Spot check for thetas, subproblem 3")
	checkTrue(all( thetas["sim1",] ==  c(17.2, 117, 1.24)),
			msg = "Spot check for thetas, subproblem 1")
	
	x <- omegas[,,"sim5"]
	y <- diag(c(0.174, 0.197, 1.47)) ;	dimnames(x) <- NULL
	checkEquals(x, y)
	x <- omegas[,,"sim4"] ; dimnames(x) <- NULL
	y <- diag(c(0.245, 0.189, 0.945))
	checkEquals(x, y)
	
	checkEquals(sigmas[,,"sim2"], 0.026)
	checkEquals(sigmas[,,"sim3"], 0.0262 )
	
	objectives <- getObjective(run2, subProblems = 1:5)
	y <- c(3696.247, 3575.252, 3660.355, 3606.526, 3701.472)
	names(y) <- paste("sim", 1:5, sep = "") 
	checkEquals(objectives, y)
}