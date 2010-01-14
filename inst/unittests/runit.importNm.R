# $Rev: 14658 $
# $LastChangedDate: 2010-01-13 14:41:12 +0000 (Wed, 13 Jan 2010) $

##### 
# SYSTEM
# runit.importNm.R
# Tests importNm, along with getThetas, getOmegas and getSigmas
# Note that these functions cannot really be tested seperately, so there is overlap with runit.retrieveparameters.R


# simple test case on some local test runs, data is included in RNMImport/inst/unittests/testdata
# tests NMBasicModel import

test.importNm.Basic <- function()
{
	# start with the test run
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun") )
	
	# import the basic test run
	
	run1 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
			path = "(internalunit)")
	# TODO: abstract away all of these access to class slots for improved encapsulation
	# tests that the correct file names have been retrieved
	
	checkEquals( basename(run1@controlFileInfo$fileName), "testdata1.ctl", msg = " |correct control file name imported" )
	checkEquals( basename(run1@reportFileInfo$fileName), "testdata1.lst", msg = " |correct list file name imported") 
	
	# test that the the number of problems is correct
	
	checkEquals(run1@numProblems, 1, " |run contains the correct number of problems")
	
	# test import of control file and report file text by checking that the correct number of lines have been imported
	
	checkEquals(length(run1@reportText), 166 , msg = " | Length of list file text correct")
	checkEquals(length(run1@controlText), 36, msg = " |Length of control file text correct" )
	
	prob1 <- getProblem(run1)
	
	# test that the theta final estimates were as expected
	
	expThetas <- c(19.6, 84.6, 1.66)
	names(expThetas) <- paste("THETA", 1:3, sep = "")
	# rownames(expThetas) <- c("estimates") 
	checkEquals( getThetas(prob1, what = "final"),  expThetas, msg = " |theta final estimates correct")
	
	# test that omega final estimates imported as expected
	
	"%pst%" <- RNMImport:::"%pst%"
	expOmega <- array(diag(c(.164, .165, 1.3)), c(3,3), dimnames = list( "OMEGA" %pst% 1:3, "OMEGA" %pst% 1:3))
	checkEquals(expOmega, getOmegas(prob1, what = "final"), msg = " |omega final estimates imported as expected")
	
	# check that sigma final estimates imported as expected
	
	expSigma <- array(0.0202, c(1,1), dimnames = list("SIGMA1", "SIGMA1"))
	checkEquals(getSigmas(run1, what = "final"), expSigma, msg = " |sigma final estimates imported as expected")
	
	# check importNm works correctly with dropInputColumns = FALSE
	run2 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
				path = "(internalunit)", dropInputColumns = FALSE)
	
	run1.inData <- nmData(run1, dataType = "input")
	run2.inData <- nmData(run2, dataType = "input")
	
	checkTrue(setequal(names(run2.inData), c(names(run1.inData), "RATE")), msg = "Correct columns were dropped")	
	
	# now try importing a run without output tables
	
	run3 <- importNm("TestData1notab.ctl", path = system.file(package = "RNMImport", "unittests/testdata/TestRunNoTab"))
	prob3 <- getProblem(run3)
	
	checkException(nmData(prob3), msg = " |Output data missing, so there is an incompatibility in data size\n")
	
	# no output tables, so the output data set should be empty
	
	checkEquals(nmData(prob3, dataType = "output"), data.frame(), msg = " |output data is empty")
	
	checkEquals(list(getThetas(prob3), getOmegas(prob3), getSigmas(prob3)), 
			list(getThetas(prob1), getOmegas(prob1), getSigmas(prob1)), " |parameter estimates still the same, and available")
	
}

# tests for NMSimModel, still based on internal data (TestData/TestSimRun)

test.importNm.SimModel <- function()
{
	run2 <- importNm(conFile = "../TestSimRun/TestData1SIM.con", path = "(internalunit)")
	prob <- getProblem(run2)
	
	# check that the class of the problem is correct
	
	checkTrue(class(prob) == "NMSimModel", msg = " |problem class is correct")
	
	# check that the number of simulations is correct
	
	checkEquals(prob@numSimulations, 5, msg = " |5 simulations expected")
	
	# extract all parameter estimates 
	
	thetas <- getThetas(prob, subProblemNum = 1:5)
	omegas <- getOmegas(prob, subProblemNum = 1:5)
	sigmas <- getSigmas(prob, subProblemNum = 1:5)
	
	checkEquals(dim(thetas), c(5,3), msg = " |dimension of all extracted subproblems as expected")
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

# tests for NONMEM 7 basic model, based on internal dataset testdata/wexample1

test.importNm.BasicNM7 <- function()
{
	run3 <- importNm(conFile = "../wexample1/wexample1.ctl", path = "(internalunit)")
	prob <- getProblem(run3)
	
	# check that "prob" is of the correct class
	actualClass <- class(prob)
	attributes(actualClass) <- NULL
	checkEquals(actualClass, "NMBasicModelNM7", msg = " | correct class for problem")
	
	# check that omegas were imported correctly
	
	# check initial values
	omegaInit <- getOmegas(prob, what = "initial")
	checkEquals(omegaInit, 
			structure(c(0.4, 0.001, 0.001, 0.001, 0.001, 0.4, 0.001, 0.001, 
							0.001, 0.001, 0.4, 0.001, 0.001, 0.001, 0.001, 0.4), .Dim = c(4L, 
							4L), .Dimnames = list(structure(c("OMEGA1", "OMEGA2", "OMEGA3", 
											"OMEGA4"), .Dim = c(4L, 1L)), structure(c("OMEGA1", "OMEGA2", 
											"OMEGA3", "OMEGA4"), .Dim = c(4L, 1L)))),
			msg = " |omega initials correct")
	
	# check final values and standard errors
	omegasFinal <- lapply(as.numeric(1:4), function(i) getOmegas(prob, method = i, what = "final"))
	omegasStderrs <- lapply(as.numeric(c(1, 3, 4)), function(i) getOmegas(prob, method = i, what = "stderrors"))

	# check that omega final values were retrieved correctly
	checkEquals(omegasFinal, 
			list(structure(c(0.165, 0.00413, 0.00538, -0.0161, 0.00413, 0.133, 
									0.0172, 0.0123, 0.00538, 0.0172, 0.21, 0.0508, -0.0161, 0.0123, 
									0.0508, 0.161), .Dim = c(4L, 4L), .Dimnames = list(c("ETA1", 
											"ETA2", "ETA3", "ETA4"), c("ETA1", "ETA2", "ETA3", "ETA4"))), 
					structure(c(0.163, 0.00239, 0.00543, -0.0214, 0.00239, 0.146, 
									0.0283, 0.0281, 0.00543, 0.0283, 0.194, 0.0304, -0.0214, 
									0.0281, 0.0304, 0.155), .Dim = c(4L, 4L), .Dimnames = list(
									c("ETA1", "ETA2", "ETA3", "ETA4"), c("ETA1", "ETA2", 
											"ETA3", "ETA4"))), structure(c(0.163, 0.00239, 0.00543, 
									-0.0214, 0.00239, 0.146, 0.0283, 0.0281, 0.00543, 0.0283, 
									0.194, 0.0304, -0.0214, 0.0281, 0.0304, 0.155), .Dim = c(4L, 
									4L), .Dimnames = list(c("ETA1", "ETA2", "ETA3", "ETA4"), 
									c("ETA1", "ETA2", "ETA3", "ETA4"))), structure(c(0.165, 
									-0.000752, 0.0124, -0.0128, -0.000752, 0.131, 0.0159, 0.0139, 
									0.0124, 0.0159, 0.188, 0.0333, -0.0128, 0.0139, 0.0333, 0.15
							), .Dim = c(4L, 4L), .Dimnames = list(c("ETA1", "ETA2", "ETA3", 
											"ETA4"), c("ETA1", "ETA2", "ETA3", "ETA4")))),
			msg = " |omega final values imported correctly")
	
	# check that omega standard errors were retrieved correctly

	checkEquals(omegasStderrs, 
			list(structure(c(0.0289, 0.0231, 0.0324, 0.0276, 0.0231, 0.0348, 
									0.0382, 0.0273, 0.0324, 0.0382, 0.0649, 0.0451, 0.0276, 0.0273, 
									0.0451, 0.0405), .Dim = c(4L, 4L), .Dimnames = list(c("ETA1", 
											"ETA2", "ETA3", "ETA4"), c("ETA1", "ETA2", "ETA3", "ETA4"))), 
					structure(c(0.0268, 0.022, 0.0292, 0.0234, 0.022, 0.0352, 
									0.0367, 0.0252, 0.0292, 0.0367, 0.0595, 0.0362, 0.0234, 0.0252, 
									0.0362, 0.0379), .Dim = c(4L, 4L), .Dimnames = list(c("ETA1", 
											"ETA2", "ETA3", "ETA4"), c("ETA1", "ETA2", "ETA3", "ETA4"
									))), structure(c(0.027, 0.0213, 0.0304, 0.0238, 0.0213, 0.0314, 
									0.0354, 0.0235, 0.0304, 0.0354, 0.078, 0.0461, 0.0238, 0.0235, 
									0.0461, 0.0396), .Dim = c(4L, 4L), .Dimnames = list(c("ETA1", 
											"ETA2", "ETA3", "ETA4"), c("ETA1", "ETA2", "ETA3", "ETA4"
									)))),
	msg = " |standard errors extracted correctly")
	
	sigmasFinal <- lapply(as.numeric(1:4), function(i) getSigmas(prob, method = i, what = "final"))
	sigmasStderrs <- lapply(as.numeric(c(1, 3, 4)), function(i) getSigmas(prob, method = i, what = "stderrors"))
	
	checkEquals( sigmasStderrs, 
			list(structure(0.00772, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
									"EPS1")), structure(0.00758, .Dim = c(1L, 1L), .Dimnames = list(
									"EPS1", "EPS1")), structure(0.0082, .Dim = c(1L, 1L), .Dimnames = list(
									"EPS1", "EPS1"))),
			msg = " |sigma standard errors correct")
	checkEquals(sigmasFinal,
			list(structure(0.0553, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
									"EPS1")), structure(0.058, .Dim = c(1L, 1L), .Dimnames = list(
									"EPS1", "EPS1")), structure(0.058, .Dim = c(1L, 1L), .Dimnames = list(
									"EPS1", "EPS1")), structure(0.0572, .Dim = c(1L, 1L), .Dimnames = list(
									"EPS1", "EPS1"))),
			msg = " |sigma final estimates correct")

	
}