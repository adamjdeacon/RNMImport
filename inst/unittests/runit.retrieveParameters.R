# $LastChangedBy: fgochez $
# $LastChangedDate: 2010-01-13 14:43:01 +0000 (Wed, 13 Jan 2010) $
# $Rev: 14659 $
# TODO: Add comment
# 
# Author: rweeks
###############################################################################

# tests getThetas to ensure that  behaviour under various combinations of parameters works as expected
# note that this function is also tested in runit.importNm.R, so this focuses more on
# parameter combinations than anything else


test.getThetas <- function()
{
	#Check NMBasicModel and NMRun
	
	testRuns <- RNMImport:::getInternalTestRuns()
	run1 <- testRuns[["NMBasic"]]
	
	prob1 <- getProblem(run1)
	
	run3 <- testRuns[["NMBasicNotab"]]
	# expThetas <- matrix(c(19.6, 84.6, 1.66), 1, 3, dimnames = list(c("estimates"), c("THETA1", "THETA2", "THETA3")))
	expThetas <- c("THETA1" = 19.6, "THETA2" = 84.6, "THETA3" = 1.66)
	expThetaInitial <- matrix(c(-Inf, 18.7, Inf, -Inf, 87.3, Inf, -Inf, 2.13, Inf), 3,
						dimnames = list(c("lowerBound", "initial", "upperBound"), c("THETA1", "THETA2", "THETA3")))
	expThetaStderrs <-  c("THETA1" = 0.963, "THETA2" = 3.56, "THETA3" = 0.218)
	checkEquals(getThetas(prob1), expThetas)
	checkEquals(getThetas(run1), expThetas)
	
	# final and initial together
	
	checkEquals(getThetas(prob1, what = c("final", "initial")), rbind( expThetaInitial, "estimates" = expThetas),
				msg = " |what = final and initial works correctly")
	
	tryStderrs <- try(getThetas(run1, what = "stderrors"), silent = TRUE)
	checkTrue(length(grep(tryStderrs, pattern = "Standard errors not available")) > 0, 
			msg = " | correct error message when no standard errors available")
	
	checkEquals(getThetas(run3, what = c("final", "stderrors")), 
			rbind("estimates" = expThetas, "standardErrors" = expThetaStderrs), 
			msg = " | final and stderrs together is correct" )
	
	############ Check NMSimModel
	
	testDir2 <- file.path(unitTestPath, "testdata/TestSimRun")
	
	run2 <- testRuns[["NMSimMod"]]
	
	prob2 <- getProblem(run2)
	
	simThetas <- matrix(c(17.2, 18.3, 18.0, 19.0, 17.1, 117.0, 108.0, 108.0, 98.9, 109.0, 1.24, 1.40, 1.24, 1.55, 1.38), 5, 3)
	dimnames(simThetas) <- list(c("sim1", "sim2", "sim3", "sim4", "sim5"), c("THETA1", "THETA2", "THETA3"))
	simInitial <- c(19.6, 84.6, 1.66)
	
	names(simInitial) <- c("THETA1", "THETA2", "THETA3")
	
	checkEquals(getThetas(prob2, subProblemNum = 1:5),  simThetas, msg = " |for sim model, final as expected")
	
	checkEquals(getThetas(prob2, subProblemNum = 1:5, what = c("final", "initial")),  
			list("initial.estimates" = simInitial, "final.estimates" = simThetas ), 
			msg = " |for sim model, final + initial as expected")
	
	######## NONMEM 7 run
	
}

test.getOmegas <- function()
{
	########### Check NMBasicModel and NMRun
	testDir1 <- file.path(unitTestPath, "testdata/TestRun")
	
	testRuns <- RNMImport:::getInternalTestRuns()
	run1 <- testRuns[["NMBasic"]]
	
	prob1 <- getProblem(run1)
	
	expOmegas <- array(c(0.164, 0, 0, 0, 0.165, 0, 0, 0, 1.3), c(3,3))
#	dimnames(expOmegas) = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", "ETA3"), "estimates)
	
	dimnames(expOmegas) = list(paste("OMEGA", 1:3, sep = ""), paste("OMEGA", 1:3, sep = ""))
	expOmegaInit <- diag(c(0.128, 0.142, 1.82))
	dimnames(expOmegaInit) = list(c("OMEGA1", "OMEGA2", "OMEGA3"), c("OMEGA1", "OMEGA2", "OMEGA3"))
	
	checkEquals(getOmegas(prob1),  expOmegas)
	checkEquals(getOmegas(run1),  expOmegas)
	
	omegaTest1 <- getOmegas(prob1, what = "initial")
	omegaTest2 <- getOmegas(run1, what = "initial")
	
	checkEquals(omegaTest1,  expOmegaInit)
	checkEquals(omegaTest2,  expOmegaInit)	
	
	run3 <- importNm(conFile = "testdata1notab.ctl", 
			reportFile = "testdata1notab.lst", 
			path = file.path(unitTestPath,"testdata/testrunnotab"))
	
	
	#Check NMSimModel
	
	
	# run2 <- importNm(conFile = "testdata1sim.con", reportFile = "testdata1sim.lst", 
	#		path = testDir2)
	
	run2 <- testRuns[["NMSimMod"]]
	prob2 <- getProblem(run2)
	
	simOmegas <- array(dim = c(3,3,5), dimnames = list(paste("OMEGA", 1:3, sep = ""), 
					c(paste("OMEGA", 1:3, sep = "")), 
					c("sim1", "sim2", "sim3", "sim4", "sim5")))
	simOmegas[, , 1] <- diag(c(0.174, 0.19, 1.45)); simOmegas[, , 2] <- diag(c(0.167, 0.143, 1.24))
	simOmegas[, , 3] <- diag(c(0.181, 0.142, 1.57)); simOmegas[, , 4] <- diag(c(0.245, 0.189, 0.945))
	simOmegas[, , 5] <- diag(c(0.174, 0.197, 1.47))
	initialSimOmegas <- diag(c(0.164, 0.165, 1.3)) 
	
	dimnames(initialSimOmegas) <- list(c("OMEGA1", "OMEGA2", "OMEGA3"), c("OMEGA1", "OMEGA2", "OMEGA3"))
	
	checkEquals(getOmegas(prob2, subProblemNum = 1:5),  simOmegas)
	checkEquals(getOmegas(prob2, what = "initial", subProblemNum = 1:5),  initialSimOmegas)
	
	checkEquals(getOmegas(prob2, what = c("final", "initial"), subProblemNum = 1:5), 
			list("initial.estimates" = initialSimOmegas , "final.estimates" = simOmegas))
}

test.getSigmas <- function()
{
	testDir1 <- file.path(unitTestPath, "testdata/TestRun")
	run1 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
			path = testDir1)
	prob1 <- getProblem(run1)
	
	expSigmas <- array(0.0202, c(1,1), dimnames = list("SIGMA1", "SIGMA1"))
	expSigmaInit <- matrix(0.0231, 1,dimnames = list("SIGMA1", "SIGMA1"))
	
	checkEquals(getSigmas(prob1),  expSigmas)
	checkEquals(getSigmas(run1),  expSigmas)
	checkEquals(getSigmas(prob1, what = "initial"),  expSigmaInit)
	checkEquals(getSigmas(run1, what = "initial"),  expSigmaInit)
	
	#Check NMSimModel
	testDir2 <- file.path(unitTestPath, "testdata/TestSimRun")
	run2 <- importNm(conFile = "testdata1sim.con", reportFile = "testdata1sim.lst", 
			path = testDir2)
	prob2 <- getProblem(run2)
	
	simSigmas <- array(c(0.0241, 0.026, 0.0262, 0.0295, 0.0244), dim = c(1,1,5), 
			dimnames = list("SIGMA1", "SIGMA1", c("sim1", "sim2", "sim3", "sim4", "sim5")))
	sigmaInitial <- matrix(0.0202, 1, dimnames = list("SIGMA1", "SIGMA1"))
	
	checkEquals(getSigmas(prob2, subProblemNum = 1:5),  simSigmas)
	checkEquals(getSigmas(prob2, what = "initial", subProblemNum = 1:5), sigmaInitial)
}