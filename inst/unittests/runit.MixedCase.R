
# Test the issue 6947: Support mixed case in NONMEM 7.2
# Description: One of the new features of NONMEM 7.2 is that control stream files may be written in 
#              mixed case. We need to make some changes in 'RNMImport' library to support this feature.
# What the test do: Test the mixed case control files. 
test.MixedCase.issue6947 <- function()
{
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestMixedCase") )

	run2 <- importNm(conFile = "TestNoSigMaxeval0/Test.ctl", 
			reportFile = "TestNoSigMaxeval0/Test.lst", path = "(internalunit)")
	run2_mc <- importNm(conFile = "TestNoSigMaxeval0_MixedCase/Test_MixedCase.ctl", 
			reportFile = "TestNoSigMaxeval0_MixedCase/Test_MixedCase.lst", path = "(internalunit)")
	checkEquals(run2@problems[[1]]@parameterIterations, run2_mc@problems[[1]]@parameterIterations, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@objectiveFinal, run2_mc@problems[[1]]@objectiveFinal, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@methodNames, run2_mc@problems[[1]]@methodNames, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@thetaFinal, run2_mc@problems[[1]]@thetaFinal, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@sigmaFinal, run2_mc@problems[[1]]@sigmaFinal, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@omegaFinal, run2_mc@problems[[1]]@omegaFinal, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@thetaStderr, run2_mc@problems[[1]]@thetaStderr, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@sigmaStderr, run2_mc@problems[[1]]@sigmaStderr, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@omegaStderr, run2_mc@problems[[1]]@omegaStderr, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@ETAShrinkage, run2_mc@problems[[1]]@ETAShrinkage, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@EPSShrinkage, run2_mc@problems[[1]]@EPSShrinkage, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@parameterCovMatrices, run2_mc@problems[[1]]@parameterCovMatrices, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@parameterCorMatrices, run2_mc@problems[[1]]@parameterCorMatrices, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@minInfo, run2_mc@problems[[1]]@minInfo, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@problemStatement, run2_mc@problems[[1]]@problemStatement, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@reportStatements, run2_mc@problems[[1]]@reportStatements, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@nmVersionMajor, run2_mc@problems[[1]]@nmVersionMajor, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@nmVersionMinor, run2_mc@problems[[1]]@nmVersionMinor, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@inputData, run2_mc@problems[[1]]@inputData, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@outputData, run2_mc@problems[[1]]@outputData, tolerance = 0.05)
	checkEquals(run2@problems[[1]]@additionalVars, run2_mc@problems[[1]]@additionalVars, tolerance = 0.05)

	run3 <- importNm(conFile = "TestSimRun/TestData1SIM.con", 
			reportFile = "TestSimRun/TestData1SIM.lst", path = "(internalunit)")
	run3_mc <- importNm(conFile = "TestSimRun_MixedCase/TestData1SIM_MixedCase.con", 
			reportFile = "TestSimRun_MixedCase/TestData1SIM_MixedCase.lst", path = "(internalunit)")
	checkEquals(run3@problems[[1]]@objectiveFinal, run3_mc@problems[[1]]@objectiveFinal, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@methodNames, run3_mc@problems[[1]]@methodNames, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@thetaFinal, run3_mc@problems[[1]]@thetaFinal, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@sigmaFinal, run3_mc@problems[[1]]@sigmaFinal, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@omegaFinal, run3_mc@problems[[1]]@omegaFinal, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@problemStatement, run3_mc@problems[[1]]@problemStatement, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@reportStatements, run3_mc@problems[[1]]@reportStatements, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@nmVersionMajor, run3_mc@problems[[1]]@nmVersionMajor, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@nmVersionMinor, run3_mc@problems[[1]]@nmVersionMinor, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@inputData, run3_mc@problems[[1]]@inputData, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@outputData, run3_mc@problems[[1]]@outputData, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@additionalVars, run3_mc@problems[[1]]@additionalVars, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@numSimulations, run3_mc@problems[[1]]@numSimulations, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@numMethods, run3_mc@problems[[1]]@numMethods, tolerance = 0.05)
	checkEquals(run3@problems[[1]]@reportStatements, run3_mc@problems[[1]]@reportStatements, tolerance = 0.05)
	
}

