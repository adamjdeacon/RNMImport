# TODO: make these tests more comprehensive!!
# Note: a great deal of the functionality of importNmReport is covered by other unit tests, such
# as importNm, so these tests will not be extensive


test.importNmReport.Basic <- function()
{
	report1 <- importNmReport("TestData1.lst", path = file.path(unitTestPath, "testdata/TestRun"))
	
	report1Class <- class(report1)
	attributes(report1Class) <- NULL
	checkEquals( report1Class, "nmRunReport", msg = " |class of returned object is correct")
	
	checkTrue("VersionInfo" %in% names(report1), msg = " |VersionInfo is a property of the entire report")
	conStatements <- importNmMod("TestData1.ctl", path = file.path(unitTestPath, "testdata/TestRun"))
	report1.withCtl <- importNmReport("TestData1.lst", controlStatements = conStatements, path = file.path(unitTestPath, "testdata/TestRun"))
	checkEquals(report1, report1.withCtl, "report loaded with control statements and without are identical (1)")
	checkEquals(length(report1$Raw), 166, msg = "Raw contents correct length")
	probRes <- report1$problemResults
	checkEquals(names(probRes[[1]]), c("nRecords", "nIndividuals", "Objective.Minimum", "FinalEstimates","Iter" ), 
			"checking presence of correct elements in report")
	
	
}

test.importNmReport.SimModel <- function()
{
	
	report2 <- importNmReport("TestData1SIM.lst", path = file.path(unitTestPath, "testdata/TestSimRun"))
	conStatements <- importNmMod("TestData1SIM.con", path = file.path(unitTestPath, "testdata/TestSimRun"))
	report2.withCtl <- importNmReport("TestData1SIM.lst", controlStatements = conStatements, path = file.path(unitTestPath, "testdata/TestSimRun"))
	
	checkEquals(report2, report2.withCtl, "report loaded with control statements and without are identical (2)")
	probRes <- report2$problemResults
	checkEquals(names(probRes[[1]]), c("nRecords", "nIndividuals", "FinalEstimates"), "checking presence of correct elements in report" )
	
}

test.importNmReport.BasicNM7 <- function()
{
	report3 <- importNmReport("TestData1.lst", path = file.path(unitTestPath, "testdata/TestDataNM7"))
	
	probResults <- report3$problemResults[[1]]
	
	checkEquals( report3$VersionInfo, c("Version" = "VII", "Level" = "1.0"), 
			msg = " |correct version")
	methResults <- probResults$MethodResults
	
	allThetas <- sapply(methResults, function(x) x$FinalEstimates$THETA)
	allSigmas <- lapply(methResults, function(x) x$FinalEstimates$SIGMA)
	allOmegas <- lapply(methResults, function(x) x$FinalEstimates$OMEGA)
	
	# check thetas
	
	checkEquals(allThetas, structure(c(20, 77.3, 1.27, 19.1, 76.7, 1.68), .Dim = c(3L, 2L
					), .Dimnames = list(c("TH1", "TH2", "TH3"), NULL)), 
			msg = " |thetas imported correctly")
	
	# check sigmas
	
	checkEquals(allSigmas, list(structure(0.0259, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
									"EPS1")), structure(0.0266, .Dim = c(1L, 1L), .Dimnames = list(
									"EPS1", "EPS1"))),
		msg = " |sigmas imported correctly")

	
	# check omegas
	
	checkEquals( allOmegas, 
			list(structure(c(0.157, 0, 0, 0, 0.162, 0, 0, 0, 0.737), .Dim = c(3L, 
									3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
											"ETA3"))), structure(c(0.145, 0, 0, 0, 0.149, 0, 0, 0, 1.42), .Dim = c(3L, 
									3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
											"ETA3")))),
			msg = " |omegas improted correctly")

	allThetaStderr <- sapply(methResults, function(x) x$StandardError$THETA)
	allSigmaStderr <- lapply(methResults, function(x) x$StandardError$SIGMA)
	allOmegaStderr <- lapply(methResults, function(x) x$StandardError$OMEGA)
	
	checkTrue(is.null(allThetaStderr[[2]]) & is.null(allSigmaStderr[[2]])
			 & is.null(allOmegaStderr[[2]]))
	checkEquals(allSigmaStderr[[1]], structure(0.000746, .Dim = c(1L, 1L), .Dimnames = list("EPS1", 
							"EPS1")))
	checkEquals(allOmegaStderr[[1]], structure(c(0.0426, 0, 0, 0, 0.0483, 0, 0, 0, 0.201), .Dim = c(3L, 
							3L), .Dimnames = list(c("ETA1", "ETA2", "ETA3"), c("ETA1", "ETA2", 
									"ETA3"))))
	# check dimensions of correlation and covariance matrices
	checkEquals(dim(methResults[[1]]$CorrelationMatrix), c(10, 10))
	checkEquals(dim(methResults[[1]]$CovarianceMatrix), c(10, 10))
	checkEquals(dim(methResults[[1]]$InverseCovarianceMatrix), c(10, 10))
	
	# check shrink values

	checkEquals( methResults[[1]]$ETAshrink, c(5.0139, 12.1730, 14.9980 ), msg = " |ETA shrink correct for method 1")
	checkEquals( methResults[[2]]$ETAshrink, c(0.072367, 6.459000, 5.190400), msg = " |ETA shrink correct for method 2")
	
	checkEquals( methResults[[2]]$EPSshrink, -10.145, msg = " |EPS shrink correct for method 2" )
	checkEquals( methResults[[1]]$EPSshrink, 12.013, msg = " |EPS shrink correct for method 1" )

}
