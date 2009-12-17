# TODO: make these tests more comprehensive!!
# Note: a great deal of the functionality of importNmReport is covered by other unit tests, such
# as importNm, so these tests will not be extensive


test.importNmReport.Basic <- function()
{
	report1 <- importNmReport("TestData1.lst", path = file.path(unitTestPath, "testdata/TestRun"))
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

# test import of a basic NONMEM 7 report

test.importNmReport.BasicNM7 <- function()
{
	# load report1, which is from wexample1.lst
	report1 <- importNmReport("wexample1.lst", 
			path = file.path(unitTestPath, "testdata/wexample1"))
	# TODO: check version...
	probResult <- report1$problemResults[[1]]
	checkEquals(probResult$nRecords, 500, " |Number of records is correct")
	checkEquals(probResult$nIndividuals, 100, " |Number of individuals is correct")
	
	methodResults <- probResult$MethodResults
	
	objectiveFunValues <- sapply( methodResults, function(x) x$Objective.Final )
	checkEquals( objectiveFunValues, -c(1120.307, 2477.443, 1145.163, 1121.028), msg = " |Objective function values retrieved correctly (check 1)" )
	
	# test 2, uses wexample2.lst
	
	report2 <- importNmReport("wexample2.lst", path = file.path(unitTestPath, "testdata/wexample2" ))
	methodResults2 <- report2$problemResults[[1]]$MethodResults
	objectiveFunValues2 <- sapply( methodResults2, function(x) x$Objective.Final)
	checkEquals( objectiveFunValues2, c( -19576.074, -10779.024 ,  -10772.143  ),  msg = " |Objective function values retrieved correctly (check 2)" )
	
	# test 3, uses wexample6.lst
	
	report3 <- importNmReport("wexample6.lst", path = file.path(unitTestPath, "testdata/wexample6" ))
	methodResult3 <- report3$problemResults[[1]]$MethodResults
	objectiveFunValues3 <- sapply( methodResult3, function(x) x$Objective.Final)
	
	checkEquals( objectiveFunValues3, c(-4708.871, -4710.416 ), msg = " |Objective function values retrieved correctly (check 3)" )
	
	### NOW TEST THETAS
	
	thetas1 <- lapply(methodResults, function(x) x$FinalEstimates$THETA)
	
	checkEquals( unname(unlist(thetas1)), 
			c(1.680, 1.590, 0.813, 2.370, 1.630, 1.560, 0.767, 2.350, 1.630, 1.560, 0.767, 2.350, 1.690, 1.610,0.819,2.390),
			msg = " |Thetas imported correctly from wexample1.lst")
	
	thetas2 <- lapply(methodResults2, function(x) x$FinalEstimates$THETA)
	
	checkEquals(unname(unlist(thetas2)), 
			c(3.3, 3.25, -0.612, -0.208, 0.733, 1.13, 0.335, 0.192, 0.69, 
					2.3, 0.0984, 3.3, 3.25, -0.612, -0.208, 0.733, 1.13, 0.335, 0.192, 
					0.69, 2.3, 0.0984, 3.31, 3.26, -0.612, -0.208, 0.735, 1.14, 0.336, 
					0.192, 0.695, 2.3, 0.1),
			msg = " |Thetas imported correctly from wexample2.lst")
	
	thetas3 <- lapply(methodResult3, function(x) x$FinalEstimates$THETA)
	checkEquals(unname(unlist(thetas3)), c(3.9, -2.17, 0.565, -0.184, 2.26, 0.207, 3.7, -0.71, 3.9, -2.22, 
					0.562, -0.18, 2.27, 0.233, 3.71, -0.701), msg = " |Thetas imported correctly from wexample6.lst")

	### OMEGAs
	
	omegas1 <- lapply(methodResults, function(x) x$FinalEstimates$OMEGA)
	
	checkEquals(omegas1, list(structure(c(0.165, 0.00413, 0.00538, -0.0161, 0.00413, 0.133, 
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
													"ETA4"), c("ETA1", "ETA2", "ETA3", "ETA4")))), msg = " |Omegas from wexample1.lst imported correctly" ) 
	
	omegas2 <- lapply( methodResults2, function(x) x$FinalEstimates$OMEGA )
	checkEquals(omegas2, 
			list(structure(c(0.0107, 0.000823, 0.00216, -0.000134, 0.000823, 
									0.0087, 0.00158, 0.00127, 0.00216, 0.00158, 0.0134, 0.00389, 
									-0.000134, 0.00127, 0.00389, 0.0106), .Dim = c(4L, 4L), .Dimnames = list(
									c("ETA1", "ETA2", "ETA3", "ETA4"), c("ETA1", "ETA2", "ETA3", 
											"ETA4"))), structure(c(0.0107, 0.000823, 0.00216, -0.000134, 
									0.000823, 0.0087, 0.00158, 0.00127, 0.00216, 0.00158, 0.0134, 
									0.00389, -0.000134, 0.00127, 0.00389, 0.0106), .Dim = c(4L, 4L
							), .Dimnames = list(c("ETA1", "ETA2", "ETA3", "ETA4"), c("ETA1", 
											"ETA2", "ETA3", "ETA4"))), structure(c(0.0103, 0.000193, 0.00129, 
									-0.000594, 0.000193, 0.00795, -0.000161, 0.000545, 0.00129, -0.000161, 
									0.01, 0.00201, -0.000594, 0.000545, 0.00201, 0.00967), .Dim = c(4L, 
									4L), .Dimnames = list(c("ETA1", "ETA2", "ETA3", "ETA4"), c("ETA1", 
											"ETA2", "ETA3", "ETA4")))),
			msg = " |omegas correct for wexample2.lst")
	
	# sigmas

	sigmas1 <- lapply(methodResults2, function(x) x$FinalEstimates$SIGMA)
	
	checkEquals(unlist(sigmas1), c(1,1,1), msg = " |sigmas correct for wexample2.lst")
	
	sigmas2 <- lapply(methodResult3, function(x) x$FinalEstimates$SIGMA)
	checkEquals(sigmas2, 
			list(structure(c(0.00903, 0, 0, 0.0218), .Dim = c(2L, 2L), .Dimnames = list(
									c("EPS1", "EPS2"), c("EPS1", "EPS2"))), structure(c(0.00892, 
									0, 0, 0.0215), .Dim = c(2L, 2L), .Dimnames = list(c("EPS1", "EPS2"
									), c("EPS1", "EPS2")))),
			msg = " |sigmas correct for wexample6.lst" )
	
	# standard errors test.  Extract method #3 of wexample2.lst
	
	stdErrorsMeth <- report2$problemResults[[1]]$MethodResults[[3]]
	
	# theta standard errors
	
	checkEquals( stdErrorsMeth$StandardError$THETA, 
			structure(c(0.0327, 0.0286, 0.00952, 0.00831, 0.0392, 0.0358, 
							0.0113, 0.0104, 0.0105, 0.00857, 0.00283), .Names = c("TH1", 
							"TH2", "TH3", "TH4", "TH5", "TH6", "TH7", "TH8", "TH9", "TH10", 
							"TH11")),
			msg = " |theta std errors correct")
	
	# sigma standard errors
	
	checkEquals( stdErrorsMeth$StandardError$SIGMA, 
			matrix(0, dimnames = list("EPS1", "EPS1")), msg = " |sigma standard errors correct")
	
	# omega standard errors
	
	checkEquals( stdErrorsMeth$StandardError$OMEGA, 
			structure(c(0.000968, 0.000828, 0.00128, 0.001, 0.000828, 0.00138, 
							0.00143, 0.0011, 0.00128, 0.00143, 0.00305, 0.00215, 0.001, 0.0011, 
							0.00215, 0.00193), .Dim = c(4L, 4L), .Dimnames = list(c("ETA1", 
									"ETA2", "ETA3", "ETA4"), c("ETA1", "ETA2", "ETA3", "ETA4")))
			, msg = " |omega std errors correct")
	# correlation and covariance matrix
	checkTrue( all( -1 <= stdErrorsMeth$CorrelationMatrix & stdErrorsMeth$CorrelationMatrix <= 1 ),
			msg = " |all correlation matrix entries between -1 and 1.")
	checkEquals( diag(stdErrorsMeth$CorrelationMatrix), 
			structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
							1, 1, 1, 1, 0), .Names = c("TH1", "TH2", "TH3", "TH4", "TH5", 
							"TH6", "TH7", "TH8", "TH9", "TH10", "TH11", "OM11", "OM12", "OM13", 
							"OM14", "OM22", "OM23", "OM24", "OM33", "OM34", "OM44", "SG11"
					))
	, 
			msg = " |Correlation matrix diagonal correct")
	
	
}
