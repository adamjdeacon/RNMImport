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
}
