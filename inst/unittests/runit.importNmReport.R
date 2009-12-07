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