
test.logging <- function()
{
	checkEquals(availableLogs(), c("stdReport", "detailedReport", "warnings", "highLevelParse", "lowLevelParse"))
	checkException(setLogConnection("fakeLog", NULL), msg = "Testing error-handling")
	setLogFile("stdReport", file.path(unitTestPath, "testdata/testlog.txt"))
	logMessage("test", "stdReport")
	checkEquals("test", scan(file.path(unitTestPath, "testdata/testlog.txt"), what = ""))
	closeLogConnection("stdReport")
	checkTrue(is.na(logConnection("stdReport")))
	setLogConnection("stdReport", stdout())
	file.remove(file.path(unitTestPath, "testdata/testlog.txt"))
}
