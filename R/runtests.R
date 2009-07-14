# $Rev$
# $LastChangedDate$


#' Executes the package regression tests
#' @title run package regression tests
#' @param testDataPaths Paths to locate the data for the suites.  Up to 4 may be specified, but the first one should
#' be located on the user's computer  
#' @param testScriptPath Path where the scripts are located.  By default, will use the package installation directory
#' @param runExtern Logical flag.  Should the external tests be run?
#' @param printTestProtocol Logical flag.  Should HTML reports be produced?
#' @return The results of running the test suite, as run by RUnit's "runTestSuite".  This will be a list of up to 4 elements
#' @author Mango Solutions
#' @keywords debugging
#' @export 

# Individual author: Francisco Gochez

runRNMImportTests <- function(
		testDataPaths = c(system.file(package="RNMImport", "unittests"), 
				"\\\\Mango-data1\\mangowork\\MangoProjects\\RNONMEM2\\data"), 
		testScriptPaths = c(system.file(package="RNMImport", "unittests"), "ExternalTests/RNMImport"),
		runExtern = FALSE,
		printTestProtocol = TRUE,
		cleanup = TRUE
)
{
	cat("Redirecting stdout to testlog.txt\n")
	sink("testlog.txt")
	stopifnot(require("RUnit", quietly = TRUE))
	results <- list()

	#logDir <- setTestLogs()
	# allocated environment for use by tests
	.innerTestEnv <<- new.env()
	# the first set of tests depends on the following global path:
	unitTestPath <<- testDataPaths[1]
	# first, run the unit tests which are 
	testSuite <- defineTestSuite("Internal unit test suite", dirs = testScriptPaths[1],
			testFileRegexp = "^runit\\..+\\.[rR]$")
	res <- runTestSuite(testSuite)
	
	if(printTestProtocol)
		printHTMLProtocol(res, fileName = "RNMImport_internalunit.html" )
	
	results[["Internal unit"]] <- res
	
	# now run some test suites dependant on external data
	"%pst%" <- RNMImport:::"%pst%"
	
	if(runExtern)
	{
		.externTestEnv <<- new.env()
		# grab all of the test data at once
		assign("testRuns",externalTestRuns(testDataPaths[2]) , envir = .externTestEnv)
		
		if(is.na(testDataPaths[2]) || !file.exists(testDataPaths[2]))
		{
			RNMImportWarning("Unable to execute test suite since " %pst% testDataPaths[2] %pst% " does not exist\n",
					call = match.call())
			return(results)
		}
		setNmPath("testpath1", testDataPaths[2])
		testSuite <- defineTestSuite("External unit test suite", dirs = testScriptPaths[2], 
				testFileRegexp = "^runitextern1.+\\.[rR]$")
		res <- runTestSuite(testSuite)
		if(printTestProtocol)
			printHTMLProtocol(res, fileName = "RNMImport_externalunit1.html" )
		results[["External unit"]] <- res
		
		testSuite <- defineTestSuite("External system test suite", dirs = testScriptPaths[2], 
				testFileRegexp = "^rsys.+\\.[rR]$")
		res <- runTestSuite(testSuite)
		if(printTestProtocol)
			printHTMLProtocol(res, fileName = "RNMImport_externalsystem.html" )
		results[["External system"]] <- res
		# finally, system
		
		if(cleanup)
		{
			rm(.externTestEnv, envir = .GlobalEnv)
			removeNmPath("testpath1")
		}
	}
	if(cleanup) 
	rm(.innerTestEnv, envir = .GlobalEnv)
	# closeTestLogs()
	sink(NULL)
	results

}

#' 
#' @title
#' @return 
#' @author fgochez
#' @keywords

externalTestRuns <- function(dataPath)
{
	paths <- file.path(dataPath, paste("importNONMEMFiles/TestData", c(1:10, 5, 11, 12), sep = ""))
	y <- getNmFileExtensions("report")
	setNmFileExtensions("report", c(y, "LST" ))
	conFiles <- paste("TestData", 1:10, ".ctl" , sep = "")
	conFiles <- c(conFiles, "TestData5SIM.ctl", "psn-1.mod", "psn-1.mod")
	res <- lapply(seq_along(conFiles), function(i) try(importNm(conFiles[i], path = paths[i])))
	setNmFileExtensions("report", y)
	res
}

setTestLogs <- function()
{
	logFiles <- paste(availableLogs(), ".log", sep = "")
	logDir <- tempdir()
	lapply(seq_along(availableLogs()), function(i) setLogFile( log = availableLogs()[i], fileName = file.path(logDir, logFiles)))
	logDir
}

closeTestLogs <- function()
{
	lapply(availableLogs(), closeLogConnection)
	RNMImport:::initializeLogs()
}