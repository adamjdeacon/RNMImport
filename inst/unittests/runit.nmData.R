
# Test nmData for an object of class "NMBasicModel" and "NMRun"
#

test.nmData.NMBasic <- function()
{
	run1 <- importNm("TestData1.ctl", "TestData1.lst", path = file.path(unitTestPath, "testdata/TestRun"))
	prob <- getProblem(run1)
	test1 <- nmData(run1, , applySubset = FALSE)
	checkEquals(test1, nmData(getProblem(run1), applySubset = FALSE), "Extracting from run and problem directly should give same result")
	test2 <- nmData(run1, dataType = "input")
	inputColumns <- c("SID", "SEX" ,"AGE" ,"RACE",  "HT",  "SMOK",  "HCTZ","PROP", "CON", "AMT", "WT" ,"TIME", "SECR", "DV", "EVID", "SS", "II" ,"ID", "OCC")
	outputColumns <- c("ID", "TIME", "IPRED", "IWRES", "CL", "V", "KA", "AGE", "HT", "WT", "SECR", "SEX", "RACE", 
				"SMOK", "HCTZ", "PROP", "CON", "OCC", "absWRES", "DV", "WRES", "PRED", "RES", "SID") 
	
	checkTrue(setequal(names(test2), inputColumns), "Checking that all input data is present")

	test3 <- nmData(run1, dataType = "output", , applySubset = FALSE)
	checkTrue(setequal(names(test3), outputColumns), "Checking that all output data is present")
	test4 <- nmData(prob, returnMode = "DFList", applySubset = FALSE)
	checkEquals(names(test4), c("input", "output"), "Check return list names")
	checkEquals(test2, test4$input)
	checkEquals(test3, test4$output)
	
	x <- union( inputColumns, outputColumns	)
	y <- intersect(inputColumns, outputColumns )
	checkTrue(setequal(names(test1), c(x, paste(y, ".INPUT", sep = ""))))

}

# test nmData for an object of class "NMSimDataGen" and "NMSimModel"

test.nmData.NMSim <- function()
{
	run1 <- importNm("TestData1SIM.con", "TestData1SIM.lst", path = file.path(unitTestPath, "testdata/TestSimRun"))
	prob <- getProblem(run1)
	# dataSubset(run1) <- NULL

	dataSubset(prob) <- NULL
	inputColumns <- c("SID", "SEX" ,"AGE" ,"RACE",  "HT",  "SMOK",  "HCTZ","PROP", "CON", "AMT", "WT" ,"TIME", "SECR", "DV", "EVID", "SS", "II" ,"ID", "OCC")
	outputColumns <- c("ID", "TIME", "IPRED", "IWRES", "CL", "V", "KA", "AGE", "HT", "WT", "SECR", "SEX", "RACE", 
			"SMOK", "HCTZ", "PROP", "CON", "OCC", "absWRES", "DV", "WRES", "PRED", "RES", "SID") 
	
	test1 <- nmData(prob, dataType = "input")
	checkEquals(nmData(run1, subProblemNum = 2 ), nmData(getProblem(run1), subProblemNum = 2, applySubset = FALSE), 
			"Extracting from run and problem directly should give same result")
	test2 <- nmData(prob, dataType = "output")
	checkTrue(setequal(names(test1), inputColumns), msg = "Checking presence of input data")
	checkTrue(setequal(names(test2), c(outputColumns, "NSIM")), msg = "Checking presence of output data")
	 
	test3 <- nmData(prob, subProblemNum = 2:3, returnMode = "DFList") 
	checkEquals(nrow(test3[["input"]]) * 2, nrow(test3[["output"]]), msg = "Number of rows of output is correct" )
	
	checkEqualsNumeric(as.numeric(test3[["output"]]$NSIM), c(rep(2, 1061 ), rep(3, 1061)), msg= "NSIM set correctly" )
	
	test4 <- nmData(prob, subProblemNum = 5)
	x <- union( inputColumns, outputColumns	)
	y <- intersect(inputColumns, outputColumns )
	
	checkTrue(setequal(names(test4), c(x, "NSIM", paste(y, ".INPUT", sep = ""))), msg = "correct columns present")
	checkTrue(any(test4$DV != test4$"DV.INPUT"), msg = "simulated DV not equiavlent to input DV")
	
	test5 <- nmData(prob, subProblemNum = 1:2)
	checkEquals(nrow(test5), 2 * 1061, msg = " result has the correct number of rows")
	checkEquals(test5$NSIM, factor(c(rep(1, 1061), rep(2, 1061)), levels = as.character(1:5), ordered = TRUE))
	
}

test.nmDatabyVarType <- function()
{
	run1 <- importNm("TestData1.ctl", "TestData1.lst", path = file.path(unitTestPath, "testdata/TestRun"))
	test1 <- nmDatabyVarType(run1, varTypes = "Covariate")
	checkEquals(dim(test1), c(1061, 4), "dimensions of the data are correct")
	checkTrue(setequal(names(test1), c("AGE", "RACE", "SEX", "SMOK")))
	test2 <- nmDatabyVarType(run1, varTypes = "Parameter")
	checkEquals(nrow(test2), 1061)
	checkTrue(setequal(names(test2), c("CL", "KA", "V")), msg = "correct variables selected")
	test3 <- nmDatabyVarType(run1, varTypes = "Eta")
	test4 <- nmDatabyVarType(run1, varTypes = "Lab covariate")
	checkEquals(c(ncol(test3), ncol(test4) ), c(0, 0), "no etas or lab covariates")
	test5 <- nmDatabyVarType(run1, varTypes = c("Covariate", "Parameter"))
	checkEquals(test5, cbind(test1, test2))
	test6 <- nmDatabyVarType(run1, varTypes = c("Covariate", "Parameter"), returnMode = "DFList")
	checkEquals(test6[[1]], test1)
	checkEquals(test6[[2]], test2)
	
	run2 <- importNm("TestData1SIM.con", "TestData1SIM.lst", path = file.path(unitTestPath, "testdata/TestSimRun"))
	
	test7 <- nmDatabyVarType(run2, varTypes = "Covariate", subProblemNum  = 2)
	test7.2 <- test7
	rownames(test7.2) <- as.character(seq_len(nrow(test7)))
	checkEquals(test7.2, test1, "simulated and non-simulated data equivalent for this cast")
}