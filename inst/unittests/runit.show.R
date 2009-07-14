# 
# Author: fgochez
###############################################################################

test.show.NMRun <- function()
{
	
	setNmPath("internalunit",  file.path(unitTestPath, "testdata/TestRun") )
	run1 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
			path = "(internalunit)")
	# sink output to a file for comparison
	
	sink(file.path(unitTestPath, "showtest1.txt"))
	show(run1)
	sink(NULL)
	test1 <- scan(file.path(unitTestPath, "showtest1.txt"), what = character(0))
	checkEquals(length(test1), 47)
	test2 <- scan(file.path(unitTestPath, "showtest1.txt"), what = character(0), sep = "\n")
	checkEquals(test2[1], "Control file: " )
	checkEquals(test2[4], "Output report file: "  )
	file.remove(file.path(unitTestPath, "showtest1.txt"))
	

}

test.show.NMBasicRun <- function()
{
	
	run1 <- importNm(conFile = "testdata1.ctl", reportFile = "testdata1.lst", 
			path =  file.path(unitTestPath, "testdata/TestRun"))
	prob1 <- getProblem(run1)
	test1 <- capture.output(show(prob1))
	checkEquals(length(test1), 32)
	checkEquals(test1[c(1, 3, 8)], 
			c("Standard NONMEM problem:", 
			 "Problem statement:  System Test 1 ", 
			 "ID TIME IPRED IWRES DV PRED RES WRES CL V KA AGE HT WT SECR SEX RACE SMOK HCTZ PROP CON OCC SID absWRES "))
	
}

test.show.NMSimModel <- function()
{
	prob <- getProblem(importNm(conFile = "testdata1sim.con", reportFile = "testdata1sim.lst", 
			path =  file.path(unitTestPath, "testdata/TestSimRun")))
	test1 <- capture.output(show(prob))
	testLines <- c(1, 3, 4, 14, 17:19, 91)
	checkEquals(length(test1), 92)
	expectedLines <- c(
			"NONMEM data simulation problem:" ,
			"Problem statement:  System Test 2 ",                                                                                                                             
			"PK: ",                                                                                                                                                                     
			"5305 24 ",
			"Number of simulations performed:  5 ",
			"Seeds:  20050213 ",
			"Final estimates for each subproblem:",
			"    sim1     sim2     sim3     sim4     sim5 "
			)
	checkEquals(test1[testLines], expectedLines, "checking expected lines" )
	
}