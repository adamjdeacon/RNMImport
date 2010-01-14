# $LastChangeDate: $
# $LastChangedBy: fgochez $
# $Rev: 13953 $
# 
# Author: fgochez
# Tests importNmMod
# Note that most of the functions that importNmMod makes use of have their own unit tests, so there is no need to 
# test this routine very thoroughly
###############################################################################


test.importNmMod <- function()
{
	testDir <- file.path(unitTestPath, "testdata")
	dat1 <- importNmMod(file.path(testDir, "control3.con"))
	checkEquals(length(dat1$Raw), 16, msg = "Length of control3\n" )
	# only one problem
	checkEquals(length(dat1$problemContents), 1)
	x <- dat1$problemContents[[1]]
	checkEquals(names(x), c("Theta", "Problem", "Tables", "Subroutine", "Input", "Data",
							"PK", "Error", "Estimates", "Cov") )
	
	dat2 <- importNmMod(file.path(testDir, "control4.con"))
	checkEquals(rownames(dat2$problemContents[[1]]$Theta), c("A", "B", "C"), msg = "Parameter labels read correctly")
	
	dat3 <- importNmMod(file.path(testDir, "multiprob1.mod"))
	
	checkEquals(length(dat3$problemContents), 2)
	x <- dat3$problemContents
	checkEquals(names(x[[1]]), c("Problem","Subroutine", "Input", "Data", "PRED"   ))
	checkEquals(names(x[[2]]), c("Theta",  "Omega", "Sigma", "Problem", "Input","Data","Sim")) 
	# now try an example with simulation statements
	dat4 <- importNmMod(file.path(testDir, "subprob1.mod"))
	
	
	checkEquals(sum(dat4$Comments == ""), 59)
	checkEquals(dat4$Comments[6], " initialize" )
	checkEquals(dat4$Comments[43], " $SUPER SCOPE=3 ITERATIONS=10")
	
	x <- dat4$problemContents
	checkEquals(length(x), 4)
	
	checkEquals(x[[2]]$Theta[2,], c(.025,.102,.4), checkNames = FALSE )
	checkEquals(diag(x[[2]]$Omega), c(.04, .04, .04), checkNames = FALSE)
	# ensure that THETAs appear in the first 3 problems
	checkTrue(all(sapply(x[-4], function(y) "Theta" %in% names(y)) ))
	expectedSim <- c("1000", "5566898", "-1", "INITIAL", "TRUE")
	names(expectedSim) <- c("nSub", "Seed1", "Seed2", "TRUE", "simOnly" )
	#nSub     Seed1     Seed2      TRUE   simOnly 
	#"1000" "5566898"      "-1" "INITIAL"    "TRUE" 
	
	checkEquals(x[[1]]$Sim, expectedSim )

}

# tests NONMEM 7-specific functionality

test.importNmModNM7 <- function( )
{
	# a NONMEM 7 file that is largely similar to NONMEM 6, but with various EST statements
	testDir <- file.path(unitTestPath, "testdata")
	mod1 <- importNmMod("wexample1.ctl", path = testDir, version = "VII")
	prob1 <- mod1$problemContents[[1]]
	
	checkTrue( all(prob1$Theta[,"Est"] == 2), msg = " |Theta initial loaded correctly " )
	checkTrue( all(prob1$Theta[,"Lower"] == 0.001), msg = " | Theta lower bounds loaded correctly" )
	
	checkTrue( all(prob1$Omega == cbind(c(0.4, 0.001, 0.001, 0.001), c(0.001, 0.4, 0.001, 0.001), 
							c(0.001, 0.001, 0.4, 0.001), c(0.001, 0.001, 0.001, 0.4))), " |Omega initial imported correctly" )
	
	checkTrue( all( prob1$Sigma == 0.6 ) )
	estMatrix1 <- cbind("method" = c("ITS", "SAEM", "IMP", "COND"), 
						"file" = c("wexample1.EXT", "", "", "wexample1.EXT"),
						"remainingText" = 
								c(" INTERACTION CTYPE=3 NITER=1000 PRINT=5 NOABORT MSFO=wexample1.MSF NSIG=3 SIGL=6",
							" NBURN=2000 NITER=500 PRINT=100",
							" EONLY=1 NITER=5 ISAMPLE=3000 PRINT=1",
							" INTERACTION MAXEVAL=9999 NSIG=2 SIGL=10 PRINT=5 ")
				)
	checkEquals(prob1$Estimates, 
			estMatrix1, " | $Est statement parsed correctly" )
	# PRIOR with NWPRI should cause exception (issue 1811)
	tryImport <- try(importNmMod("wexample8.ctl", version = "VII", path = testDir), silent = TRUE)
	checkTrue(length( grep(tryImport, pattern = "\\$PRIOR NWPRI statement detected\\.  Importing of this is not supported") ) > 0,
	msg = " |Presence of $PRIOR NWPRI raises exception with adequate error message"   )
	#			msg = " |Presence of $PRIOR NWPRI raises exception with adequate error message" )
	
}