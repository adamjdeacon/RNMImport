###################################################
### chunk number 1: 
###################################################
#line 28 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
require(RNMImport)


###################################################
### chunk number 2: 
###################################################
#line 33 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
# Import an example run
runPath <- system.file(package = "RNMImport", "unittests/testdata/TestRun")
# List file deduced automatically
run <- importNm(conFile = "TestData1.ctl", path = runPath)
print(run)
print(class(run))


###################################################
### chunk number 3: 
###################################################
#line 64 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
prob <- getProblem(run)
print(prob)


###################################################
### chunk number 4: 
###################################################
#line 77 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
runPath <- system.file(package = "RNMImport", "unittests/testdata/TestSimRun")
# List file deduced automatically
simRun <- importNm(conFile = "TestData1SIM.con", path = runPath)
simProblem <- getProblem(simRun)
print(simProblem)


###################################################
### chunk number 5: 
###################################################
#line 98 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
print(getThetas(prob))
print(getOmegas(prob))
print(getThetas(simProblem))
print(getOmegas(simProblem))


###################################################
### chunk number 6: 
###################################################
#line 110 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
print(getThetas(run, problemNum = 1))
print(getOmegas(prob, problemNum = 1))



###################################################
### chunk number 7: 
###################################################
#line 130 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
probOutData <- nmData(prob, dataTypes = "output")
print(head(probOutData))
probData <- nmData(prob)
print(head(probData))


###################################################
### chunk number 8: 
###################################################
#line 140 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
simOutData <- nmData(simRun, dataTypes = "output", subProblemNum = 2:3, problemNum = 1)
print(dim(simOutData))


###################################################
### chunk number 9: 
###################################################
#line 148 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
x <- nmDatabyVarType(run, varTypes = "Parameter,Covariate", problemNum = 1 )
print(head(x))


###################################################
### chunk number 10: 
###################################################
#line 154 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
prob <- getProblem(run)
prob <- addDerivedCategorical(prob, "RES", "RES.CUT", breaks = 3, labels = c("low", "medium", "high"))
print(head(addedData(prob)))


###################################################
### chunk number 11: 
###################################################
#line 163 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
print(runPath)
setNmPath("runPath", runPath)
# note the use of round brackets
controlContents <- importNmMod("TestData1SIM.con", path = "(runPath)" )
print(head(controlContents))
removeNmPath("runPath")


###################################################
### chunk number 12: 
###################################################
#line 175 "c:/svn/RNONMEM2/importing/RNMImport/inst/doc/RNMImport.Rnw"
print(getVarDescription(c("SEX", "SMOK")))
setVarDescription("SMOK", "Smokes", varFormat = "0=NO, 1 = YES", varType = "Covariate")
prob <- imposeCategoryFormat(prob, varSubset = "SMOK")
nmData(prob)[,"SMOK"]


