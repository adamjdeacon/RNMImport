
require(RNMImport)

# store a path.  This step is optional
setNmPath("runPath", system.file(package = "RNMImport", "examples/theoph"))

# Import the run by specifying control and list files.  Note the way that the path parameter is specified in order to 
# use the stored path
testRun <- importNm(conFile = "theoph.con", reportFile = "theoph.lst", path = "(runPath)")

# show the basic information
show(testRun)

# extract the only problem in this run

testModel <- getProblem(testRun)

# show basic information
show(testModel)
cat("\n")
# extract the parameters.  No standard errors are available for this run unfortunately
print(getThetas(testModel))
print(getOmegas(testModel))
print(getSigmas(testModel))
						

# we can also extract them from the full run object
print(getThetas(testRun, problemNum = 1))

# extract the iterations.  This must be done manually for now, but will be changed in the future
print(testModel@parameterIterations)

# now extract the data as a single data.frame

modelData <- nmData(testModel, returnMode = "singleDF")
print(head(modelData))

# now get a list of seperate input and output

modelData <- nmData(testModel, returnMode = "DFList")
print(head(modelData$input))
print(head(modelData$output))

# add a categorical variable
testModel <- addDerivedCategorical(testModel, varName = "IPRED", breaks = 4)
print(addedData(testModel))


removeNmPath("runPath")

