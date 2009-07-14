###############################################
# test "assertClass"
###############################################

test.assertClass <- function()
{
	assertClass <- RNMImport:::assertClass
	checkException(assertClass(1:10, "character"))
	res <- try(assertClass(0.01, "numeric"), silent = TRUE)
	checkTrue(!inherits(res, "try-error"))
}

test.RNMImportStop <- function()
{
	tempOut <- tempfile()
	checkException(RNMImportStop("Exception correctly generated:"), match.call())
	
}

test.RNMImportWarning <- function()
{
	
}