# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################


test.importModelOutputTables <- function()
{
	# we want to test that APPEND is handled correctly.  In this case, 
	
	tableStatement <-  "$TABLE  ONEHEADER ID STUD WEEK NITE DRUG DOSE A B C D
  	ETA(1) ETA(2) MDV WRES IWRES IPRED NOPRINT FILE=testtab.tab"
  	
  	tableInfo <- RNMImport:::.importNmModTables(tableStatement)
  	tableTest <- RNMImport:::importModelOutputTables(tableInfo, path = file.path(unitTestPath, "testdata"))
  	
  	checkEquals(as.matrix(tableTest), structure(c(11001, 11001, 11001, 11001, 11001, 3, 3, 3, 3, 3, 
		1, 1, 2, 2, 3, 1, 2, 1, 2, 1, 0, 0, 1, 1, 0, 0, 0, 10, 10, 25, 
		32.436, 32.436, 32.436, 32.436, 32.436, 1, 1, 1, 1, 1, 68.935, 
		68.935, 68.935, 68.935, 68.935, 0.29034, 0.29034, 0.29034, 0.29034, 
		0.29034, -0.68686, -0.68686, -0.68686, -0.68686, -0.68686, 0.91745, 
		0.91745, 0.91745, 0.91745, 0.91745, 0, 0, 0, 0, 0, -20.436, -6.9358, 
		-11.018, 6.4817, 15.197, 32.436, 32.436, 23.018, 23.018, 23.803, 
		12, 25.5, 12, 29.5, 39, 64.465, 64.465, 45.748, 45.748, 33.792, 
		-52.465, -38.965, -33.748, -16.248, 5.208, -0.84774, -0.48733, 
		-0.66465, -0.0062969, 0.51131, 0.84774, 0.48733, 0.66465, 0.0062969, 
		0.51131), .Dim = c(5L, 20L), .Dimnames = list(c("1", "2", "3", 
		"4", "5"), c("ID", "STUD", "WEEK", "NITE", "DRUG", "DOSE", "A", 
		"B", "C", "D", "ETA.1.", "ETA.2.", "MDV", "IWRES", "IPRED", "DV", 
		"PRED", "RES", "WRES", "absWRES"))), msg = " |table output captured correctly when APPEND is TRUE but an appended column is repeated")
  	
}
