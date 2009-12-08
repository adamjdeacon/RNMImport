# $LastChangedDate$
# $LastChangedBy$
# $Rev$
# 
# Author: fgochez
###############################################################################

# tests the "processPath" utility function

test.processPath.R <- function()
{
	processPath <- RNMImport:::processPath
	setNmPath("foobarpath", "D:/Temp")
	
	checkEquals( processPath("(foobarpath)"), "D:/Temp" )
	checkEquals( processPath("   (foobarpath)     "), "D:/Temp" )
	checkEquals( processPath("(foobarpath)     "), "D:/Temp" )
	checkEquals( processPath("   (foobarpath)"), "D:/Temp" )
	checkEquals(processPath("D:/Temp"), "D:/Temp")
	
	# the following checks specifically address mantis issue 1868
	# brackets should be allowed in path names, so long as they don't surround the whole path
	checkEquals(processPath("D:/T(em)p"), "D:/T(em)p" )
	
	checkEquals(processPath("D:/T(emp)"), "D:/T(emp)" )
	checkEquals(processPath("(D:/Te)mp"), "(D:/Te)mp" )
	
	
}