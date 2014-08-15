

# removes the method name attribute from an object.  Meant to facilitate certain importNm tests

.removeMethName <- function(x)
{
	attr(x, "methodName") <- NULL
	x
	
}
