# TODO: Add comment
# 
# Author: fgochez
###############################################################################

# TODO: unit tests

# TODO: make this a generic function later

"dataSubset<-" <- function(x, value)
{
	attr(x, "dataSubset") <- value
	x
}
# TODO: mantual entry
# TODO: take this out of RNMGraphics later

"graphSubset<-" <- function(x, value)
{
	attr(x, "graphSubset") <- value
	x
}

dataSubset <- function(x)
{
	attr(x, "dataSubset")
}

applyDataSubset <- function(obj, sub = NULL)
{
	if(is.null(sub))
		return(obj)
	
	for(x in sub)
	{
		res <- try(subset(obj, eval(parse(text = x)) ), silent = TRUE)
		if(!inherits(res, "try-error")) obj <- res
		
	}
	return(obj)	
}
