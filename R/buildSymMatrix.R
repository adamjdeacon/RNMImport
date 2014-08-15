



.buildSymMatrix <- function(vec)
{
	
	###############################################################################
	N <- round((sqrt(8 * length(vec) + 1) - 1)/2)
	myMat <- matrix(1:(N^2), ncol = N, byrow = T)
	myMat <- replace(myMat, !lower.tri(myMat), vec)
	.buildSymMatrix.R.returned <- replace(myMat, lower.tri(myMat, T), t(myMat)[lower.tri(myMat, T)])
	.buildSymMatrix.R.returned
	###############################################################################
}
