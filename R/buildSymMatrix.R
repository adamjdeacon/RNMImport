
##################################################################
# .buildSymMatrix
# Builds a symmetric matrix whose lower triangle is some vector
# Author: J James, modifications by F. Gochez
# Added: Jan 6 2009
# Last modified: Jan 6 2009
# parameters :
# @ vec - A vector which is turned into the triangle of the symmetric matrix
# Returns: A symmetric matrix whose lower triangle is the given vector  
##################################################################


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
