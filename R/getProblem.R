# $Rev$
# $LastChangedDate$


#' Retrieves a desired problem from a full NONMEM run object.
#' @title Extract an individual NONMEM problem
#' @param run An NMRun class object
#' @param problemNumber  Number of the problem to retrieve
#' @return The given problem number (NMBasicModel, NMSimDataGen, or NMSimModel)
#' @author fgochez
#' @keywords manip
#' @export

getProblem <- function(run, problemNumber = 1)
{
	assertClass(run, "NMRun")
	if(problemNumber < 1 | problemNumber > run@numProblems)
	{
		msg <- problemNumber %pst% " is not a valid problem number\n"
		RNMImportWarning(msg, call = match.call())
		return(NULL)
		
	}
	run@problems[[problemNumber]]
}
