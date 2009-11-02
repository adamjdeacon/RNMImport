\name{applyDataSubset}
\alias{applyDataSubset}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
applyDataSubset(obj, sub = NULL)
}

\arguments{
  \item{obj}{ ~~Describe \code{obj} here~~ }
  \item{sub}{ ~~Describe \code{sub} here~~ }
}

\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (obj, sub = NULL) 
{
    assertClass(obj, "data.frame")
    if (is.null(sub)) 
        return(obj)
    assertClass(sub, "character")
    for (x in sub) {
        res <- try(subset(obj, eval(parse(text = x))), silent = TRUE)
        if (!inherits(res, "try-error")) 
            obj <- res
        else {
            RNMImportWarning(paste("Unable to apply subset:", 
                x, "\n"), call = match.call())
        }
    }
    return(obj)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
