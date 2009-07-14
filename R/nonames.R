
### mainly used for testing, not exported
### removes names of an object
# Originally written by R. Francois
# Added: Jan 7 2009


nonames <- function(x){
  UseMethod("nonames")
}

nonames.default <- function(x){
  names(x) <- NULL
  attributes(x) <- NULL
  x
}

nonames.array <- nonames.matrix <- function(x){
  dimnames(x) <- NULL
  x
}

