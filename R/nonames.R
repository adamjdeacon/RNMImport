

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

