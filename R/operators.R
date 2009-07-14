## 
## # is there at least one element of x that matches the regex rx
## `%~%` <- function(x, rx){
##   regexpr(rx, x)  > 0 
## }
## `%a~%` <- function(...) any( `%~%`(...) )
## `%!a~%` <- function(...) !`%a~%`(...)
## # TRUE if no element of x matches the regex rx
## `%!~%` <- function(x, rx){
##   any( regexpr(rx, x) == -1 )
## }
## 
## `%~^%` <- function( x, rx ){
##   `%~%`( x, sprintf("^%s", rx))
## }
## `%a~^%`  <- function(...) any( `%~^%`(...) )
## `%!a~^%` <- function(...) !`%a~^%`(...)
## `%!~^%` <- function(x, rx){
##   any( regexpr(sprintf("^%s", rx), x) == -1 )
## }
## 
## `%~$%` <- function( x, rx ){
##   `%~%`( x, sprintf("%s$", rx))
## }
## `%a~$%`  <- function(...) any( `%~$%`(...) )
## `%!a~$%` <- function(...) !`%a~$%`(...)
## `%!~$%` <- function(x, rx){
##   any( regexpr(sprintf("%s$", rx), x) == -1 )
## }
## 
## 
## 
## # negation of %in%
## `%!in%`    <- function(...) !`%in%`(...)
## 
## # wrap all around %in%
## `%allin%`  <- function(...) all( `%in%`(...) )
## `%!allin%` <- function(...) !all( `%in%`(...) )
## 
## # wrap any around %in%
## `%anyin%`  <- function(...) any( `%in%`(...) )
## `%!anyin%` <- function(...) !any( `%in%`(...) )
## 
## `%in~%` <- function(x, y){
##   x %in% strsplit(y, '')[[1]]
## }
## `%!in~%`<- function(...) !`%in~%`(...)
## `%allin~%` <- function(...) all( `%in~%`(...) )
## `%!allin~%` <- function(...) !all( `%in~%`(...) )
## 
## ## operator to check that two objects have the same length
## `%l%` <- function(x,y){ 
##   length(x) == length(y) 
## }
## `%!l%` <- function(...) !`%l%`(...) 
## 

## 
## # .. with a newline
## `%.n%` <- function(s1, s2) {
##   paste(s1, s2, sep = "\n", collapse = "\n") 
## }
## # .. with a new line and a tabulation
## `%.nt%` <- `%.tn%` <- function(s1, s2) {
##   paste(s1, s2, sep = "\n\t") 
## }
## 
## # .. with a space
## `%.s%` <- function(s1, s2) {
##   paste(s1, s2, sep = " ", collapse = "\n")                       
## }
## 
## # .. remove a pattern from a string
## `%-~%` <- function(txt, rx){
##   gsub( rx, '', txt )                
## }     
## 
## # .. split a string by a regex
## `%/~%` <- function( txt, rx ){
##   out <- unlist( strsplit( txt, rx) )
##   out[out!= ""]
## }
## 
## # is that oject of that class
## `%of%` <- function(e1, e2){
##   inherits( e1, e2 ) 
## }
## 
## `%!of%` <- function(...) !`%of%`(...) 
## 
## # assign value to the value of name
## `%<-%` <- function(name, value){
##   assign( name, value, parent.frame() )
## }

##################################################################
# %wo%
# operator that performs set difference
# Author: F. Gochez
# Added: Jan 7 2009
# Last modified: Jan 7 2009
# TODO: deprecate in favor of setdiff
##################################################################

`%without%` <- `%wo%` <- `%w/o%` <- function(x, y){
  x[!x %in% y ]
}

## # string concatenation operator
`%pst%` <- function(s1, s2) {
	paste(s1, s2, sep = "") 
}                                  


## 
## # perl-like statement modifiers
## `%if%` <- function( expr, condition ){
##   if( condition ) eval(expr, parent.frame() )
## }
## `%unless%` <- function( expr, condition ){
##   if( !condition ) eval(expr, parent.frame() )
## }
## 
## `%mustbe%` <- function( e1, e2 ){
##     assign( deparse(substitute(e1)), if( e1 %of% e2) e1 , parent.frame() )
## }
## 
## 
