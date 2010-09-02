
.extractInformation <- function( x, guessNames = TRUE, component)
{
	startAt <- 1
	rx = .getPattern(paste(tolower(component),'s',sep=''))
	# extract comments	
	theLines <- x
	comments <- commentPop( x, inPlace = TRUE )
	comments <- stripBlanks( comments)
	# check for the presence of "FIXED"
	fixedOMEGAS <- fixed <- logicalPop( x, "FIXE?D?", inPlace = TRUE) 
	
	if( !is.null( comments) && guessNames )
	{
		guess <- ogrep( rx, comments, filter = "\\1")
		guess <- negGrep( "^[[:digit:]]", guess, value = TRUE ) # name should not start with a digit
	}
	
	if(logicalPop( x, "SAME", inPlace = TRUE) ){
		### SAME STYLE                                                              
		out <- matrix("SAME", 1, 1)
		if( !is.null( comments) && guessNames )
		{
			if( length(guess) == nrow(out) ){
				dimnames(out) <- rep(list(guess), 2)
				attr(out, 'comments') <- guess
			}
		}
	} else{ 
		### BLOCK style, indicates a block diagonal specification                                                             
		# retrieve the number of blocks present
		nBlocks <- equalExpressionPop( x, "BLOCK", sep = "[[:space:]]*", removeBrackets = TRUE, 
				absent =  NULL, inPlace = TRUE)
		sortIt <- lapply(.readValues(x),function(X)tryCatch(as.numeric(X), 
							warning=function(e){NA}, 
							error = function(e){NA})
		)
		dropGuess <- which(sapply(sortIt, is.na))
		if(length(dropGuess)>0){
			guess <- guess[-dropGuess]
			sortIt <- sortIt[-dropGuess]
		}
		surplus <- regexpr('[0-9.+-E]', x)
		if(length(which(surplus<0))>0){
			guess <- guess[-which(surplus<0)]
		}
		sortIt <- unlist(sortIt)
		if(length(guess) > length(sortIt)){
			guess <- guess[1:length(sortIt)]
		}
		if( !is.null(nBlocks)){
			
			if(length(guess) < length(sortIt)) {
				guess <- c(guess, rep(' ', length(sortIt) - length(guess)))
			}
			out <- try( .buildSymMatrix( sortIt )) 
			if( !is.null( comments) && guessNames )
			{
				dim1 <- dim(out)[1]
				# looking for 
				half <- dim1*(dim1 + 1)/2
				# 2 possibilities: either we have counted the BLOCK as a ''	
				if(length(guess) == half + 1)
					guess <- guess[-1]
				if(length(guess) == half )
				{
					dimNames <- vector()
					for(i in 1:dim1)
						dimNames[i]  <- paste(guess[i + 1:i -1], collapse='|')
					dimnames(out) <- rep(list(dimNames), 2)
					attr(out, 'comments') <- guess
				} else {
					dimnames(out)<- list(guess, guess)
				}
			} else {
				dimnames(out)<- list(rep(' ', dim(out)[1]), rep(' ', dim(out)[2]) )
			}
		} else {
			### DIAG style                                                              
			equalExpressionPop( x, "DIAG", sep = "[[:space:]]*", inPlace = TRUE )                                                
			x <- gsub( "[\\(\\)]", "", x )
			out <- as.numeric( .readValues( x ) )
			out <- if( length(out)==1) as.matrix(out) else diag(out)
			if(length(guess) < length(sortIt)) {
				guess <- c(guess, rep(' ',length(sortIt) - length(guess) ))
			}
			if( !is.null( comments) && guessNames )
			{
#				stretch the dimnames...
				simpleGuess <- guess	
				for (i in 1:dim(out)[1]){
					guess[i] <- 
							paste(simpleGuess[1:i], collapse='|')
				}
				if( length(guess) == nrow(out) ){
					dimnames(out) <- rep(list(guess), 2)
					attr(out, 'comments') <- guess
				}
			}
		}
		
	}
	
#	get size of OMEGAS
	dim1 <- dim(out)[1]
# 	looking for this number of names etc. 
	half <- dim1*(dim1 + 1)/2
	
#	If there is no mention of FIX then replicate FALSE the necessary number of times
	if(length(fixedOMEGAS)!=half){
		fixedOMEGAS <- rep(fixedOMEGAS, half)
	}
	
#	If there is a comment however on the $OMEGA line but no OMEGAVALUE actuall specified there
	if(length(fixedOMEGAS) == half + 1 )
		fixedOMEGAS <- fixedOMEGAS[-1]
	
	dimNames <- vector()
	if(length(fixedOMEGAS) == half )
	{
		for(i in 1:dim1)
			dimNames[i]  <- paste(fixedOMEGAS[i + 1:i -1], collapse='|')
	}
	dimnames(out)<- list( paste(dimnames(out)[[1]], dimNames), paste(dimnames(out)[[2]],dimNames))
#	print(out)
	
	out
}

##################################################################
# .importNmModOmega
# Reads the "omegas" or "sigmas" from the control file 
# Author: J James, R Francois, F. Gochez
# Added: Jan 5 2009
# Last modified: Jan 8 2009
# parameters :
# @ txt [C,+] - A character vector holding the text to pase
# @ guessNames [L,1] - Logical flag.  If TRUE, will return substrings of the full title names found
# @ component [C,1] - Which bit to extract, OMEGAs or SIGMAs
# @ file [C,1] - File from which to scan txt, if desired
# Matrix of OMEGA initial values  
# See the NONMEM documentation, parts IV and VIII for a description of some of the ways that $OMEGA can be
# specified
##################################################################

.importNmModOmega <- function(
		txt = NULL, 
		guessNames = TRUE, 
		component = c("OMEGA", "SIGMA"), 		 
		file = NULL
)
{	
	if(!is.null(file))
		txt <- scanFile(file)
	component <- match.arg(component)
	.extract <- length(grep( sprintf("\\$", component), txt))
	if( is.null( txt)) return(NULL)
	
	### import the OMEGA declarations                                             
	if(.extract) {
		omegas <- 	section( txt, component, "", strip = TRUE ) 
	} else {
		omegas <- txt
	}
	### each $OMEGA is a separate block
	# this is somewhat complex because omegas can be specified in different ways
	
	mList <- 
			lapply( omegas, 
					.extractInformation, guessNames = guessNames, component)
	
	### structure the output in one single matrix                                 
	out <- 
			blockBind( mList, defaultPrefix=component, TRUE )
	out
	
}
