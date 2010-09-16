

##################################################################
# blockBind
# Builds a matrix from a set of NONMEM-style matrix specifications
# Author: R. Francois, modifications by F. Gochez
# Added: Jan 6 2009
# Last modified: Jan 6 2009
# parameters :
# @ mList [list,+] - A list of character vectors holding NONMEM matrix specs
# @ defaultPrefix [C,1] - Default prefix for the names of the matrix rows and colunmns (e.g. OMEGAX)
# @ givenNames [N, 1] - Starting position of the title substrings returned (only used if substring is TRUE)
# Returns:  A matrix
##################################################################

blockBind <- function(
		mList, 
		defaultPrefix,
		giveNames = !missing(defaultPrefix) ) 
{
	
	### handling "SAME". See ?$OMEGA
	# 
	for( i in seq(along = mList))
	{
		if(i==1) next
		mi <- mList[[i]]
		if( is.character(mi) && length(mi) == 1 && mi == "SAME")
		{
			names <- rownames( mi ) 
			mList[[i]] <- mList[[i-1]]
			if( !is.null(names) &&length(names) == nrow(mList[[i]]) )  
				dimnames( mList[[i]] ) <- rep( list(names), 2)
		}	
	}
	
	mList <- lapply( mList, as.matrix )
	rowList <- sapply(mList, nrow)
	outMat <- array(0, dim = rep(sum(rowList), 2))
	rowList <- 1 + cumsum(rowList) - rowList
	
	for(i in 1:length(mList)) 
	{
		start <- rowList[i]
		end <- rowList[i] - 1 + nrow(mList[[i]])
		outMat[start:end, start:end] <- mList[[i]]
	}
	
	if( giveNames ) 
	{
		### get the names that are already there
		blockName <- names <- 
				unlist( 
						lapply( mList, function(x){ 
									if(is.null(dimnames(x)[[1]])) rep("FALSE", nrow(x)) else rownames(x)  
								}
						)
				)
		fixed <- regexpr(' TRUE| FALSE', names)
		these <- which(fixed >0)
#		names will have the FIXED|TRUE in them
		if(length(these)>0){
			names[these] <- substring(names[these], fixed[these] + 1)
		} 
		
		comments <- unlist( 
				sapply( mList, function(x){ 
							gsub('$ *| *$','', attr(x, 'comments'))  
						}
				)
		)
		
		half <- unlist( 
				sapply( mList, function(x){ 
							dim(x)[1]  
						}
				)
		)
		
		half <- sum(half)
		fixed <- addThese <- character(0)
		elementName <- matrix('', nrow=half, ncol=half)
#		browser()
		if(length(comments)== half*(half+1)/2){
#			assumes block structure
			count <- 0
			trimmedComments <- gsub('^ +| +$','', comments)
			for(i in 1:half){
				for(j in 1:i){
					count <- count + 1
					if(nchar(trimmedComments[count])==0)
						trimmedComments[count] <- paste(defaultPrefix, i, j, sep='')
					elementName[i,j]<- trimmedComments[count] 
				}
				addThese[i] <- paste(elementName[i,1:i], collapse='|')
			}
			blankNames <- which(nchar(gsub(' |TRUE|FALSE|[|]', '', names))==0)
			if(length(blankNames)>0){
				for(thisName in blankNames){
					names[thisName] <- paste(addThese[thisName], names[thisName], sep=' | ')
				}
			}
			### replace empty with appropriate name
			names <- ifelse( names == "", sprintf("%s%d", defaultPrefix, 1:nrow(outMat)), names  )
		} else {
			if(all(length(comments) == half)){
#			assumes diagonal structure
				### replace empty with appropriate name
				dummies <- which(comments == "")
				if(length(dummies)>0){
					for(dummy in dummies){
						comments[dummy] <- 
								sprintf("%s%d%d", defaultPrefix, dummy, dummy)
					}
					
				}
				dummies <- which(comments == " |")
				if(length(dummies)>0){
					for(dummy in dummies){
						comments[dummy] <- 
								paste(defaultPrefix, dummy, 1:dummy, sep='', collapse='|')
					}
					
				}
				
				for(thisName in seq(along=names)){
					names[thisName] <- paste(comments[thisName], names[thisName], sep=' | ')
				}
			} else {
				dimnames( outMat ) <- rep( list(blockName ), 2)
				return(outMat )
			}
		} 
		dimnames( outMat ) <- rep( list(names), 2)
	}
	outMat
}