block.parser <- function(txts) {
    # simple name for method description
    simple.name = function(long.desc){
        meth.desc = list(
            list(name='FO',desc=function(desc) {
                regexpr('first order',ignore.case=TRUE, desc) > 0  && 
                regexpr('conditional',ignore.case=TRUE, desc) < 0
            }),
            list(name='FOCE',desc=function(desc) {
                if ((key0 <- regexpr('first order',ignore.case=TRUE, desc)) < 1) return(FALSE)
                regexpr('conditional', ignore.case=TRUE, desc) > key0
            })
        )
        for(i in meth.desc){
            if (i$desc(long.desc)) {
                return(i$name)
            }
        }
        'unknown'
    }

    # txts is a character vector
    pos = 0
    NL  = length(txts)
    BLOCKWIDTH = 80
    word = NULL
    # stream fun
    nextl = function(){
        if (pos + 1 > NL ) {
            return(NULL)
        }
        txts[pos + 1]
    }
    consum= function(){
        if (!is.null(foo<-nextl())) {
            pos <<- pos +1
        }
        foo
    }
    # TOKEN fun
    # Here we relax the constraint for ^\s+ to ^\s* and \*+$ to \*+\s*$
    dilim = function(){
        x = nextl()
        if (nchar(x)<BLOCKWIDTH) return(FALSE)
        if (regexpr('^\\s*\\*+\\s*$', x)<1) return(FALSE)
        consum()
        TRUE
    }
    bol.txt = function(){
        x = nextl()
        if (nchar(x)<BLOCKWIDTH) return(FALSE)
        if (regexpr('^\\s*\\*+([^*]+.*[^*]+)\\*+\\s*$',x) < 1) return(FALSE)
        consum()
        word <<- sub('^\\s*\\*+([^*]+.*[^*]+)\\*+\\s*$','\\1',x)
        TRUE
    }
    # stack fun
    stack <- vector(mode='list',10)
    stack.top <- 0
    push         = function(entry){
        stack.top <<- stack.top + 1
        stack[[ stack.top ]] <<- entry
    }
    pop          = function(){
        if (stack.top < 1) stop('stack overflow')
        re = stack[[ stack.top ]]
        stack.top <<- stack.top - 1
        re
    }
    clear.stack  = function(){
        stack.top <<- 0
    }
    reduce.stack = function(){
        if ( stack.top == 0 ) return(FALSE)
        # check if the stack has a pattern
        # here we only have one pattern, a formal one may split the rules matching outside
        #    DILIM 
        #     bol.txt 
        #     bol.txt 
        #     bol.txt 
        #     bol.txt 
        #    DILIM
        # however, this simple one doesn't need 
        if ( stack.top < 6 ) return(FALSE)

        exp.typ = c('dilim',rep('word',4),'dilim')
        for(i in 1:6){
            if ( exp.typ[i] != stack[[ stack.top - 6 + i ]]$type ) {
                return(FALSE)
            }
        }
        # the pattern matched
        method.line = stack[[ stack.top - 3]]
        method.name = simple.name(method.line$val)
        stack.top <<- stack.top - 6 + 1
        stack[[ stack.top ]] <<- list(type='method.name',val=method.name,lineno=method.line$lineno)
        TRUE
    }

    # results
    results = vector(mode='list', 10)
    results.n = 0

    # main loop
    while(!is.null(nextl())){
        # First emit TOKENs
        if(dilim()){
            push(list(type='dilim',val=1))
        } else if (bol.txt()) {
            push(list(type='word',val=word, lineno=pos))
        } else {
            # For this particular problem
            # if match failed, just clear the stack
            consum()
            clear.stack()
        }
        # Then reduce stack by rules
        if (reduce.stack()) {
            # because we clear the stack
            # we need to save the results first
            results.n <- results.n + 1
            results[[ results.n ]] = pop()
        }
    }
    # return results
    if (results.n > 0) {
        results[1:results.n]
    } else {
        NULL
    }
}
