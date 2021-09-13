#' Captures function call
#'
#' A function call and its arguments, including default arguments, are captured and returned as class call
#'
#'@param ... F
#'
#' @return the function call
#'
#'
#'@export

capture_function_call <- function(...) {
  call <- base::evalq(base::match.call(expand.dots = FALSE), parent.frame(1))
  formals <- base::evalq(formals(), parent.frame(1))
  
  for(i in base::setdiff(names(formals), names(call))){
    call[i] <- list( formals[[i]] )
  }
  
  return(base::match.call(base::sys.function(base::sys.parent()), call))
}
