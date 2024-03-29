#' Utility function to convert tonnage to strings
#' 
#' inputs to get_ functions are required as strings when passed as a sql statement
#' This function converts numeric inputs to character strings. Used specifically for tonnage class
#'
#' @param itemName Character string. Variable name as it exists in the data base
#' @param chosenItem User input value of variable listed in itemName
#' 
#' @return A character string
#' 
#' @examples 
#' \dontrun{
#' createTonnageString(itemName="toncl2",chosenItem=c(1,2))
#' createTonnageString(itemName="toncl2",chosenItem="all")
#' 
#' }
#' @export


createTonnageString <- function(itemName,chosenItem){
  if (is.numeric(chosenItem)) { # tonnage class
    # search for individual tonnage class type or vector
    itemStr <- " ("
    numItems <- length(chosenItem)
    if (numItems > 1) { # if more than one ionnage class.
      for (it in 1:(numItems-1)) {
        tonnage <- chosenItem[it]
        itemStr <- paste0(itemStr,itemName," like \'",tonnage,"%\' or ")
      }
    } else {# append last one
    }
    itemStr <- paste0(itemStr,itemName," like \'",utils::tail(chosenItem,1),"%\')")
  } else { #Fleet
    if (tolower(chosenItem) == "all") {
      itemStr <- NULL
    } else {
      stop("Not valid tonnage Argument. Either \"ALL\" or a vector of tonnage classes (see toncl1)")
    }
    # AND area IN (513,515,514) etc.
  }
  return(itemStr)
}
