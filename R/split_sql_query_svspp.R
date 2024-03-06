#' Splits a query into multiple parts, SVSPP codes
#' 
#' If a query has > 1000 items it will fail. For all cases where a query has > \code{maxItems} the call will be split. This occurrs when NESPP4 codes used in an a sql call.
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#'@param NESPP4s Numerical Vector. NESPP4 codes used in an sql query
#'@param maxItems Numeric scalar. Maximum number of items in a single sql query
#'
#'@return List
#'\item{sql}{the sql statements}
#'\item{speciesTable}{Resulting table from concatenating the sql calls}
#'
#' @importFrom rlang .data


split_sql_query_svspp <- function(channel,NESPP4s,maxItems=900) {
  

  speciesTable <- data.frame()
  sql <- list()
  
  numNESPP4s <- length(NESPP4s)
  is <- 1
  inum <- 0
  while (numNESPP4s > 0) {
    inum <- inum + 1
    
    if (numNESPP4s < maxItems) {
      nespp4split <- NESPP4s[is:length(NESPP4s)]
    } else { 
      nespp4split <- NESPP4s[is:(is+maxItems-1)]
    }
    # use NESPP4s to select from species_itis_ne
    nespp4s <- paste0("'",nespp4split,"'",collapse=",")

    sqlpart <- paste0("select distinct NESPP3, NESPP4, NAFOSPP, SVSPP from cfdbs.cfspp where NESPP4  in (",nespp4s,") order by NESPP4")
    speciesTablepart <- DBI::dbGetQuery(channel,sqlpart)  |> 
      dplyr::mutate(SVSPPcf = gsub("^\\s+|\\s+$", "",.data$SVSPP)) |> 
      dplyr::select(-.data$SVSPP)
    
    sql[[inum]] <- sqlpart
    speciesTable <- rbind(speciesTable,speciesTablepart)
    
    is <- is + maxItems 
    numNESPP4s <- numNESPP4s - maxItems
  }
  return(list(sql=sql,speciesTable=speciesTable))
  
}