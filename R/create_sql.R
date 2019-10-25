#' called by many functions from several db packages to convert the passed arguments to sql statements
#' 
#' utility function to create a SQL statement based on passed arguments
#' 
#' @param dataName Character or numeric. value of argument passed
#' @param fieldName Character. Database fieldname name which has value = \code{dataName}
#' @param fieldName2 Character. Alternative field name for string values of \code{dataName}
#' @param dataType Character. Formatting for the \code{dataName} in the database
#' @param defaultSqlStatement String. Select statement. (eg. "Select * from db")
#' 
#' @return String. SQL statement 
#' 
#' @examples
#' \dontrun {
#' create_sql(dataName = "all",fieldName="svspp", fieldname2= "comname", dataType = "%03d",defaultSqlStatement="select * from svdbs.svspecies_list")
#' }
#' @export

create_sql <- function(dataName,fieldName,fieldName2,dataType,defaultSqlStatement) {

  sqlStatement <- defaultSqlStatement

  if (is.numeric(dataName)) { # convert to string as where clause
    str <- sprintf(dataType,dataName)
    str <- paste0("'", str, "'", collapse=", ")
    where <-  paste0(" (",fieldName," in (",str,"))")
    sqlStatement <- paste(sqlStatement,"where",where,";")
  } else if (length(dataName) > 1){ # vector of character ids
    str <- paste0("'", dataName, "'", collapse=", ")
    where <-  paste0(" (",fieldName," in (",str,"))")
    sqlStatement <- paste(sqlStatement,"where",where,";")
  } else if (dataName == "all") {
    # we use default sqlStament
  } else if (suppressWarnings(is.na(as.numeric(dataName)))) {# species name
    where <-  paste0(fieldName2," like ","'%",toupper(dataName),"%'")
    sqlStatement <- paste(sqlStatement,"where",where,";")
  } else { # single character species id
    str <- paste0("'", dataName, "'", collapse=", ")
    where <-  paste0(" (",fieldName," in (",str,"))")
    sqlStatement <- paste(sqlStatement,"where",where,";")
  }

  return(sqlStatement)

}
