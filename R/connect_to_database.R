#' Connect to an internal NEFSC database.
#'
#'A utility function enabling the user to connect to an internal database provided the user has permissions and
#'posesses a valid username and password. A popup window is used for secure password enntry
#'
#' For this to work, you need an oracle client installed.
#' Tested with Oracle instantClient_12_2 installed
#' Note: if you use 64 bit Rstudio then you need a 64 bit client
#' Note: if you use 32 bit Rstudio then you need a 32 bit client
#'
#'
#' @param server  name of the server
#' @param uid  username of person with permissions
#' @param quiet Boolean. Suppress successful connection message. 
#' 
#' @return Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#'
#' @section Warning:
#' 3 failed attempts and you will be locked out of the system.
#' @seealso \code{\link{DBI}}, \code{\link{odbc}}
#'
#' @examples
#' \dontrun{
#' con <- connect_to_database(server="name_of_server",uid="individuals_username")
#'}
#' @export

connect_to_database  <-  function(server,uid,quiet=F){
  # calls function for user to enter password
  pwd <- getPass::getPass(msg=paste0("Enter the password for user ",uid," on server (",server,"):"),forcemask = FALSE)
  

  # connects to DB and catches errors and warnings
   chan <- tryCatch(
      {
        chan <- DBI::dbConnect(odbc::odbc(), dsn=server,uid=uid,pwd=pwd, timeout = 10)
        
      }, warning=function(w) {
        if (grepl("login denied",w)) {message("login to server failed - Check username and password")}
        if (grepl("locked",w)) {message("logon to server failed - Account may be locked")}
        message(paste0("Can not Connect to Database: ",server))
        return(NA)
      }, error=function(e) {
        message(paste0("Terminal error: ",e))
        return(NA)
      }, finally = {

      }
    )
   
   
     if (isS4(chan) & (!quiet)){
       message(paste0("Successfully connected to Database: ",server))
     } else if (isS4(chan) & (quiet)) {
       # no message
     } else {
       message(paste0("NOT sucessfully connected to Database: ",server,". You attemptted to connect using username: ",uid,". Three incorrect password attempts will lock your account and you will need DMS to reset your password."))
     }
   
   
  # returns connection object
  return(chan)
}

