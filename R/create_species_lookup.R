#' Pair NESPP3, SVSPP3, Species_itis etc
#'
#' Create a table with all species infomation across databases. 
#' Combine  NESPP3, SVSPP3, species_itis, common name and scientific name
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#' @param species Character String. Either NESPP3, SVSPP, Species_itis (not case sensitive)
#' @param speciesType Character string. Type of species code entered for \code{species}. Default = "NESPP3"
#' 
#' @return Tibble:
#' 
#' The number of rows = the length of \code{species} with columns
#' COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP3, NAFOSPP, SVSPP
#' 
#' @section Database tables used:
#' 
#' Two tables are used to compile infomation. cfdbs.cfspp and cfdbs.species_itis_ne.
#' The resulting tables are then joined.
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

create_species_lookup <- function(channel,species="all",speciesType="NESPP3"){
  
  if ((tolower(speciesType) == "nespp3") | (tolower(speciesType) == "svspp")) {
    # look in cfdbs.cfspp table for nespp3, nespp4, svspp, nafospp, then join with species_itis_ne
    # by nespp4 to obtain sci & com name
    if (!is.character(species)) {
      speciesList <- sprintf(paste0("%03d"),species)
      speciesList <-  paste0("'",speciesList,"'",collapse=",")
    }
    
    sql2 <- paste0("select distinct NESPP3, NESPP4, NAFOSPP, SVSPP from cfdbs.cfspp where ",toupper(speciesType)," in (",speciesList,") order by NESPP4;")
    speciesTable2 <- DBI::dbGetQuery(channel,sql2)
    
    nespp4s <- paste0("'",speciesTable2$NESPP4,"'",collapse=",")
    sql <- paste0("select distinct COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 from cfdbs.species_itis_ne where NESPP4 in (",nespp4s,") order by NESPP4;")
    speciesTable1 <- DBI::dbGetQuery(channel,sql)

  } else if (tolower(speciesType) == "species_itis") {
    # convert numeric code to character
    if (!is.character(species)) {
      speciesList <- sprintf(paste0("%06d"),species)
      speciesList <-  paste0("'",speciesList,"'",collapse=",")
    }
    # look in species_itis_ne table and join with cfdbs.cfspp table by nespp4 to obtain other codes
    sql <- paste0("select distinct COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 from cfdbs.species_itis_ne where SPECIES_ITIS in (",speciesList,") order by SPECIES_ITIS, NESPP4;")
    speciesTable1 <- DBI::dbGetQuery(channel,sql)
    #speciesTable$NESPP3 <- substr(speciesTable$NESPP4,start=1,stop=3)
    #speciesTable <- speciesTable %>% dplyr::select(COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP3) %>% dplyr::distinct()
    nespp4s <- paste0("'",speciesTable1$NESPP4,"'",collapse=",")
    sql2 <- paste0("select distinct NESPP3, NESPP4, NAFOSPP, SVSPP from cfdbs.cfspp where NESPP4 in (",nespp4s,") order by NESPP4;")
    speciesTable2 <- DBI::dbGetQuery(channel,sql2)

  } else {
    stop(paste0("Not coded for speciesType = ",speciesType))
  }
  
  speciesTable <- dplyr::inner_join(speciesTable1,speciesTable2,by="NESPP4") %>% 
    dplyr::select(-NESPP4) %>%
    dplyr::mutate(SCIENTIFIC_NAME = gsub("^\\s+|\\s+$", "",SCIENTIFIC_NAME)) %>%
    dplyr::mutate(COMMON_NAME = gsub(", ","",COMMON_NAME)) %>%
    dplyr::distinct()
  # cleaned up table. Remove blanks in common name
  
  
  return(dplyr::as_tibble(speciesTable))
}

# trim leading and trailing whitespace
#trim_blanks <- function (x) gsub("^\\s+|\\s+$", "", x)
 