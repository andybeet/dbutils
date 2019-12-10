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
  
  if (tolower(speciesType) == "nespp3"){
    
    # look in cfdbs.cfspp table for nespp3, nespp4, svspp, nafospp, then join with species_itis_ne
    # by nespp4 to obtain sci & com name
    # first format to database type
    if (!is.character(species)) {
      species <- sprintf(paste0("%03d"),species)
      species <-  paste0("'",species,"'",collapse=",")
    }
    
    sql1 <- paste0("select distinct NESPP3, NESPP4, NAFOSPP, SVSPP from cfdbs.cfspp where ",toupper(speciesType)," in (",species,") order by NESPP4;")   
    speciesTable1 <- DBI::dbGetQuery(channel,sql1) %>%
      dplyr::mutate(SVSPPcf = gsub("^\\s+|\\s+$", "",SVSPP)) %>%
      dplyr::select(-SVSPP)
    
    # use NESPP4s to select from species_itis_ne
    nespp4s <- paste0("'",speciesTable1$NESPP4,"'",collapse=",")
    sql2 <- paste0("select distinct COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 from cfdbs.species_itis_ne where NESPP4 in (",nespp4s,")and NESPP4_FLAG = 1 order by NESPP4;")
    speciesTable2 <- DBI::dbGetQuery(channel,sql2) %>%
      dplyr::mutate(SCIENTIFIC_NAME = gsub("^\\s+|\\s+$", "",SCIENTIFIC_NAME)) %>%
      dplyr::mutate(COMMON_NAME = gsub(", ",",",COMMON_NAME))
    
    sciNames <- paste0("'",speciesTable2$SCIENTIFIC_NAME,"'",collapse=",")

    # use sci names to get SVSPP from svdbs
    sql3 <- paste0("select distinct COMNAME, SCINAME, SVSPP from svdbs.svspecies_list where SCINAME in (",sciNames,") order by SVSPP;")
    speciesTable3 <- DBI::dbGetQuery(channel,sql3) %>%
      dplyr::rename(SCIENTIFIC_NAME=SCINAME) %>%
      dplyr::mutate(SVSPPsv = SVSPP) %>%
      dplyr::select(-SVSPP)
    
    # join 2 tables by NESPP4
    speciesTable <- dplyr::inner_join(speciesTable1,speciesTable2,by="NESPP4") %>% 
      dplyr::select(-NESPP4)
    # join with svdbs
    speciesTable <- dplyr::inner_join(speciesTable,speciesTable3,by="SCIENTIFIC_NAME") %>%
      dplyr::distinct() %>%
      dplyr::select(NESPP3,NAFOSPP,SVSPPcf,SVSPPsv,COMMON_NAME,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>%
      dplyr::arrange(NESPP3) %>%
      dplyr::as_tibble()
    
  } else if  (tolower(speciesType) == "svspp") {
    # first format to database type svspp are numeric
    species <-  paste0(as.numeric(species),collapse=",")

    sql1 <- paste0("select distinct COMNAME, SCINAME, SVSPP from svdbs.svspecies_list where SVSPP in (",species,") order by SVSPP;")

    speciesTable1 <- DBI::dbGetQuery(channel,sql1) %>%
      dplyr::rename(SCIENTIFIC_NAME=SCINAME) %>%
      dplyr::mutate(SVSPPsv = SVSPP) %>%
      dplyr::select(-SVSPP)
    
    sciNames <- paste0("'",speciesTable1$SCIENTIFIC_NAME,"'",collapse=",")
    
    # use SCIENTFIC_NAME to select from species_itis_ne
    sql2 <- paste0("select distinct COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 from cfdbs.species_itis_ne where SCIENTIFIC_NAME in (",sciNames,") and NESPP4_FLAG = 1 order by NESPP4;")
    speciesTable2 <- DBI::dbGetQuery(channel,sql2) %>%
      dplyr::mutate(SCIENTIFIC_NAME = gsub("^\\s+|\\s+$", "",SCIENTIFIC_NAME)) %>%
      dplyr::mutate(COMMON_NAME = gsub(", ",",",COMMON_NAME))
    
    # use NESPP4s to select from cfdbs.cfspp (NAFOSPP)
    nespp4s <- paste0("'",speciesTable2$NESPP4,"'",collapse=",")
    
    sql3 <- paste0("select distinct NESPP3, NESPP4, NAFOSPP, SVSPP from cfdbs.cfspp where NESPP4  in (",nespp4s,") order by NESPP4;")
    speciesTable3 <- DBI::dbGetQuery(channel,sql3) %>%
      dplyr::mutate(SVSPPcf = gsub("^\\s+|\\s+$", "",SVSPP)) %>%
      dplyr::select(-SVSPP)
    
    # join 2 tables by NESPP4
    speciesTable <- dplyr::inner_join(speciesTable1,speciesTable2,by="SCIENTIFIC_NAME") 
    # join with svdbs
    speciesTable <- dplyr::inner_join(speciesTable,speciesTable3,by="NESPP4") %>%
      dplyr::select(-NESPP4) %>%
      dplyr::distinct() %>%
      dplyr::select(NESPP3,NAFOSPP,SVSPPcf,SVSPPsv,COMMON_NAME,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>%
      dplyr::arrange(NESPP3) %>%
      dplyr::as_tibble()
  
  } else if (tolower(speciesType) == "species_itis") {
    # convert numeric code to character
    species <- sprintf(paste0("%06d"),as.numeric(species))
    species <-  paste0("'",species,"'",collapse=",")
    
    
    # look in species_itis_ne table and join with cfdbs.cfspp table by nespp4 to obtain other codes
    sql <- paste0("select distinct COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 from cfdbs.species_itis_ne where SPECIES_ITIS in (",species,") and NESPP4_FLAG = 1 order by NESPP4;")
    speciesTable1 <- DBI::dbGetQuery(channel,sql) %>% 
      dplyr::mutate(SCIENTIFIC_NAME = gsub("^\\s+|\\s+$", "",SCIENTIFIC_NAME)) %>%
      dplyr::mutate(COMMON_NAME = gsub(", ",",",COMMON_NAME))
      
    # use NESPP4 to select from cfdbs.cfspp
    nespp4s <- paste0("'",speciesTable1$NESPP4,"'",collapse=",")
    # use SCIENTIFIC_NAME from svspecies_list
    sciNames <- paste0("'",speciesTable1$SCIENTIFIC_NAME,"'",collapse=",")
    
    sql2 <- paste0("select distinct NESPP3, NESPP4, NAFOSPP, SVSPP from cfdbs.cfspp where NESPP4 in (",nespp4s,") order by NESPP4;")
    speciesTable2 <- DBI::dbGetQuery(channel,sql2) %>% 
      dplyr::mutate(SVSPPcf = gsub("^\\s+|\\s+$", "",SVSPP)) %>%
      dplyr::select(-SVSPP)

    sql3 <- paste0("select distinct COMNAME, SCINAME, SVSPP from svdbs.svspecies_list where SCINAME in (",sciNames,") order by SVSPP;")
    speciesTable3 <- DBI::dbGetQuery(channel,sql3) %>%
      dplyr::rename(SCIENTIFIC_NAME=SCINAME) %>%
      dplyr::mutate(SVSPPsv = SVSPP) %>%
      dplyr::select(-SVSPP)
    
    # join 2 tables by NESPP4
    speciesTable <- dplyr::inner_join(speciesTable1,speciesTable2,by="NESPP4") 
    # join with svdbs
    speciesTable <- dplyr::inner_join(speciesTable,speciesTable3,by="SCIENTIFIC_NAME") %>%
      dplyr::select(-NESPP4) %>%
      dplyr::distinct() %>%
      dplyr::select(NESPP3,NAFOSPP,SVSPPcf,SVSPPsv,COMMON_NAME,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>%
      dplyr::arrange(NESPP3) %>%
      dplyr::as_tibble()
    
  } else {
    stop(paste0("Not coded for speciesType = ",speciesType))
  }


  return(speciesTable)
}
