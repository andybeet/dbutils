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
#' @return A list is returned containing the result of the data pull as well as the sql statements used in the data pulls and a list of missing species codes, names, that were not present:
#' 
#' \item{data}{Tibble. TEach row represents a species with columns NESPP3, NAFOSPP, SVSPPcf, SVSPPsv, COMMON_NAME, COMNAME, SCIENTIFIC_NAME, SPECIES_ITIS}
#' \item{sql1}{Character string. sql statement used in first table}
#' \item{sql2}{Character string. sql statement used in second table}
#' \item{sql3}{Character string. sql statement used in third table}
#' \item{missing}{List. Codes not found in respective tables}
#' \item{lookupOrder}{Character string. The order in which the tables were accessed. Compare with the sqlx output}
#'  
#' @section Database tables used:
#' 
#' Three tables are used to compile infomation:
#' 
#' cfdbs.cfspp and species_itis_ne and svdbs.svspecies_list. The resulting tables are then joined.
#' 
#' The order in which these tables are accessed depends on \code{speciesType}. For example,
#' \code{speciesType = "NESPP3"}
#' 
#' 1. cfdbs.cfssp is accessed and NESPP3, NESPP4, NAFOSPP, SVSPP are pulled.
#' 
#' 2. cfdbs.species_itis_ne is accessed using the NESPP4 codes found in step 1 and COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 are pulled.
#' 
#' 3. svdbs.spsvspecies_list is accessed using the SCIENTIFIC_NAME codes found in step2 and COMNAME, SCINAME, SVSPP are pulled.
#' 
#' 4. The tables are then joined 
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export

create_species_lookup <- function(channel,species=NULL,speciesType="NESPP3"){
  
  if (is.null(species)) return(NA)
  missing <- list()
  message("This may take a minute, we need to access multiple databases ...")
  
  if (tolower(speciesType) == "nespp3"){
    
    # look in cfdbs.cfspp table for nespp3, nespp4, svspp, nafospp, then join with species_itis_ne
    # by nespp4 to obtain sci & com name
    # first format to database type
    if (!is.character(species)) {
      species <- sprintf(paste0("%03d"),species)
    }
    speciesToFind <-  paste0("'",species,"'",collapse=",")
    
    # form sql query then execue
    sql1 <- paste0("select distinct NESPP3, NESPP4, NAFOSPP, SVSPP from cfdbs.cfspp where ",toupper(speciesType)," in (",speciesToFind,") order by NESPP4;")   
    speciesTable1 <- DBI::dbGetQuery(channel,sql1) %>%
      dplyr::mutate(SVSPPcf = gsub("^\\s+|\\s+$", "",SVSPP)) %>%
      dplyr::select(-SVSPP) %>%
      dplyr::distinct()
    
    # compare species passed vs species found to get list of missing species
    missing[["cfdbs.cfspp"]] <- tibble::enframe(setdiff(species,unique(speciesTable1$NESPP3)),name=NULL,value="NESPP3")
    
    # if any string exceeds 1000 items the sql query will fail. 
    # So split into smaller calls
    res <- split_sql_query_nespp3(channel,speciesTable1$NESPP4,maxItems=900)
    sql2 <- res$sql
    speciesTable2 <- res$speciesTable

    # compare species in table1 vs species found in table 2 to get missing species
    missing_nespp4s <-  setdiff(unique(speciesTable1$NESPP4),unique(speciesTable2$NESPP4))
    missing_nespp3s <- unique(substring(missing_nespp4s,1,3))
    # select nespp3s missing from species_itis_ne
    missing[["cfdbs.species_itis_ne"]] <- speciesTable1 %>%
      dplyr::filter(NESPP3 %in% missing_nespp3s) %>%
      dplyr::count(NESPP3) %>% 
      dplyr::filter(n==1) %>%
      dplyr::select(NESPP3)

    sciNames <- paste0("'",unique(speciesTable2$SCIENTIFIC_NAME),"'",collapse=",")

    # use sci names to get SVSPP from svdbs
    sql3 <- paste0("select distinct COMNAME, SCINAME, SVSPP from svdbs.svspecies_list where SCINAME in (",sciNames,") order by SVSPP;")
    speciesTable3 <- DBI::dbGetQuery(channel,sql3) %>%
      dplyr::rename(SCIENTIFIC_NAME=SCINAME) %>%
      dplyr::mutate(SVSPPsv = SVSPP) %>%
      dplyr::select(-SVSPP)
 
    # compare species passed vs species found to get missing species
    missing[["svdbs.svsspecies_list"]] <-  setdiff(unique(speciesTable2$SCIENTIFIC_NAME),unique(speciesTable3$SCIENTIFIC_NAME))
    
    # Join tables -------------------------------------------------------------

    # join 2 tables by NESPP4
    speciesTable <- dplyr::inner_join(speciesTable1,speciesTable2,by="NESPP4") %>% 
      dplyr::select(-NESPP4) %>% 
      dplyr::distinct()

    # join with svdbs
    # preserve all nespp3 codes passed as argument
    speciesTable <- dplyr::left_join(speciesTable,speciesTable3,by="SCIENTIFIC_NAME") %>%
      dplyr::distinct() %>%
      dplyr::select(NESPP3,NAFOSPP,SVSPPcf,SVSPPsv,COMMON_NAME,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>%
      dplyr::arrange(NESPP3) %>%
      dplyr::as_tibble()
    
    lookupOrder <- "cfdbs.cfspp (sql1) -> cfdbs.species_itis_ne (sql2) -> svspp.svspecies_list (sql3)"

#################################################################
#################################################################
#################################################################
    
  } else if  (tolower(speciesType) == "svspp") {
    # first format to database type. svspp are numeric
    speciesToFind <-  paste0(as.numeric(species),collapse=",")

    sql1 <- paste0("select distinct COMNAME, SCINAME, SVSPP from svdbs.svspecies_list where SVSPP in (",speciesToFind,") order by SVSPP;")

    speciesTable1 <- DBI::dbGetQuery(channel,sql1) %>%
      dplyr::rename(SCIENTIFIC_NAME=SCINAME) %>%
      dplyr::mutate(SVSPPsv = SVSPP) %>%
      dplyr::select(-SVSPP)
    
    # compare species passed vs species found to get list of missing species
    missing[["svspp.svspecies_list"]] <- tibble::enframe(setdiff(species,unique(speciesTable1$SVSPP)),name=NULL,value="SVSPP")
    
    sciNames <- paste0("'",speciesTable1$SCIENTIFIC_NAME,"'",collapse=",")
    
    # use SCIENTFIC_NAME to select from species_itis_ne
    sql2 <- paste0("select distinct COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 from cfdbs.species_itis_ne where SCIENTIFIC_NAME in (",sciNames,") and NESPP4_FLAG = 1 order by NESPP4;")
    speciesTable2 <- DBI::dbGetQuery(channel,sql2) %>%
      dplyr::mutate(SCIENTIFIC_NAME = gsub("^\\s+|\\s+$", "",SCIENTIFIC_NAME)) %>%
      dplyr::mutate(COMMON_NAME = gsub(", ",",",COMMON_NAME))
    
    # compare species passed vs species found to get missing species
    missing[["cfdbs.species_itis_ne"]] <-  setdiff(unique(speciesTable1$SCIENTIFIC_NAME),unique(speciesTable2$SCIENTIFIC_NAME))

    # use NESPP4s to select from cfdbs.cfspp (NAFOSPP)
    # if any string exceeds 1000 items the sql query will fail. 
    # So split into smaller calls
    res <- split_sql_query_svspp(channel,speciesTable2$NESPP4,maxItems=900)
    sql3 <- res$sql
    speciesTable3 <- res$speciesTable
    
    # compare species in table1 vs species found in table 2 to get missing species
    missing_nespp4s <-  setdiff(unique(speciesTable2$NESPP4),unique(speciesTable3$NESPP4))
    missing_nespp3s <- unique(substring(missing_nespp4s,1,3))
    # select nespp3s missing from species_itis_ne
    missing[["cfdbs.cfspp"]] <- speciesTable3 %>%
      dplyr::filter(NESPP3 %in% missing_nespp3s) %>%
      dplyr::count(NESPP3) %>% 
      dplyr::filter(n==1) %>%
      dplyr::select(NESPP3)
    
    # join 2 tables by NESPP4
    speciesTable <- dplyr::inner_join(speciesTable1,speciesTable2,by="SCIENTIFIC_NAME") 
    # join with svdbs
    speciesTable <- dplyr::inner_join(speciesTable,speciesTable3,by="NESPP4") %>%
      dplyr::select(-NESPP4) %>%
      dplyr::distinct() %>%
      dplyr::select(NESPP3,NAFOSPP,SVSPPcf,SVSPPsv,COMMON_NAME,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>%
      dplyr::arrange(NESPP3) %>%
      dplyr::as_tibble()
    
    lookupOrder <- "svspp.svspecies_list (sql1) -> cfdbs.species_itis_ne (sql2) -> cfdbs.cfspp (sql3)"
  
    
    #################################################################
    #################################################################
    #################################################################
    
    
  } else if (tolower(speciesType) == "species_itis") {
    # convert numeric code to character
    speciesToFind <- sprintf(paste0("%06d"),as.numeric(species))

    speciesToFind <-  paste0("'",speciesToFind,"'",collapse=",")
    
    # look in species_itis_ne table and join with cfdbs.cfspp table by nespp4 to obtain other codes
    sql1 <- paste0("select distinct COMMON_NAME, SCIENTIFIC_NAME, SPECIES_ITIS, NESPP4 from cfdbs.species_itis_ne where SPECIES_ITIS in (",speciesToFind,") and NESPP4_FLAG = 1 order by NESPP4;")
    speciesTable1 <- DBI::dbGetQuery(channel,sql1) %>% 
      dplyr::mutate(SCIENTIFIC_NAME = gsub("^\\s+|\\s+$", "",SCIENTIFIC_NAME)) %>%
      dplyr::mutate(COMMON_NAME = gsub(", ",",",COMMON_NAME))
      
        # compare species passed vs species found to get list of missing species
    missing[["cfdbs.species_itis_ne"]] <- tibble::enframe(setdiff(species,unique(speciesTable1$SPECIES_ITIS)),name=NULL,value="SPECIES_ITIS")

    # use NESPP4 to select from cfdbs.cfspp
    # if any string exceeds 1000 items the sql query will fail. 
    # So split into smaller calls
    res <- split_sql_query_svspp(channel,speciesTable1$NESPP4,maxItems=900)
    sql2 <- res$sql
    speciesTable2 <- res$speciesTable


    # compare species in table1 vs species found in table 2 to get missing species
    missing_nespp4s <-  setdiff(unique(speciesTable1$NESPP4),unique(speciesTable2$NESPP4))
    missing_nespp3s <- unique(substring(missing_nespp4s,1,3))
    # select nespp3s missing from species_itis_ne
    missing[["cfdbs.cfspp"]] <- speciesTable2 %>%
      dplyr::filter(NESPP3 %in% missing_nespp3s) %>%
      dplyr::count(NESPP3) %>% 
      dplyr::filter(n==1) %>%
      dplyr::select(NESPP3)

    # use SCIENTIFIC_NAME from svspecies_list
    sciNames <- paste0("'",speciesTable1$SCIENTIFIC_NAME,"'",collapse=",")

    sql3 <- paste0("select distinct COMNAME, SCINAME, SVSPP from svdbs.svspecies_list where SCINAME in (",sciNames,") order by SVSPP;")
    speciesTable3 <- DBI::dbGetQuery(channel,sql3) %>%
      dplyr::rename(SCIENTIFIC_NAME=SCINAME) %>%
      dplyr::mutate(SVSPPsv = SVSPP) %>%
      dplyr::select(-SVSPP)
    
    # compare species passed vs species found to get missing species
    missing[["svdbs.svsspecies_list"]] <-  setdiff(unique(speciesTable1$SCIENTIFIC_NAME),unique(speciesTable3$SCIENTIFIC_NAME))

    # join 2 tables by NESPP4
    speciesTable <- dplyr::inner_join(speciesTable1,speciesTable2,by="NESPP4") 
    # join with svdbs
    speciesTable <- dplyr::inner_join(speciesTable,speciesTable3,by="SCIENTIFIC_NAME") %>%
      dplyr::select(-NESPP4) %>%
      dplyr::distinct() %>%
      dplyr::select(NESPP3,NAFOSPP,SVSPPcf,SVSPPsv,COMMON_NAME,COMNAME,SCIENTIFIC_NAME,SPECIES_ITIS) %>%
      dplyr::arrange(NESPP3) %>%
      dplyr::as_tibble()
    
    lookupOrder <- "cfdbs.species_itis_ne (sql1) -> cfdbs.cfspp (sql2) -> svspp.svspecies_list (sql3)"
    
    
  } else {
    stop(paste0("Not coded for speciesType = ",speciesType))
  }


  return(list(data=speciesTable,sql1=sql1,sql2=sql2,sql3=sql3,missing=missing,lookupOrder=lookupOrder))
}
