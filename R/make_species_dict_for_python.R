#' Create species dictionary for Python workflows
#' 
#' @description Takes the global_species_dictionary bundled in the BTOTools package 
#' and exports all relevant taxa to make a lookup for Python workflows.
#' 
#' @param path_out, the location where the species dictionary should be written
#' 
#' @import BTOTools
#' 
#' @export
#' 
#' @examples
#' make_species_dict_for_python(path_out = 'E://polyglotta/python/_packages/polyglotta')
#' 
#' 
make_species_dict_for_python <- function(path_out) {
  
  
  data(global_species_lookup, package = 'BTOTools')
  gsl <- global_species_lookup
  #rm(global_species_lookup)
  
  #remove nil waterfowl as this uses XX code
  gsl <- subset(gsl, master_taxon_id != 1718)
  
  
  #reassign generic bird as XX
  gsl$code2ltr <- ifelse(gsl$master_taxon_id==100026, 'XX', gsl$code2ltr)
  
  #add codes for Tasmania species we need to retain
  gsl$code5ltr <- ifelse(gsl$master_taxon_id==52029, 'AUSBI', gsl$code5ltr) #Australasian Bittern
  gsl$code5ltr <- ifelse(gsl$master_taxon_id==54191, 'FAECU', gsl$code5ltr) #Far Eastern Curlew


  #deal with birds
  birds <- subset(gsl, taxa == 'Birds' & (!is.na(code5ltr) | !is.na(code2ltr)))
  birds$euring <- NULL
  birds$alt_int_name <- NULL
  birds$taxon_rank_id <- NULL
  birds$taxon_rank_name <- NULL
  birds$code <- ifelse(is.na(birds$code2ltr), birds$code5ltr, birds$code2ltr)
  birds$code2ltr <- NULL
  birds$code5ltr <- NULL
  birds$pipeline_code <- NULL
  birds$parent_species_mti <- NULL

  #deal with amphibians
  amphi <- subset(gsl, taxa == 'Amphibians' & !is.na(pipeline_code), select = c('master_taxon_id', 'scientific_name', 'english_name','taxa', 'pipeline_code', 'sort_order'))
  names(amphi)[which(names(amphi)=='pipeline_code')] <- 'code'
  amphi$code <- toupper(amphi$code)
  
  #deal with mammals
  mammals <- subset(gsl, taxa == 'Mammals')
  #add code for unidentified deer
  mammals$code5ltr <- ifelse(mammals$master_taxon_id == 5630, 'XDEER', mammals$code5ltr)
  mammals <- subset(mammals, !is.na(code5ltr), select = c('master_taxon_id', 'scientific_name', 'english_name','taxa', 'code5ltr', 'sort_order'))
  names(mammals)[which(names(mammals)=='code5ltr')] <- 'code'

  print(names(birds))
  print(names(amphi))
  print(names(mammals))
  
  all <- rbind(birds, mammals, amphi)
  
  write.csv(x = all, file = file.path(path_out, "global_species_lookup.csv"), row.names = FALSE) 
  
  
}

