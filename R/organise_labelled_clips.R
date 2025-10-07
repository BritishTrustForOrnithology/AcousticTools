#' Organise labelled files into species folders
#' 
#' @description
#' This objective of this function is to move audio files with paired label file 
#' into appropriate 'species' folders within the audio library. It should be used 
#' after running check_labels() to ensure compliance of labels with audio library 
#' rules. 
#' 
#' @import BTOTools
#' @import stringr
#' @import rstudioapi
#' 
#' @param folder = Path to a folder containing the audio and label files to be moved. Can be NULL and use RStudio folder dialogue instead.
#' @param path_library = Path to the top level of the audio library. Can be NULL and use RStudio folder dialogue instead.
#' 
#' @export
#' 
organise_labelled_clips <- function(folder = NULL, path_library = NULL) {
  if(is.null(folder)) {
    path_input <- rstudioapi::selectDirectory(caption = 'Select Dir containing clips to organise')
  } else path_input = folder
  if(is.null(path_library)) {
    path_library <- rstudioapi::selectDirectory(caption = 'Select location of audio library')
  }
  #validate paths - last level must be "audio_library"
  if(basename(path_library) != 'audio_library') stop("path_library incorrectly specified. Must be top level ending audio_library")
  
  #prepare the species dictionary as used for labelling
  data("global_species_lookup")
  gsl <- global_species_lookup
  #deal with Pied Wag slash
  gsl$english_name <- ifelse(gsl$code2ltr=='PW', 'Pied-White Wagtail', gsl$english_name)
  #deal with dog
  gsl$english_name <- ifelse(gsl$scientific_name=='Canis familiaris', 'Dog', gsl$english_name)
  gsl$english_name <- ifelse(gsl$scientific_name=='Ovis aries', 'Sheep', gsl$english_name)
  gsl$english_name <- ifelse(gsl$scientific_name=='Bos taurus', 'Cattle', gsl$english_name)
  #replace some special codes 
  gsl$english_name <- ifelse(!is.na(gsl$code2ltr) & gsl$code2ltr=='XX', 'Unidentified', gsl$english_name)
  # #convert dot to underscore as used in filenames
  gsl$code2ltr <- gsub(pattern = '.', '_', gsl$code2ltr, fixed = TRUE)
  #remove numeric mammal codes
  gsl$code2ltr <- ifelse(!is.na(suppressWarnings(as.numeric(gsl$code2ltr))), NA, gsl$code2ltr)
  #remove Waterbird assemblage ZZ code
  gsl <- subset(gsl, is.na(english_name) |english_name != "Waterbird assemblage")
  gsl$code <- gsl$code2ltr
  gsl$code <- ifelse(is.na(gsl$code), gsl$code5ltr, gsl$code)
  gsl$code <- ifelse(is.na(gsl$code), gsl$pipeline_code, gsl$code)
  gsl <- subset(gsl, !is.na(gsl$code))
  gsl$code <- toupper(gsl$code)
  gsl <- gsl[c('code', 'english_name', 'taxa')]
      
  addon <- data.frame(
  stringsAsFactors = FALSE,
              code = c("ZZ", "XHUMAN"),
      english_name = c("Noise", "Human"),
              taxa = c("Noise", "Human")
  )
  gsl <- rbind(gsl, addon)

  
  
  #list files to be moved
  files_labels <- list.files(path_input, pattern = '*.txt', recursive = FALSE, ignore.case = TRUE, full.names = TRUE)
  files_labels <- as.data.frame(files_labels)
  names(files_labels) <- 'filename'
  
  #split the names to get the species code
  files_labels_bits <- as.data.frame(str_split_fixed(basename(files_labels$filename), pattern = '-|\\.', n = Inf), stringsAsFactors = FALSE)
  files_labels$spcode <- files_labels_bits$V6

  #check and move each file
  for(i in 1:NROW(files_labels)) {
    #i <- 1
    labfile <- files_labels$filename[i]
    this_sp <- files_labels$spcode[i]
    this_name <- gsl$english_name[gsl$code == this_sp]
    
    #verify species code
    if(!this_sp %in% gsl$code) stop('Unexpeced species code:',labfile)
    
    #get taxa
    taxon <- gsl$taxa[gsl$code==this_sp]

    #create folder name
    this_folder <- NA
    this_folder <- ifelse(taxon == 'Mammals', file.path(path_library, "labels_call_level/mammals", paste0(this_sp, '-', this_name)), this_folder)
    this_folder <- ifelse(taxon == 'Birds', file.path(path_library, "labels_call_level/Birds - Europe", paste0(this_sp, '-', this_name)), this_folder)
    this_folder <- ifelse(taxon == 'Amphibians', file.path(path_library, "labels_call_level/amphibians", paste0(this_sp, '-', this_name)), this_folder)
    this_folder <- ifelse(taxon == 'Human', file.path(path_library, "labels_call_level/human", paste0(this_sp, '-', this_name)), this_folder)
    this_folder <- ifelse(taxon == 'Noise', file.path(path_library, "labels_call_level/noise - Europe", paste0(this_sp, '-', this_name)), this_folder)

    #check if folder exists - if not, create it
    if(!dir.exists(this_folder)) {
      dir.create(this_folder, recursive = TRUE)
      cat('Created: ',this_folder, '\n')
    } 

    #move label file
    from_lab <- labfile
    to_lab <- file.path(this_folder, basename(labfile))
    status_lab <- file.rename(from_lab, to_lab)
    
    #move audio file
    from_audio <- gsub(".txt", ".mp3", labfile)
    if(!file.exists(from_audio)) from_audio <- gsub(".txt", ".wav", labfile)
    to_audio <- file.path(this_folder, basename(from_audio))
    status_audio <- file.rename(from_audio, to_audio)
    
    if(!all(c(status_lab, status_audio))) stop('Problems processing files for:',labfile)
    
    rm(list=c('from_lab', 'to_lab', 'from_audio', 'to_audio','this_folder','taxon','labfile', 'this_sp','this_name', 'status_lab', 'status_audio'))
  }
}


