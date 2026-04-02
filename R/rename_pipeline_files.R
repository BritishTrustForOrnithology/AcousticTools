#' Rename Pipeline audio files
#' 
#' @description
#' Takes the standard format of filename produced by the Acoustic Pipeline and 
#' converts to BTO audible recordings audio library format. Optionally can also 
#' rename label (.txt) files for instances where labelling was done prior to 
#' renaming of the audio file.
#' 
#' @details
#' Pipeline files are of format: YYYYMMDD_HHMMSS-(latitude+longitude)-SUFFIX, 
#' where SUFFIX is the name of the original file from which the extract was made.
#' This function will rename files in the format:
#' YYYYMMDD-HHMMSS-location-recordist-PREFIX.wav
#' where location is parsed from the lat-long and recordist is  provided in the 
#' function call (but BTOAP by default). The folder of audio files to be renamed 
#' can be specified using a GUI interface or passed as a parameter in the 
#' function call. 
#' 
#' @param folder, string. Optional. Path to the audio files to be renamed. Can 
#' be omitted and GUI pop-up used instead
#' @param recordist, string, name of the recordist (BTOAP by default).
#' @param species_code, string, NULL or a a species code to be inserted in the filename
#' @param include_subfolders, bool, whether audio files in subfolders should be 
#' included, default FALSE
#' @param include_label_file, bool, whether to rename label files in same folder
#' 
#' @import rstudioapi
#' @import stringr
#' @import tools
#' 
#' @export
#' 
rename_pipeline_files <- function(folder = NULL, 
                                   recordist = 'BTOAP', 
                                   species_code = NULL,
                                   include_subfolders = FALSE,
                                   include_label_files = TRUE
) {
  #check incoming params for problems
  if(is.null(recordist)) stop('recordist must be provided')
  if(!is.null(species_code)) {
    if(grepl('-', species_code)) stop('species_code cannot contain hyphens')
  }
  if(grepl('-', recordist)) stop('recordist cannot contain hyphens')
  if(!is.null(folder)) {
    if(!dir.exists(folder)) stop('selected folder does not exist')
  } 
  
  #GUI select folder to scan 
  if(is.null(folder)) folder <- rstudioapi::selectDirectory()
  
  #get the list of audio files
  audios <- list.files(path = folder, pattern = "*.wav", full.names = TRUE, recursive = include_subfolders, ignore.case = TRUE)
  if(length(audios)==0) stop('No wav files in selected folder')
  
  if(include_label_files==TRUE) {
    labfiles <- list.files(path = folder, pattern = "*.txt", full.names = TRUE, recursive = include_subfolders)
    audios <- c(audios, labfiles)
  }
  
  
  #iterate over files
  for(i in 1:length(audios)) {
    
    thisfile <- basename(audios[i])
    thisfolder <- dirname(audios[i])
    

    #extract bits, format latlong and create the new file name
    thisfile <- gsub("(", "N", thisfile, fixed = TRUE)
    thisfile <- gsub(")", "", thisfile, fixed = TRUE)
    thisfile <- gsub("~", ".", thisfile, fixed = TRUE)
    thisfile <- gsub("+.", "+0.", thisfile, fixed = TRUE)
    thisfile <- gsub("+-", "W", thisfile, fixed = TRUE)
    thisfile <- gsub("+", "E", thisfile, fixed = TRUE)
    date <- stringr::str_split_fixed(thisfile, "_|-", Inf)[1]
    time <- stringr::str_split_fixed(thisfile, "_|-", Inf)[2]
    latlong <- stringr::str_split_fixed(thisfile, "_|-", Inf)[3]
    ext <- tools::file_ext(thisfile)
    if(is.null(species_code)) new <- paste0(date, '-', time, '-', latlong, '-', recordist, '-XM.', ext)
    if(!is.null(species_code)) new <- paste0(date, '-', time, '-', latlong, '-', recordist, '-XM-',species_code,'.', ext)
    cat(i, new, "\n")
    
    newname <- file.path(thisfolder, new)
    #check if newname exists - stop if so as this should not happen
    if(file.exists(newname)) {
      stop("A file already exists with the proposed newname.\nOld name = ",audios[i],"\nNew name = ",newname,"\nProceeding will cause files to be overwritten so process is terminated")
    }
    file.rename(from = audios[i], to = newname)
    
  }
}