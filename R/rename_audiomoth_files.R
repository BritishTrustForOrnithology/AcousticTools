#' Rename AudioMoth audio files to audio library format
#' 
#' @description
#' Takes the standard format of an AudioMoth audio file and 
#' converts to BTO audible recordings audio library format.
#' 
#' @details
#' AudioMoth audio files typically are named YYYYMMDD_HHMMSS. In large projects 
#' it is recommended to rename files to include location information to avoid 
#' downstream confusion. This function will rename files in the format:
#' YYYYMMDD-HHMMSS-location-recordist-equipment.wav
#' where location and recordist are provided in the function call.
#' 
#' @param location, string, a site name or lat-long string. No dashes permitted 
#' as these are delimiters in the main file name format.
#' @param recordist, string, name of the recordist (or BTO if not provided).
#' @param include_subfolders, bool, whether audio files in subfolders should be included, default FALSE
#' 
#' @import rstudioapi
#' @import stringr
#' 
#' @export
#' 
rename_audiomoth_files <- function(location, recordist = 'BTO', include_subfolders = FALSE) {
  #check incoming params for problems
  if(is.null(location)) stop('location must be provided')
  if(is.null(recordist)) stop('location must be provided')
  if(grepl('-', location)) stop('location cannot contain hyphens')
  if(grepl('-', recordist)) stop('recordist cannot contain hyphens')
  
  
  #folder to scan 
  folder <- rstudioapi::selectDirectory()
  
  #get the list of audio files
  audios <- list.files(path = folder, pattern = "*.wav|*.WAV", full.names = TRUE, recursive = include_subfolders)
  if(length(audios)==0) stop('No wav files in selected folder')
  
  
  #iterate over files
  for(i in 1:length(audios)) {
    
    thisfile <- basename(audios[i])
    thisfolder <- dirname(audios[i])
    
    #split the existing file name - should be date, time, extension
    bits <- stringr::str_split_fixed(thisfile,"_|\\.",Inf)
    if(dim(bits)[2] != 3) stop('Existing file name format has more components than expected. Expecting YYYYMMDD_HHMMSS.WAV')
    
    #create the new file name
    newname <- file.path(thisfolder, paste0(bits[,1], '-', bits[,2],'-', location, '-', recordist, '-AM.' , bits[,3]))
    #check if newname exists - stop if so as this should not happen
    if(file.exists(newname)) {
      stop("A file already exists with the proposed new name.\nOld name = ",newname,"\nNew name = ",newname,"\nProceeding will cause files to be overwritten so process is terminated")
    }
    file.rename(from = audios[i], to = newname)
  }
}
