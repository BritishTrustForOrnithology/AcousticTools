#' Rename audio files on SD card using creation date
#' 
#' @description
#' For audio files named consecutively, rename in a Pipeline-friendly format 
#' using the recordings' creation datetime, which equates to the start datetime
#' of the recording.
#' 
#' @details
#' This function will rename files in the format YYYYMMDD-HHMMSS-suffix.wav/mp3/flac, 
#' where suffix is supplied in the function call. It uses the creation date of 
#' the file. If the file is still on a memory card this will be the start time 
#' of the recording. Beware if the file has been copied to a new location the 
#' creation date no longer equates to the start time!
#' 
#' 
#' @param suffix, string, detailing a string to add to the date time prefix. 
#' A typical use case will be a site name.
#' @param sep, string, the separator to use between datetime and suffix components. 
#' Defaults to a hyphen. Underscore is another valid option.
#' 
#' @import rstudioapi
#' @import tools
#' 
#' @export
#' 
rename_by_creation_date <- function(suffix, sep = "-") {
  #folder to scan 
  folder <- rstudioapi::selectDirectory()
  
  #if suffix does not start with a separator, add one
  suffix <- ifelse(substr(suffix,1,1) != sep, paste0(sep, suffix), suffix)  
  
  #get the list of audio files
  audios <- list.files(path = folder, pattern = "*.flac|*.mp3|*.wav", full.names = TRUE)
  
  #iterate over files
  for(i in 1:length(audios)) {
    
    #get the file extension
    ext <- tools::file_ext(audios[i])
    
    #get the file creation date
    ct <- file.info(audios[i])$ctime
    
    #turn this into a string with the appropriate seperator
    dt <- gsub(" ",sep,gsub("-|:","",substr(ct,1,19)))
    
    #concatenate the new file name, folder/date[sep]time[sep]suffix.ext
    newname <- file.path(folder, paste0(dt,suffix,".",ext))
    file.rename(audios[i], newname)
  }
}