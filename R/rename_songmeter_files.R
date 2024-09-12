#' Rename SongMeter audio files to audio library format
#' 
#' @description
#' Takes the standard format of a Wildlife Acoustics SongMeter audio file and 
#' converts to BTO audible recordings audio library format.
#' 
#' @details
#' SongMeter audio files typically are named PREFIX_YYYYMMDD_HHMMSS. Unless 
#' preconfigured the PREFIX is the device code and not human-friendly for 
#' subsequent work. This function will rename files in the format:
#' YYYYMMDD-HHMMSS-location-recordist-equipment.wav
#' where location and recordist are provided in the function call.
#' 
#' @param location, string, a site name of lat-long string. No dashes permitted 
#' as these are delimiters in the main file name format.
#' @param recordist, string, name of the recordist (or BTO if not provided).
#' 
#' @import rstudioapi
#' @import stringr
#' 
#' @export
#' 
rename_songmeter_files <- function(location, recordist = 'BTO') {
  #folder to scan 
  folder <- rstudioapi::selectDirectory()
  
  #get the list of audio files
  audios <- list.files(path = folder, pattern = "*.wav", full.names = TRUE)
  
  #iterate over files
  for(i in 1:length(audios)) {
    
    thisfile <- basename(audios[i])
    thisfolder <- dirname(audios[i])
    
    #split the existing file name - should be suffix, date, time, extension
    bits <- stringr::str_split_fixed(thisfile,"_|\\.",4)
    
    #create the new file name
    newname <- file.path(thisfolder, paste0(bits[,2], '-', bits[,3], location, '-', recordist, '-XM.' , bits[,4]))
    file.rename(from = audios[i], to = newname)
    
  }
}