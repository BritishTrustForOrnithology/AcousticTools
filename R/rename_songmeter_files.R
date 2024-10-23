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
#' where location and recordist are provided in the function call. The folder 
#' of audio files to be renamed can be specified using a GUI interface or passed 
#' as a parameter in the function call.
#' 
#' @param folder, string. Optional. Path to the audio files to be renamed. Can 
#' be omitted and GUI pop-up used instead
#' @param site, string, a site name of lat-long string. No dashes permitted 
#' as these are delimiters in the main file name format.
#' @param recordist, string, name of the recordist (or BTO if not provided).
#' @param include_subfolders, bool, whether audio files in subfolders should be 
#' included, default FALSE
#' 
#' @import rstudioapi
#' @import stringr
#' 
#' @export
#' 
rename_songmeter_files <- function(folder = NULL, site, recordist = 'BTO', include_subfolders = FALSE) {
  #check incoming params for problems
  if(is.null(site)) stop('site must be provided')
  if(is.null(recordist)) stop('recordist must be provided')
  if(grepl('-', site)) stop('site cannot contain hyphens')
  if(grepl('-', recordist)) stop('recordist cannot contain hyphens')
  if(!is.null(folder)) {
    if(!dir.exists(folder)) stop('selected folder does not exist')
  } 
  
  #GUI select folder to scan 
  if(is.null(folder)) folder <- rstudioapi::selectDirectory()
  
  #get the list of audio files
  audios <- list.files(path = folder, pattern = "*.wav", full.names = TRUE, recursive = include_subfolders)
  if(length(audios)==0) stop('No wav files in selected folder')
  
  #iterate over files
  for(i in 1:length(audios)) {
    
    thisfile <- basename(audios[i])
    thisfolder <- dirname(audios[i])
    
    #split the existing file name - should be suffix, date, time, extension
    bits <- stringr::str_split_fixed(thisfile,"_|\\.",Inf)
    if(dim(bits)[2] != 4) stop('Existing file name format has more components than expected. Expecting PREFIX_YYYYMMDD_HHMMSS.WAV')
    
    #create the new file name
    newname <- file.path(thisfolder, paste0(bits[,2], '-', bits[,3], '-', site, '-', recordist, '-XM.' , bits[,4]))
    #check if newname exists - stop if so as this should not happen
    if(file.exists(newname)) {
      stop("A file already exists with the proposed newname.\nOld name = ",audios[i],"\nNew name = ",newname,"\nProceeding will cause files to be overwritten so process is terminated")
    }
    file.rename(from = audios[i], to = newname)
    
  }
}