#' Get Audio File Duration
#' 
#' @description
#' Get the duration (in seconds) of an audio file without reading the whole file.
#' 
#' @details 
#' Reading the whole of a large file simply to find our how long it is can be 
#' inefficient. This function uses the tuneR function to read the header of 
#' a wav file and uses the information therein to calculate the file's duration.
#' 
#' @import tuneR
#' 
#' @param wavfile = the full path and filename of a wav file
#' 
#' @return a list containing the file's sample rate, duration (seconds) and a 
#' boolean flag for if the file is corrupt.
#' 
get_duration <- function(wavfile) {
  
  #try to read the wav file using readWave
  wav <- tryCatch({
    tuneR::readWave(wavfile, header = TRUE)
  }, warning = function(w) {
    message("Warning: ", conditionMessage(w))
    NA
  }, error = function(e) {
    message("Error: ", conditionMessage(e))
    NA
    # You can handle the error here if needed
  }, finally = {
    #message("Finished attempt to readWave.")
    # Code in the finally block will always be executed
  })
  
  if(!anyNA(wav)) {
    sr <- wav$sample.rate
    duration <- round(wav$samples / wav$sample.rate, 2)
    status <- FALSE
  }  
  if(anyNA(wav)) {
    sr <- NA
    duration <- NA
    status <- TRUE
  }  
  
  out <- list(sample_rate = sr, duration = duration, corrupt = status)  
  return(out)
}
