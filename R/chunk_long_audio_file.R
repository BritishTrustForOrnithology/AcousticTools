#' Chunk-up a long audio file
#' 
#' @details Sometimes it is useful to break a long file into smaller chunks. 
#' This function aims to do that. For example, splitting a 4:06 file into 24 
#' 10-minute chunks (with truncate = TRUE, or with an final 6 minute file with 
#' truncate = FALSE. Note that audio files must start YYYYMMDD-HHMMSS otherwise 
#' will be skipped. If files are not in this format see the 
#' AcousticTools::rename... functions
#'
#' @param path_in = path to folder containing wav files to chunk up
#' @param path_out = path to folder where chunks will be written out
#' @param chunk_duration = required duration in seconds
#' @param truncate = TRUE/FALSE, whether to drop any final incomplete chunk
#' 
#'
#' @import tuneR
#' @import lubridate
#' 
#' @export
#' 
chunk_long_audio_file <- function(path_in, path_out, chunk_duration, truncate = TRUE) {
  if(!dir.exists(path_in)) stop("path_in does not exist")
  if(!is.numeric(chunk_duration)) stop("chunk duration must be numeric")
  
  #make output folder if required
  if(!dir.exists(path_out)) dir.create(path_out)

  #get list of wavs to chunk up
  wavs <- list.files(path_in, pattern = 'wav', ignore.case = TRUE)
  if(length(wavs)==0) stop('No wav files at path_in')
    
  #iterate over original wavs
  for(i in 1:length(wavs)) {
    #i <- 1
    this_wav <- wavs[i]
    
    #check the WAV is in the required format, with leading datetime format
    if(is.na(strptime(substr(this_wav, 1, 15), format = "%Y%m%d-%H%M%S"))) {
      warning('Filename unexpected format, skipping:', this_wav)
      next()
    }

    #get the start time of the recording
    start_datetime <- as.POSIXct(substr(this_wav,1,15), format = '%Y%m%d-%H%M%S')

    #find out how long the recording is and how many chunks of required length
    wav_info <- readWave(file.path(path_in,this_wav), header = TRUE)  
    duration_s <- wav_info$samples/wav_info$sample.rate
    #chunk count with/without truncation of incomplete final chunk
    n_chunks <- ifelse(truncate == TRUE, floor(duration_s/chunk_duration), ceiling(duration_s/chunk_duration))
    
    #iterate: read chunk, determine new start time, write out wav with new starttime
    #initialise starting values
    start <- 0
    chunk_datetime <- start_datetime
    for(s in 1:n_chunks) {
      cat('chunk', s,'\n')
      chunk <- readWave(file.path(path_in,this_wav), 
                        from = start, 
                        to = start + chunk_duration, 
                        units = 'seconds')
      
      #construct new filename
      chunk_name <- paste0(format(chunk_datetime, '%Y%m%d-%H%M%S'), 
                           substr(this_wav,16,nchar(this_wav)))
      file_chunk <- file.path(path_out, chunk_name)  
      
      #save chunk
      writeWave(chunk, filename = file_chunk)
      
      #tidy up
      rm(chunk)
      
      #update start and datetime
      start <- start+chunk_duration
      chunk_datetime <- chunk_datetime+chunk_duration
    
    } #end chunk loop
    
  } #end wav loop
}
