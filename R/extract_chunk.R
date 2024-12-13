#' Extract audio chunk
#' 
#' @description
#' Extract short audio chunk from a longer file. Useful for extracting classifier 
#' detections from long passive audio files.
#' 
#' @param file_wav = full path to the original audio file
#' @param file_chunk = full path and filename for the output chunk
#' @param start = start time of the detection in file_wav, in seconds
#' @param end = end time of the detection in file_wav, in seconds
#' @param chunk_duration = desired length of the chunk, in seconds. See Details
#' @param verbose = should messages be printed to console?

#' @details
#' chunk_duration can be set to longer than the length of a detection. For example, 
#' for a classifier that outputs detections of length 4s, setting chunk_duration to 
#' 7 will export a 7-second chunk centred on the 4s of interest, thereby giving 
#' additional context to aid in verification, or for downstream training clips 
#' with jitter. Note that if a chunk is at the start or end of a file, silence 
#' is padded onto the start or end of the chunk to maintain chunk_duration.
#' 
#' Output directories will be created as required if not already present.

#' @author Simon Gillings

#' @export
#' 
#' @import tuneR
#' 
#' @examples
#' \dontrun{
#' extract_chunk(file_wav = 'C:/long_audio_file.wav', file_chunk = 'C:/extracts/sparrow.wav', start = 4, end = 8, chunk_duration = 7)
#' }

extract_chunk <- function(file_wav, file_chunk, start, end, chunk_duration, verbose = FALSE) {
  #validate inputs
  if(!file.exists(file_wav)) stop("file_wav does not exist")
  if(!is.numeric(start)) stop("start must be a number")
  if(start<0) stop("start cannot be a negative number")
  if(!is.numeric(end)) stop("end must be a number")
  if(!is.numeric(chunk_duration)) stop("chunk_duration must be a number")
  if(chunk_duration<=0) stop("chunk_duration must be greater than 0")
  if(chunk_duration < (end-start)) stop("chunk_duration is shorter than classifier resolution (start-end interval")
  
  
  #get info on original file
  file_info <- tuneR::readWave(filename = file_wav, header = TRUE)
  sr <- file_info$sample.rate
  duration <- ceiling(file_info$samples / file_info$sample.rate)
  bits <- file_info$bits
  
  #check start and end are within the file - if not exit function
  if(start > duration) {
    warning("Skipping because start falls outside file: ", file_chunk, ". Duration: ", duration, ". Detection start: ", start)
    return()
  }
  if(end > duration) {
    warning("Skipping because end falls outside file: ", file_chunk, ". Duration: ", duration, ". Detection end: ", end)
    return()
  }
  
  
  #make destination folder if doesn't already exist
  path_out <- dirname(file_chunk)
  if(!dir.exists(path_out)) dir.create(path_out, showWarnings = FALSE, recursive = TRUE)
  
  #get chunk start and end times, accounting for chunk_duration
  chunk_mid <- mean(c(start, end))
  chunk_start_seconds <- chunk_mid - (chunk_duration*0.5)
  chunk_end_seconds <- chunk_start_seconds + chunk_duration
  
  #deal with chunks overlapping start of file
  if(chunk_start_seconds < 0) {
    if(verbose) cat('Chunk truncated at start:', file_chunk,'\n')
    #how much silence to pad at start
    silence_length <- abs(chunk_start_seconds)
    #make empty audio chunk
    silent_samples <- rep(0, sr * silence_length)
    silence_start <- tuneR::Wave(left = silent_samples, bit = bits, samp.rate = sr)
    #NOT WORKING silence_start <- tuneR::silence(duration = silence_length, samp.rate = sr, xunit = "time", bit = bits)
    #reset chunk start
    chunk_start_seconds <- max(0, chunk_start_seconds)
  }
  
  #deal with chunks overlapping end of file
  if(chunk_end_seconds > duration) {
    if(verbose) cat('Chunk truncated at end:', file_chunk,'\n')
    #how much silence to pad at end
    silence_length <- chunk_end_seconds - duration
    #make empty audio chunk
    silent_samples <- rep(0, sr * silence_length)
    silence_end <- tuneR::Wave(left = silent_samples, bit = bits, samp.rate = sr)
    #NOT WORKING silence_end <- tuneR::silence(duration = silence_length, samp.rate = sr, xunit = "time", bit = bits)
    #reset chunk end
    chunk_end_seconds <- min(duration, chunk_end_seconds)
  }
  
  #extract a specific chunk. Note that tuneR::extractWave() should do this, but 
  #it returns the wrong number of samples (88201 instead of 88200). Previous 
  #version of verification app could not open these (incomplete end of file warning) 
  #but this is now updated to use a different pacakge in these instances. If this 
  #becomes a problem again this step can be replaced with basic indexing on the 
  #file file read with audio package
  chunk <- tuneR::readWave(filename = file_wav,
                           from = chunk_start_seconds,
                           to = chunk_end_seconds,
                           unit = "seconds")
  
  if(exists("silence_start")) {
    if(verbose) cat('Padding start\n')
    chunk <- tuneR::bind(silence_start, chunk)  
  } 
  if(exists("silence_end")) {
    if(verbose) cat('Padding end\n')
    chunk <- tuneR::bind(chunk, silence_end)  
  }
  
  #export chunk
  #if(verbose) cat('Writing: ', file_chunk, '\n')
  tuneR::writeWave(object = chunk, filename = file_chunk)
}

