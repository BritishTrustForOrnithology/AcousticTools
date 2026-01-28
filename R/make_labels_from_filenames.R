#' Create fixed position label files using species in filename
#' 
#' @description
#' For every audio file in a folder, create a label file of the same name with 
#' the specified label position and inherit species code from the filename
#' 
#' @details
#' For a given folder of audio files the function will 
#' make an label file with the same base name, using the provided position 
#' (seconds). It will pick the species code from the filename and add call type 
#' provided. Optionally the user can specify to overwrite any existing label 
#' files, and to scan recursively.
#' 
#' @param position = the position (in seconds) where the label should be created
#' @param calltype = the call type code to be assigned at position
#' @param overwrite = whether to overwrite existing label files (default FALSE)
#' @param subfolders = whether to make labels for audio in subfolders (default TRUE)
#'
#' @import rstudioapi
#' 
#' @export

make_labels_from_filename <- function(position = NULL, calltype = NULL, overwrite = FALSE, subfolders = TRUE) {
  if(is.null(position)) stop("position must be supplied")
  if(!is.numeric(position)) stop('position must be numeric')
  if(is.null(calltype)) stop("calltype must be supplied")
  if(!calltype %in% c('C','S','F','Z','D')) stop("calltype must be one of C, D, F, S, Z")
  
  path_clips <- rstudioapi::selectDirectory()
  
  #label_string <- paste(position,position,label, sep = "\t")
  
  audios <- list.files(path_clips, pattern = "*.flac|*.mp3|*.wav", full.names = TRUE, recursive = subfolders)
  txts <- audios
  txts <- gsub(".wav", ".txt", txts)
  txts <- gsub(".mp3", ".txt", txts)
  txts <- gsub(".flac", ".txt", txts)
  
  #iterate over the files to make
  for(t in 1:length(txts)) {
    if(overwrite == FALSE) {
      if(file.exists(txts[t])) {
        cat(txts[t],'exists. Skipping...\n')
        next
      }
    }
    bits <- stringr::str_split_fixed(basename(txts[t]), '-|\\.', Inf)
    code <- bits[6]
    label <- ifelse(code=='ZZ', 'ZZ-Z', paste0(code,"-",calltype))
    label_string <- paste(position,position,label, sep = "\t")
    writeLines(label_string, con = txts[t])
  }
}

