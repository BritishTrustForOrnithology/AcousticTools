#' Create fixed position label files
#' 
#' @description
#' For every audio file in a folder, create a label file of the same name with 
#' the specified label position and code.
#' 
#' @details
#' This function is useful as a preparatory step when labelling audio files that 
#' have been pre-verified. For a given folder of audio files the function will 
#' make an label file with the same base name, using the provided position 
#' (seconds) and label. Optionally the user can specify to overwrite any existing label 
#' files, and to scan recursively.
#' 
#' @param position = the position (in seconds) where the label should be created
#' @param label = the label code to be assigned at position
#' @param overwrite = whether to overwrite existing label files (default FALSE)
#' @param subfolders = whether to make labels for audio in subfolders (default TRUE)
#'
#' @import rstudioapi
#' 
#' @export

make_fixed_labels <- function(position = NULL, label = NULL, overwrite = FALSE, subfolders = TRUE) {
  if(is.null(position)) stop("position must be supplied")
  if(!is.numeric(position)) stop('position must be numeric')
  if(is.null(label)) stop("label must be supplied")
  
  path_clips <- rstudioapi::selectDirectory()
  
  label_string <- paste(position,position,label, sep = "\t")
  
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
    writeLines(label_string, con = txts[t])
  }
}

