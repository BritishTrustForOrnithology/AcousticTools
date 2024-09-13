#' Create empty label files
#' 
#' @description
#' For every audio file in a folder, create an empty text file of the same name 
#' ready for populating with event labels.
#' 
#' @details
#' This function is useful as a preparatory step when labelling audio files. For 
#' a given folder of audio files the function will make an empty text file with 
#' the same base name. Then, when labelling in Audacity it is quicker/safer to 
#' save the labels to the existing empty file than to type in the file name in 
#' manually. Optionally the user can specify to overwrite any existing label 
#' files, and to scan recurisvely.
#' 
#' 
#' @param overwrite = whether to overwrite existing label files (default FALSE)
#' @param subfolders = whether to make labels for audio in subfolders (default TRUE)
#'
#' @import rstudioapi
#' 
#' @export

make_empty_labels <- function(overwrite = FALSE, subfolders = TRUE) {
  path_clips <- rstudioapi::selectDirectory()
  
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
    writeLines("", con = txts[t])
  }
}

