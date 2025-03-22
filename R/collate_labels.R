#' Scan a folder and collate any label file contents
#'
#' @details Scan a folder to find Audacity label files and collate any labels. 
#' 
#' 
#' @param folder = str, folder to be scanned
#' @param include_subfolders = bool, whether to scan recursively
#'
#' @return A dataframe with columns start, end, label, label_file name and original_audio_file name.
#'
#' @export
#'
collate_labels <- function(folder = 'E:/Data', include_subfolders = FALSE) {
  if(!dir.exists(folder)) stop('Folder does not exist')
  if(!is.logical(include_subfolders)) stop('include_subfolders must be TRUE or FALSE')
  txts <- list.files(path = folder, pattern = "*.txt", full.names = TRUE, recursive = include_subfolders)
  labels <- list()
  for(t in 1:length(txts)) {
    #Use try catch in case there are txt files of incorrect format
    tryCatch(
      {
        labs <- read.csv(txts[t], header = FALSE, sep = '\t', col.names = c('start','end','label'))
        labs$label_file <- txts[t]
        labs$original_audio_file <- gsub(".txt", ".wav",txts[t])
        labels[[t]] <- labs
        rm(labs)
      },
      warning = function(w) {
        warning('wrong file format: ', txts[t])
        #next
      },
      error = function(e) {
        message('wrong file format: ', txts[t])
        #next
      }
    )
  }
  labels <- do.call(rbind, labels)
  return(labels)
}