#' Read BirdNET species list
#' 
#' @description
#' Get the full species list supported by the current version of BirdNET. Requires 
#' a local installation of BirdNET.
#' 
#' @param path_birdnet, string, the path to where BirdNET Analyzer is installed, 
#' e.g. "F:/BirdNET-Analyzer"
#' @param version, numeric, the BirdNET version to check, e.g. 2.4
#' 
#' @export
#' 
get_birdnet_splist <- function(path_birdnet = 'F:/BirdNET-Analyzer', version = 2.4) {
  path_checkpoint <- file.path(path_birdnet, 'checkpoints', paste0("V",version), paste0('BirdNET_GLOBAL_6K_V', version, '_Labels.txt'))
  birdnet_splist <- read.delim(path_checkpoint, 
                      sep = "_", 
                      col.names = c("scientific_name", "english_name"))
  cat('Read', nrow(birdnet_splist), 'species entries\n')
  return(birdnet_splist)
}
