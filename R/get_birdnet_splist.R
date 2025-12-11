#' Read BirdNET labels file
#' 
#' @description
#' Get the full species list supported by the current version of BirdNET. Requires 
#' a local installation of BirdNET. As the location of the label file has changed 
#' in some versions, this function requires you to provide the path or navigate 
#' to the file, ending like ".../checkpoints/V2.4/BirdNET_GLOBAL_6K_V2.4_Labels.txt"  
#' 
#' @param file = string, the path to a labels file
#' 
#' @examples
#' \dontrun{
#' get_birdnet_splist(file = "C:/Users/simon.gillings/AppData/Local/Programs/BirdNET-Analyzer/_internal/birdnet_analyzer/checkpoints/V2.4/BirdNET_GLOBAL_6K_V2.4_Labels.txt")
#' }
#' @export
#' 
get_birdnet_splist <- function(file = NULL) {
  if(is.null(file)) {
    file_birdnet_labels <- rstudioapi::selectFile(caption = 'Select a BirdNET labels file', filter = '_Labels.txt')
  } else {
    if(!file.exists(file)) stop('Provided file does not exist.')
    file_birdnet_labels <- file
  }
  
  birdnet_labels <- read.delim(file_birdnet_labels, 
                      sep = "_", 
                      col.names = c("scientific_name", "english_name"))
  cat('Read', nrow(birdnet_labels), 'species entries\n')
  return(birdnet_labels)
}
