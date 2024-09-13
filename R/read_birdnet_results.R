#' Read BirdNET output
#' 
#' @description 
#' Reads and formats the standard output of a BirdNET run.
#' 
#' @details 
#'
#' @param format = string, the format of the BirdNET output. One of Audacity, 
#' Raven or csv (currently only Audacity supported)
#' @param include_fname = boolean, whether to include the label filename in the 
#' output dataframe. This should be the same root as the audio filename so can 
#' be used to track back to the audio to which the detections relate (but note 
#' audio file extension is unknown)
#' 
#' @import stringr
#' 
#' @return a dataframe containing detections and positions in the audio file
#' 
read_birdnet_csv <- function(file, format = 'Audacity', include_fname = FALSE) {
  temp <- read.delim(file, sep = "\t", header = FALSE, col.names = c('start','end','name','score'))
  bits <- stringr::str_split_fixed(temp$name, ',', n = 2)
  temp$scientific_name <- bits[,1]
  temp$english_name <- trimws(bits[,2])
  temp$name <- NULL  
  if(include_fname == TRUE) {
    temp$label_file <- file
  }

  return(temp)
}
