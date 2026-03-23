#' Error check label files
#' 
#' @description Given a folder of labelled audio files the function does some basic 
#' checks that the label files meet required standards. Currently the function checks:
#' 
#' * if the file can be read (correct tab-delimited format)
#' * if the label start is numeric
#' * if the label end is numeric
#' * if all species codes are recognised
#' * if all call type codes are recognised
#' * if channels are present, if they are correctly specified
#' * if there is a matching wav or mp3 file
#' 
#' @return A dataframe listing the label files checked and 1/0 indication of pass status for each check.
#' 
#' @import BTOTools
#' @import stringr
#' @import rstudioapi
#' 
#' @param folder = Path to a folder containing the labels to check. Can be NULL and use RStudio folder dialogue instead.
#' @param calltypes = an optional list of allowed call types to add to the inbuilt defaults ('F','S','C','D','Z'). Useful if checking within-species call-type based labels
#' 
#' @examples
#' check_labels(calltypes = c(1,2,3,4))
#' 
#' 
#' @export
#' 

check_labels <- function(folder = NULL, calltypes = NULL) {
  if(is.null(folder)) {
    path_input <- rstudioapi::selectDirectory(caption = 'Select Dir containing clips to organise')
  } else path_input = folder
  if(is.null(calltypes)) {
    calltypes <- c('F','S','C','D','Z')
  } else {
    calltypes <- c(c('F','S','C','D','Z'), as.character(calltypes))
  }
  
  #prepare the species dictionary as used for labelling
  data("global_species_lookup",package = 'BTOTools')
  gsl <- global_species_lookup
  #deal with Pied Wag slash
  gsl$english_name <- ifelse(gsl$code2ltr=='PW', 'Pied-White Wagtail', gsl$english_name)
  #replace some special codes 
  gsl$english_name <- ifelse(gsl$code2ltr=='XX', 'Unidentified', gsl$english_name)
  # #convert dot to underscore as used in filenames
  gsl$code2ltr <- gsub(pattern = '.', '_', gsl$code2ltr, fixed = TRUE)
  #remove numeric mammal codes
  gsl$code2ltr <- ifelse(!is.na(suppressWarnings(as.numeric(gsl$code2ltr))), NA, gsl$code2ltr)
  gsl$code <- gsl$code2ltr
  gsl$code <- ifelse(is.na(gsl$code), gsl$code5ltr, gsl$code)
  gsl$code <- ifelse(is.na(gsl$code), gsl$pipeline_code, gsl$code)
  gsl <- subset(gsl, !is.na(gsl$code))
  gsl$code <- toupper(gsl$code)
  
  #get the list of label files
  files_labels <- list.files(path_input, pattern = '*.txt', recursive = FALSE, ignore.case = TRUE, full.names = TRUE)
  
  #check label contents for errors
  error_check <- list()
  for(i in 1:length(files_labels)) {
    #i <- 1
    file_format <- 1
    start_is_numeric <- 0
    end_is_numeric <- 0
    sppcodes_accepted <- 0
    callcodes_accepted <- 0
    channels_accepted <- 0
    
    tryCatch({
      lab <- read.csv(files_labels[i], sep='\t', header = FALSE, col.names = c('start','end','lab'))
    }, error = function(e) {
      file_format <<- 0
    }, warning = function(w) {
      file_format <<- 0
    })
    
    if (file_format == 1) {
      
      # check the start and end are properly populated
      start_is_numeric <- as.numeric(all(is.numeric(lab$start)))
      end_is_numeric <- as.numeric(all(is.numeric(lab$end)))
      
      # check the label string...
      bits <- stringr::str_split_fixed(lab$lab, "-", n=Inf)
      # are all species codes recognised?
      sppcodes_accepted <- as.numeric(all(bits[,1] %in% c(gsl$code, 'ZZ','XHUMAN')))
      # are all call type codes recognised?
      callcodes_accepted <- as.numeric(all(bits[,2] %in% calltypes))
      # if channels are provided, are they acceptable?
      if (dim(bits)[2] == 3) {
        channels_accepted <- as.numeric(all(bits[,3] %in% c('10','01','11')))
      } else {
        channels_accepted <- 1
      }
      
    }
    
    # check for matching audio file (runs regardless of file_corrupt)
    file_wav <- file.exists(gsub(".txt", ".wav", files_labels[i]))
    file_WAV <- file.exists(gsub(".txt", ".WAV", files_labels[i]))
    file_mp3 <- file.exists(gsub(".txt", ".mp3", files_labels[i]))
    
    matching_audio <- as.numeric(any(file_wav, file_WAV, file_mp3))
    
    error_check[[i]] <- data.frame(file = files_labels[i],
                                   file_format,
                                   start_is_numeric,
                                   end_is_numeric,
                                   sppcodes_accepted,
                                   callcodes_accepted,
                                   channels_accepted,
                                   matching_audio)
    
    
  }
  
  error_check <- do.call(rbind, error_check)
  error_check$has_errors <- abs(1-apply(error_check[,2:7],1,min))
  haserrors <- sum(error_check$has_errors)
  message('Files with errors = ',haserrors)
  
  return(error_check)
}

