#' Error check label files
#' 
#' @description Given a folder of labelled audio files the function does some basic 
#' checks that the label files meet required standards. Currently the function checks:
#' 
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
#' 
#' 
#' 
#' @export
#' 

check_labels <- function(folder = NULL) {
  if(is.null(folder)) {
    path_input <- rstudioapi::selectDirectory(caption = 'Select Dir containing clips to organise')
  } else path_input = folder
  
  
  #prepare the species dictionary as used for labelling
  data("global_species_lookup")
  gsp <- global_species_lookup
  #deal with Pied Wag slash
  gsp$english_name <- ifelse(gsp$code2ltr=='PW', 'Pied-White Wagtail', gsp$english_name)
  #replace some special codes 
  gsp$english_name <- ifelse(gsp$code2ltr=='XX', 'Unidentified', gsp$english_name)
  # #convert dot to underscore as used in filenames
  gsp$code2ltr <- gsub(pattern = '.', '_', gsp$code2ltr, fixed = TRUE)
  #remove numeric mammal codes
  gsp$code2ltr <- ifelse(!is.na(suppressWarnings(as.numeric(gsp$code2ltr))), NA, gsp$code2ltr)
  gsp$code <- gsp$code2ltr
  gsp$code <- ifelse(is.na(gsp$code), gsp$code5ltr, gsp$code)
  gsp$code <- ifelse(is.na(gsp$code), gsp$pipeline_code, gsp$code)
  gsp <- subset(gsp, !is.na(gsp$code))
  gsp$code <- toupper(gsp$code)
  
  #get the list of label files
  files_labels <- list.files(path_input, pattern = '*.txt', recursive = FALSE, ignore.case = TRUE, full.names = TRUE)
  
  #check label contents for errors
  error_check <- list()
  for(i in 1:length(files_labels)) {
    #i <- 1
    lab <- read.csv(files_labels[i], sep='\t', header = FALSE, col.names = c('start','end','lab'))
    #check the start and end are properly populated
    start_is_numeric <- as.numeric(all(is.numeric(lab$start)))
    end_is_numeric <- as.numeric(all(is.numeric(lab$end)))
    
    #check the label string...
    bits <- stringr::str_split_fixed(lab$lab, "-", n=Inf)
    #are all species codes recognised?
    sppcodes_accepted <- as.numeric(all(bits[,1] %in% c(gsp$code, 'ZZ','XHUMAN')))
    #are all call type codes recognised?
    callcodes_accepted <- as.numeric(all(bits[,2] %in% c('F','S','C','D','Z')))
    #if channels are provided, are they acceptable?
    if(dim(bits)[2] == 3) {
      channels_accepted <- as.numeric(all(bits[,3] %in% c('10','01','11')))
    } else channels_accepted <- 1
    
    #check for matching audio file
    file_wav <- file.exists(gsub(".txt",".wav",files_labels[i]))
    file_WAV <- file.exists(gsub(".txt",".WAV",files_labels[i]))
    file_mp3 <- file.exists(gsub(".txt",".mp3",files_labels[i]))
    
    matching_audio <- as.numeric(any(file_wav, file_WAV, file_mp3))
    
    
    error_check[[i]] <- data.frame(file = files_labels[i],
                                   start_is_numeric, 
                                   end_is_numeric, 
                                   sppcodes_accepted, 
                                   callcodes_accepted, 
                                   channels_accepted,
                                   matching_audio)
  }
  
  error_check <- do.call(rbind, error_check)
  error_check$nerrors <- apply(error_check[,2:7],1,sum)
  error_check$nerrors <- 6-error_check$nerrors
  haserrors <- sum(error_check$nerrors)
  message('Number of errors found = ',haserrors)
  
  return(error_check)
}

