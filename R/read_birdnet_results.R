#' Read and collate BirdNET outputs
#' 
#' @description
#' This function can be used to read the different versions of BirdNET outputs and 
#' compile them into a standard output format. It does not apply any filtering 
#' beyond that already done in the BirdNET GUI.
#' 
#' @details
#' BirdNET produces 5 format types, with some having the option to produce a 
#' collated multi-wav output. The naming conventions for these are: 
#' 
#' * Audacity format: single file = WAVNAME.BirdNET.results.txt; No combined 
#' output made.
#' * R format: single = WAVNAME.BirdNET.results.r.csv; Combined format = 
#' BirdNET_RTable.csv.
#' * CSV format: single WAVNAME.BirdNET.results.csv; Combined format = 
#' BirdNET_CombinedTable.csv.
#' 
#' The function assumes that ONLY ONE FORMAT is present in the passed folder, 
#' although if a combined file is present, this is read preferentially to the individual files. 
#' 
#' @param folder = the full path to the folder for which results are to be collated/read
#' @param subfolders = bool, whether to include subfolders (default = TRUE)
#'
#' @return A dataframe of BirdNET detections with the following format. Note that 
#' the last eight fields are only output by the R method:
#' * original_wav = the full path to the origina audio file
#' * start = start position in seconds of the detection
#' * end = end position in seconds of the detection
#' * birdnet_scientific_name = the scientific name of the species detected, following the taxonomy used by BirdNET
#' * birdnet_english_name = the English name of the species detected, following the taxonomy used by BirdNET
#' * score = BirdNET confidence score
#' * lat = latitude (if supplied) for filtering
#' * lon = longitude (if supplied) for filtering
#' * week = week (if supplied) for filtering
#' * overlap = overlap between chunks
#' * sensitivity = sensitivity used in prediction
#' * min_conf = minimum confidence filter applied
#' * species_list = custom species list (if supplied)
#' * model = classifier model version
#'
#' @import stringr
#' 
#' @export
#' 
read_birdnet_results <- function(folder, subfolders = TRUE)  {
  #check params
  if(!dir.exists(folder)) stop('folder does not exist')
  
  filepaths <- list.files(path = folder, pattern = "*.csv|*.txt", 
             recursive = subfolders, 
             full.names = TRUE)
  
  #preferred order of columns
  pref_cols <- c("original_wav",
                 "start", "end",
                 "birdnet_scientific_name", "birdnet_english_name",
                 "score",
                 "lat", "lon",
                 "week", "overlap", "sensitivity", "min_conf", "species_list", "model")
  
  method <- NA
  
  #Combined methods
  #CSV
  if(any(grepl("BirdNET_CombinedTable.csv", filepaths))) {
    message("Folder contains a BirdNET combined CSV file")
    method <- 'CSV_ALL'
  }
  
  #R
  if(any(grepl("BirdNET_RTable.csv", filepaths))) {
    message("Folder contains a BirdNET combined R file")
    method <- 'R_ALL'
  }
  
  #Individual files methods
  #Audacity
  if(is.na(method)) {
    if(any(grepl('*.BirdNET.results.txt', filepaths))) {
      message("Folder contains individual Audacity files")
      method <- 'AUD_IND'
    }
  }
  
  #CSV
  if(is.na(method)) {
    if(any(grepl('*.BirdNET.results.csv', filepaths))) {
      message("Folder contains individual CSV files")
      method <- 'CSV_IND'
    }
  }
  
  #R
  if(is.na(method)) {
    if(any(grepl('*.BirdNET.results.r.csv', filepaths))) {
      message("Folder contains individual R files")
      method <- 'R_IND'
    }
  }
  
  if(is.na(method)) stop('Unable to find any recognised BirdNET outputs')
  
  #for each method, read and prep
  if(method == 'CSV_ALL') {
    file <- filepaths[grepl("BirdNET_CombinedTable.csv", filepaths)]
    detections <- read.csv(file)
    names(detections) <- c('start','end','birdnet_scientific_name', 'birdnet_english_name', 'score', 'original_wav')
    detections$lat <- NA
    detections$lon <- NA
    detections$week <- NA
    detections$overlap <- NA
    detections$sensitivity <- NA
    detections$min_conf <- NA
    detections$species_list <- NA
    detections$model <- NA
    
    #fix col order
    detections <- detections[,pref_cols]
  }
  
  if(method == 'CSV_IND') {
    #read all individual csvs
    detections <- do.call(rbind, lapply(filepaths, read.csv))
    #standardise column names
    names(detections) <- c('start','end','birdnet_scientific_name', 'birdnet_english_name', 'score', 'original_wav')
    detections$lat <- NA
    detections$lon <- NA
    detections$week <- NA
    detections$overlap <- NA
    detections$sensitivity <- NA
    detections$min_conf <- NA
    detections$species_list <- NA
    detections$model <- NA
    
    #fix col order
    detections <- detections[, pref_cols]
  }
  
  if(method == 'R_ALL') {
    file <- filepaths[grepl("BirdNET_RTable.csv", filepaths)]
    detections <- read.csv(file)
    names(detections)[1] <-'original_wav'
    names(detections)[4] <- 'birdnet_scientific_name'
    names(detections)[5] <- 'birdnet_english_name'
    names(detections)[6] <- 'score'
    
    #fix col order
    detections <- detections[, pref_cols]
  }
  
  if(method == 'R_IND') {
    #read all individual csvs
    detections <- do.call(rbind, lapply(filepaths, read.csv))
    #standardise column names
    names(detections)[1] <-'original_wav'
    names(detections)[4] <- 'birdnet_scientific_name'
    names(detections)[5] <- 'birdnet_english_name'
    names(detections)[6] <- 'score'
    
    #fix col order
    detections <- detections[, pref_cols]
  }
  
  if(method == 'AUD_IND') {
    detections <- list()
    for(i in 1:length(filepaths)) {
      temp <- read.csv(filepaths[i], sep='\t', header = FALSE, col.names = c('start', 'end','name','score'))
      
      #split the label name into sci and Eng names
      bits <- stringr::str_split_fixed(temp$name, ',', n = 2)
      temp$birdnet_scientific_name <- bits[,1]
      temp$birdnet_english_name <- trimws(bits[,2])
      temp$name <- NULL  
      
      temp$original_wav <- gsub('.BirdNET.results.txt','.wav', filepaths[i])
      detections[[i]] <- temp
      rm(temp)
      rm(bits)
    }
    detections <- do.call(rbind, detections)  
    detections$lat <- NA
    detections$lon <- NA
    detections$week <- NA
    detections$overlap <- NA
    detections$sensitivity <- NA
    detections$min_conf <- NA
    detections$species_list <- NA
    detections$model <- NA
    
    #fix col order
    detections <- detections[, pref_cols]
    
    
  }

  return(detections)
}



