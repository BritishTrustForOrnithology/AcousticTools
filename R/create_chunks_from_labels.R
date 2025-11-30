#' Create audio chunks from long labelled audio
#' 
#' @description This utility is for extracting fixed-length audio chunks from 
#' longer soundscape files on the basis of instantaneous (zero-length) labels. 
#' It assumes that labels are of the form BT-S,GT-S,ST-F, or WS-F. If no call 
#' type code is given, a 'S' code (stationary/standard) is assumed.
#' 
#' @param path_in = the folder where the audio+label files are located. If NULL, uses directory picker pop-up.
#' @param path_out = the folder where the extracted audio chunks and associated 
#' labels will be saved. If NULL, uses directory picker pop-up.
#' @param primary_spp = the species code to use for the exported file names. 
#' If NULL this will default to the first species listed in the label string
#' @param chunk_duration = desired length of the exported audio chunk
#' 
#' @export
#'
#'

create_chunks_from_labels <- function(path_in = NULL, path_out = NULL, primary_spp = NULL, 
                                      chunk_duration = 5) {
  #validate inputs
  if(is.null(path_in)) path_in <- rstudioapi::selectDirectory(caption = 'Select path for audio to process')
  if(!dir.exists(path_in)) stop("path_in does not exist")
  if(is.null(path_out)) path_out <- rstudioapi::selectDirectory(caption = "Select path for output of labelled chunks", path = path_in)
  if(!dir.exists(path_out)) stop("path_out does not exist")
  if(!is.numeric(chunk_duration)) stop('chunk duration must be a number')
  
  ################################################################################
  
  #list all label files
  files_labels <- list.files(path_in, pattern = 'txt', full.names = TRUE)
  
  #iterate over label files
  for(i in 1:length(files_labels)) {
    #i <- 2
    file1 <- files_labels[i]
    #get name of corresponding audio file
    wav1 <- gsub(".txt", ".wav",file1)
    #check audio exists
    if(!file.exists(wav1)) stop("wav file does not exist:", wav1)
    
    #find the starttime of the file for making new names
    file_start <- regmatches(basename(file1), regexpr("\\d{8}-\\d{6}", basename(file1)))
    file_start <- as.POSIXct(file_start, format = "%Y%m%d-%H%M%S")
    
    #read the labels
    labels <- read.csv(file1, sep="\t", header = FALSE, col.names = c('start','end','label'))
    
    #iterate over labels
    for(z in 1:nrow(labels)) {
      #z <- 2
      labels1 <- labels[z,]
      
      #turn the label string into a list of labels
      labs <- strsplit(gsub("^\\[|\\]$", "", labels1$label), ",")[[1]]
      
      #add default S where call type is missing
      labs <- ifelse(grepl("-", labs), labs, paste0(labs, "-S"))
      
      #get primary species
      if(is.null(primary_spp)) {
        spp_for_filename <- strsplit(labs[1],"-")[[1]][1] 
      } else spp_for_filename <- primary_spp
      
      #use file start time and the position of the label to make the detection start time
      detection_time <- format(file_start + labels1$start, "%Y%m%d-%H%M%S")
      
      #use this, the primary species, and bits of the original file name to make the new chunk's file name
      chunk_name <- paste0(detection_time, substr(basename(file1),16,nchar(basename(file1))-4), "-", spp_for_filename,".wav")
      
      #create chunk centred on this label
      AcousticTools::extract_chunk(file_wav = wav1, 
                                   file_chunk = file.path(path_out, chunk_name),
                                   start = labels1$start, 
                                   end = labels1$end, 
                                   chunk_duration)
      
      #create associated label file
      lab_content <- data.frame(start = rep(chunk_duration/2, length(labs)), 
                                end = rep(chunk_duration/2, length(labs)),
                                txt = labs)
      lab_name <- gsub(".wav", ".txt", chunk_name)
      write.table(lab_content, file = file.path(path_out, lab_name), quote = FALSE, sep = "\t", col.names = FALSE, row.names = FALSE)
      
      rm(list=c("labels1", "labs", "spp_for_filename", "detection_time", "chunk_name", "lab_content", "lab_name"))
    }
    rm(list = c("labels", "wav1", "file1", "file_start"))
  }

}
