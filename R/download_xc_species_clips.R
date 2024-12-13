#' Download Species Recordings from xeno-canto
#' 
#' @description
#' Uses the warbleR package and the xeno-canto API to download filtered samples 
#' of clips for a species, renamed and organised into folders.
#' 
#' @details
#' Pass in a species' scientific name and a number of clips desired. If there are 
#' more clips than you want, three variants of sampling are offered: random, or 
#' preferentially by file duration. Various filtering options are offered including 
#' by clip duration, quality, license, whether seen, whether there are background 
#' species, and whether nestlings. 
#' 
#' All filenames are renamed to a standard format:
#' YYYYMMDD-HHMM-Country-XenoCantoContributor-XX-SpeciesCode-(XCref).wav
#' 
#' Optionally audio clips will be placed in folders by month, which can be helpful 
#' when labeling to get a seasonal spread of vocalisations. 
#' 
#' Optionally you can opt to export empty Audacity label files. These can make 
#' audio labeling quicker as you can overwrite a pre-named file than have to 
#' create a label file to match the audio file.
#' 
#' @import warbleR
#' @import lubridate
#' @import stringr
#' 
#' @param scientific_name, string, the scientific name of the required species
#' @param species_code, string, code to use when making new filenames
#' @param path_out, string, a folder where files are to be saved.
#' @param month_folders, bool, whether to put files in month folders.
#' @param n, numeric, the number of clips to return
#' @param min_length, numeric, minimum clip length in seconds, default 10.
#' @param max_length, numeric, maximum clip length in seconds.
#' @param min_quality, character, minimum acceptable quality code (A:E).
#' @param no_background, bool, exclude clips with named background species.
#' @param seen_only, bool, only retain clips where focal species was seen.
#' @param no_nestlings, bool, exclude clips of nestlings.
#' @param drop_noderivs, bool, exclude clips with a No Derivatives license.
#' @param drop_noncomms, bool, exclude clips with a Non-commercial license.
#' @param sample, character, strategy for sampling clips: random, short 
#' @param make_labels, bool, whether to make empty Audacity label files.
#' (favouring short clips), long (favouring long clips).
#' 
#' 
#' @examples
#' \dontrun{
#' download_XC_species_clips(scientific_name = 'Galerida theklae', 
#' species_code = 'THELA', path_out = 'E:/Acoustics/THELA', 
#' month_folders = TRUE, n = 50, min_length = 10, min_quality = 'C', 
#' no_background = TRUE, no_nestlings = TRUE, drop_noncomms = TRUE,
#' sample = 'random', make_labels = TRUE)
#' #' 
#' }
#' 
#' @export
#' 
download_XC_species_clips <- function(scientific_name = NULL, 
                                      species_code = NULL,
                                      path_out, 
                                      month_folders = TRUE,
                                      n, 
                                      min_length = 10, 
                                      max_length = NULL, 
                                      min_quality = NULL,
                                      no_background = FALSE,
                                      seen_only = FALSE,
                                      no_nestlings = TRUE,
                                      no_noderivs = FALSE,
                                      no_noncomms = FALSE,
                                      sample = 'random',
                                      make_labels = TRUE) {
  #check inputs
  if(is.null(scientific_name)) stop('scientific_name must be supplied')
  if(is.null(species_code)) stop('species_code must be supplied')
  
  if(!is.null(min_quality)) {
    if(!is.character(min_quality)) stop("min_quality must be a character")
    if(!min_quality %in% c("A", "B","C","D","E")) stop("min_quality must be in range A to E")
  }
  if(!is.null(sample)) {
    if(!sample %in% c('random',  'short', 'long')) stop('sample must be one of: random, short, long')
  }
  
  
  #check and create output location
  if(!dir.exists(path_out)) {
    warning("path_out does not exist; trying to make it now...")
    trying <- dir.create(path_out)
    if(trying==TRUE) warning('...path_out successfully created')
    if(trying==FALSE) stop('...unable to create path_out.')
  } 
  
  #create the search string and get the XC listings
  searchstring <- paste(scientific_name)
  xcdf <- query_xc(searchstring, 
                   download = FALSE, 
                   file.name = c("Genus", "Specific_epithet", "Date", "Time", "Country", "Recordist"), 
                   parallel = 1, 
                   pb = TRUE)

  #double check correct species
  #sometimes search string fails for spp like Riparia riparia
  xcdf$scientific_name <- paste(xcdf$Genus, xcdf$Specific_epithet)
  xcdf <- subset(xcdf, scientific_name == scientific_name)
  
  #filter the XC listings...
  #remove clips with other species
  if(no_background==TRUE) {
    xcdf <- subset(xcdf, is.na(Other_species))
    xcdf <- subset(xcdf, is.na(Other_species1))
    cat("# records after other species filtering = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  
  #remove not seen
  if(seen_only==TRUE) {
    xcdf <- subset(xcdf, Bird_seen == 'yes')
    cat("# records after not-seen filtering = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  #remove nestlings
  if(no_nestlings==TRUE) {
    xcdf <- subset(xcdf, stage != 'nestling')
    cat("# records after nestling filtering = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  #remove poor quality
  if(!is.null(min_quality)) {
    acceptable_quality <- LETTERS[1:which(LETTERS[1:5]==min_quality)]
    xcdf <- subset(xcdf, Quality %in% acceptable_quality)
    cat("# records after quality filtering = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  #remove no derivatives
  if(no_noderivs==TRUE) {
    xcdf <- subset(xcdf, !grepl("-nd", xcdf$License))
    cat("# records after removal of no-derivatives license = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  #remove non commercial
  if(no_noncomms==TRUE) {
    xcdf <- subset(xcdf, !grepl("-nc-", xcdf$License))
    cat("# records after removal of no-derivatives license = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  
  #calculate clip duration from chr field
  bits <- stringr::str_split_fixed(xcdf$Length, ":", 2)
  xcdf$mins <- as.numeric(bits[,1])
  xcdf$secs <- as.numeric(bits[,2])
  xcdf$duration <- (xcdf$mins * 60) + xcdf$secs
  
  #apply min_duration and max_duration
  if(!is.null(min_length)) xcdf <- subset(xcdf, duration >= min_length)
  cat("# records after min_length filtering = ", nrow(xcdf),"\n")
  if(nrow(xcdf)==0) stop("No recordings left")
  if(!is.null(max_length)) xcdf <- subset(xcdf, duration <= max_length)
  cat("# records after max_length filtering = ", nrow(xcdf),"\n")
  if(nrow(xcdf)==0) stop("No recordings left")
  
  #apply sample method to order remaining clips
  if(n < nrow(xcdf)) {
    message('More rows remaining than requested. Sampling will be applied')
    if(sample == 'random') {
      xcdf$random <- runif(n = nrow(xcdf))
      xcdf <- xcdf[order(xcdf$random),]
      xcdf <- xcdf[1:n,]
    }
    if(sample == 'short') {
      xcdf <- xcdf[order(xcdf$duration),]
      xcdf <- xcdf[1:n,]
    }
    if(sample == 'long') {
      xcdf <- xcdf[order(-xcdf$duration),]
      xcdf <- xcdf[1:n,]
      
    }
  }
  if(n>=nrow(xcdf)) {
    message('Fewer clips remaining than requested so exporting all')
  }
  
  #now run download query with this set
  xc <- query_xc(X = xcdf, 
                 #file.name = c("Genus", "Specific_epithet", "Date", "Country"), 
                 file.name = "Recording_ID", 
                 parallel = 1, 
                 path = path_out, 
                 pb = TRUE)
  
  #format file names
  audios <- list.files(path = path_out, full.names = TRUE, pattern = "*.mp3")
  
  #check if any should be wavs
  cat('Correct extensions for existing WAVs...\n\n')
  xcdf$iswav <- ifelse(grepl(pattern = '.wav', x = xcdf$file.name), TRUE, FALSE)

  xcdfwavs <- subset(xcdf, iswav == TRUE)
  if(nrow(xcdfwavs)>0) {
    for(w in 1:nrow(xcdfwavs)) {
      id <- xcdfwavs$Recording_ID[w]
      #find this audio file
      thisaudio <- audios[grepl(id, audios)]
      #rename it
      thisaudio2 <- gsub('.mp3','.wav', thisaudio)
      file.rename(thisaudio, thisaudio2)
      rm(thisaudio)
      rm(thisaudio2)
    }
  }
  
  #convert the remaining mp3s into wavs
  cat('Convert remaining MP3s to WAVs...\n\n')
  mp3s <- list.files(path = path_out, full.names = TRUE, pattern = "*.mp3")
  for(m in 1:length(mp3s)) {
    cat(basename(mp3s[m]),'\n')
    #read the mp3
    audiotemp <- read_sound_file(mp3s[m])
    
    #make the wav filename
    file_wav <- gsub('.mp3', '.wav', mp3s[m])
    
    #write the wav
    file_corrupt <- tryCatch({
      # Attempt to read the file
      writeWave(audiotemp, file_wav)
      0
    }, error = function(e) {
      # Handle the error
      1
    })
    
    #remove the mp3
    if(file_corrupt == 0) file.remove(mp3s[m])
    gc(verbose = FALSE)
  }
  
  
  #format file names
  cat('Format file names...\n\n')
  #split the names  
  audios <- list.files(path = path_out, full.names = FALSE)
  
  #format columns for making filename - needs some cleaning!
  #Dates
  xcdf$Datestr <- format(as.Date(xcdf$Date), "%Y%m%d")
  xcdf$Datestr <- ifelse(is.na(xcdf$Datestr), '19700101', xcdf$Datestr)

  #Times
  # Step 1: Standardize "HH:MM" or "H:MM" formats
  xcdf$Timestr <- xcdf$Time
  xcdf$Timestr <- gsub("^(\\d):", "0\\1:", xcdf$Timestr) # Add leading zero to single-digit hours colon variant
  xcdf$Timestr <- gsub("^(\\d)\\.", "0\\1:", xcdf$Timestr) # Add leading zero to single-digit hours dot variant
  xcdf$Timestr <- gsub("^(\\d)-", "0\\1:", xcdf$Timestr) # Add leading zero to single-digit hours hyphen variant
  xcdf$Timestr <- gsub("^(\\d{2}):(\\d{2}).*", "\\1\\2", xcdf$Timestr) # Keep HHMM from HH:MM
  xcdf$Timestr <- gsub("^(\\d{2})\\.(\\d{2}).*", "\\1\\2", xcdf$Timestr) # Keep HHMM from HH.MM
  xcdf$Timestr <- gsub("^(\\d{2})-(\\d{2}).*", "\\1\\2", xcdf$Timestr) # Keep HHMM from HH-MM
  # Step 2: Handle "HHMM" without colon
  xcdf$Timestr <- gsub("^(\\d{4})$", "\\1", xcdf$Timestr)
  # Step 3: convert invalid to 0000
  valid_format <- grepl("^(\\d{4})$", xcdf$Timestr) # Keep only HHMM
  xcdf$Timestr[!valid_format] <- "0000"

  #create newname
  xcdf$oldname <- with(xcdf, paste0("-",Recording_ID, ".wav"))
  xcdf$newname <- with(xcdf, paste0(Datestr,"-",Timestr,"-",Country,"-XenoCantoContributor-XX-", species_code, "-(XC", Recording_ID, ").wav"))

  #rename files
  file.rename(from = file.path(path_out, xcdf$oldname), 
              to = file.path(path_out,xcdf$newname)
              )
  
  #split into month folders for easier way of getting seasonal coverage of clips
  if(month_folders == TRUE) {
    cat('Splitting into month folders...\n\n')
    audios <- list.files(path = path_out, full.names = FALSE)
    months <- unique(substr(audios,5,6))
    for(i in 1:length(months)) {
      newloc <- file.path(path_out, months[i])
      dir.create(newloc, recursive = TRUE)
      tomove <- subset(audios, substr(audios,5,6) == months[i])
      file.rename(from = file.path(path_out, tomove),
                  to = file.path(newloc, tomove))
    }
  } 
  
  #make empty default label file
  if(make_labels==TRUE) {
    cat('Make empty labels...\n\n')
    audio <- list.files(path_out, pattern = "*wav", full.names = TRUE, recursive = TRUE)
    for(a in 1:length(audio)) {
      file_txt <- gsub('.wav', '.txt', audio[a])
      writeLines(text = '', con = file_txt)
    }
  }


}
