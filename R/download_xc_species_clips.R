#' Download Species Recordings from xeno-canto
#' 
#' @description
#' Uses the xeno-canto API (v3) to download filtered samples 
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
#' @import lubridate
#' @import stringr
#' @import httr
#' @import jsonlite
#' @import warbleR
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
#' (favouring short clips), long (favouring long clips).
#' @param make_labels, bool, whether to make empty Audacity label files.
#' @param used_clips, character vector, XC ID numbers of clips that have already 
#' been labelled. These should be supplied as a character vector consisting of 
#' just the numbers (i.e. omitting the XC prefix)
#' @param xc_key, character, your API access key
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
                                      make_labels = TRUE,
                                      used_clips = NULL, 
                                      xc_key = NULL) {
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
  if(!is.null(used_clips)) {
    if(!is.character(used_clips)) stop("used clips must be a character vector")
    if(any(grepl('XC', used_clips))) stop("used clips should not have XC prefixes")
  }
  
  if(is.null(xc_key)) stop('You must supply a xeno-canto API access key. See XC website')
  
  #check and create output location
  if(!dir.exists(path_out)) {
    warning("path_out does not exist; trying to make it now...")
    trying <- dir.create(path_out, recursive= TRUE)
    if(trying==TRUE) warning('...path_out successfully created')
    if(trying==FALSE) stop('...unable to create path_out.')
  } 
  
  #create the search string and get the XC listings
  
  scientific_name2 <- gsub(" ", "%20", scientific_name)
  
  #do initial page 1 response
  response <- GET(paste0(
    'https://xeno-canto.org/api/3/recordings?query=sp:"',
    tolower(gsub(" ", "%20", scientific_name)),
    '"&key=',
    xc_key))
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON content
    json_data <- content(response, "text", encoding = "UTF-8")
    parsed_data <- fromJSON(json_data, flatten = TRUE)
    
    # Convert to a dataframe
    xcdf <- as.data.frame(parsed_data)
  } else {
    cat("API request failed with status:", status_code(response), "\n")
  }
  
  #check how many pages in total
  npages <- xcdf$numPages[1]
  
  #iterate over remaining pages and append results
  if(npages > 1) {
    for(p in 2:npages) {
      response <- GET(paste0(
        'https://xeno-canto.org/api/3/recordings?query=sp:"',
        gsub(" ", "%20", scientific_name),
        '"&key=',
        xc_key, 
        '&page=', p))

      # Check if the request was successful
      if (status_code(response) == 200) {
        # Parse the JSON content
        json_data <- content(response, "text", encoding = "UTF-8")
        parsed_data <- fromJSON(json_data, flatten = TRUE)
        
        # Convert to a dataframe
        dfp <- as.data.frame(parsed_data)
        xcdf <- rbind(xcdf, dfp)
        rm(dfp)
      }
      
    }
  }
  
  
  
  

  if(!is.null(used_clips)) {
    #dropping used clips
    before <- nrow(xcdf)
    xcdf <- subset(xcdf, !recordings.id %in% used_clips)
    after <- nrow(xcdf)
    cat("# records after dropping recordings already labelled = ", nrow(xcdf),"\n")
  }
  
  #filter the XC listings...
  #remove clips with other species
  if(no_background==TRUE) {
    xcdf$recordings.also <- ifelse(xcdf$recordings.also == 'character(0)', NA, xcdf$recordings.also)
    xcdf <- subset(xcdf, is.na(recordings.also))
    cat("# records after other species filtering = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  
  #remove not seen
  if(seen_only==TRUE) {
    xcdf <- subset(xcdf, recordings.animal.seen == 'yes')
    cat("# records after not-seen filtering = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  #remove nestlings
  if(no_nestlings==TRUE) {
    xcdf <- subset(xcdf, recordings.stage != 'nestling')
    cat("# records after nestling filtering = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  #remove poor quality
  if(!is.null(min_quality)) {
    acceptable_quality <- LETTERS[1:which(LETTERS[1:5]==min_quality)]
    xcdf <- subset(xcdf, recordings.q %in% acceptable_quality)
    cat("# records after quality filtering = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  #remove no derivatives
  if(no_noderivs==TRUE) {
    xcdf <- subset(xcdf, !grepl("-nd", recordings.lic))
    cat("# records after removal of no-derivatives license = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  #remove non commercial
  if(no_noncomms==TRUE) {
    xcdf <- subset(xcdf, !grepl("-nc-", recordings.lic))
    cat("# records after removal of no-derivatives license = ", nrow(xcdf),"\n")
    if(nrow(xcdf)==0) stop("No recordings left")
  }
  
  
  #calculate clip duration from chr field
  bits <- stringr::str_split_fixed(xcdf$recordings.length, ":", 2)
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
  
  
  #do some column formatting
  #Dates
  xcdf$datestr <- format(as.Date(xcdf$recordings.date), "%Y%m%d")
  xcdf$datestr <- ifelse(is.na(xcdf$datestr), '19700101', xcdf$datestr)
  xcdf$month <- lubridate::month(as.Date(xcdf$recordings.date))
  xcdf$month <- ifelse(is.na(xcdf$month), 0, xcdf$month)
  
  #Times
  # Step 1: Standardize "HH:MM" or "H:MM" formats
  xcdf$timestr <- xcdf$recordings.time
  xcdf$timestr <- gsub("^(\\d):", "0\\1:", xcdf$timestr) # Add leading zero to single-digit hours colon variant
  xcdf$timestr <- gsub("^(\\d)\\.", "0\\1:", xcdf$timestr) # Add leading zero to single-digit hours dot variant
  xcdf$timestr <- gsub("^(\\d)-", "0\\1:", xcdf$timestr) # Add leading zero to single-digit hours hyphen variant
  xcdf$timestr <- gsub("^(\\d{2}):(\\d{2}).*", "\\1\\2", xcdf$timestr) # Keep HHMM from HH:MM
  xcdf$timestr <- gsub("^(\\d{2})\\.(\\d{2}).*", "\\1\\2", xcdf$timestr) # Keep HHMM from HH.MM
  xcdf$timestr <- gsub("^(\\d{2})-(\\d{2}).*", "\\1\\2", xcdf$timestr) # Keep HHMM from HH-MM
  # Step 2: Handle "HHMM" without colon
  xcdf$timestr <- gsub("^(\\d{4})$", "\\1", xcdf$timestr)
  # Step 3: convert invalid to 0000
  valid_format <- grepl("^(\\d{4})$", xcdf$timestr) # Keep only HHMM
  xcdf$timestr[!valid_format] <- "0000"
  
  
  #if split into month folders, make them
  if(month_folders == TRUE) {
    xcmonths <- unique(xcdf$month)
    for(i in 1:length(xcmonths)) {
      newloc <- file.path(path_out, xcmonths[i])
      dir.create(newloc, recursive = TRUE)
    }
  } 
  
  
  #iterate over list and download, formatting, making label files etc as needed
  for(i in 1:nrow(xcdf)) {
    this <- xcdf[i,]
    url <- this$recordings.file
    id <- this$recordings.id
    
    #what type of file?
    ext <- tolower(substr(this$recordings.file.name, 
                  nchar(this$recordings.file.name)-3, 
                  nchar(this$recordings.file.name)))
    
    destfilename <- with(xcdf, paste0(this$datestr,"-",
                            this$timestr,"-",
                            gsub(" ","",this$recordings.cnt),
                            "-XenoCantoContributor-XX-", 
                            species_code, 
                            "-(XC", 
                            this$recordings.id, 
                            ")",
                            ext))
    if(month_folders==TRUE) {
      destfilepath <- file.path(path_out, this$month, destfilename)
      
    }
    #and download
    download.file(url, destfilepath, mode = "wb")          # 'wb' for binary files
    
    #convert mp3?
    if(ext == ".mp3") {
      #read the mp3
      audiotemp <- warbleR::read_sound_file(destfilepath)
      
      #make the wav filename
      file_wav <- gsub('.mp3', '.wav', destfilepath)
      
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
      if(file_corrupt == 0) file.remove(destfilepath)
      gc(verbose = FALSE)
    }
    
    
  
  
    #make empty default label file
    if(make_labels==TRUE) {
      cat('Make empty labels...\n\n')
      file_txt <- gsub('.mp3|.wav', '.txt', destfilepath)
      writeLines(text = '', con = file_txt)
    }

  } #end download loop

}
