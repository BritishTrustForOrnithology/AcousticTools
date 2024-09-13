#' Download Species Recordings from xeno-canto
#' 
#' @description
#' Uses the warbleR package and the xeno-canto API to download clips for a species.
#' 
#' @details
#' Pass in a species name and a number of clips desired. Some filtering is currently 
#' hard coded, e.g. only returns clips with quality A:B, removes nestlings and 
#' birds not seen, and any clips with other species reported in the background. 
#' It is possible these filters will result in zero returns.
#' 
#' @import warbleR
#' @import lubridate
#' @import stringr
#' @import BTOTools
#' 
#' @param path_out, string, the main path where files are to be saved.
#' @param code2ltr, string, the two letter code of the species required
#' @param n, numeric, the number of clips to return
#' 
#' @export
#' 
download_XC_species_clips <- function(path_out, code2ltr, n) {

  #get the species names and codes for this species
  spinfo <- BTOTools::get_species_info('2ltr', code2ltr)
    
  #create the search string and get the XC listings
  searchstring <- paste0(spinfo$english_name)
  xcdf <- query_xc(searchstring, 
                   download = FALSE, 
                   file.name = c("Genus", "Specific_epithet", "Date", "Country", "Recordist"), 
                   parallel = 1, 
                   pb = TRUE)

  #filter the XC listings...
  #remove clips with other species
  xcdf <- subset(xcdf, is.na(Other_species))
  xcdf <- subset(xcdf, is.na(Other_species1))
  
  #remove not seen
  xcdf <- subset(xcdf, Bird_seen == 'yes')
  
  #remove nestlings
  xcdf <- subset(xcdf, stage != 'nestling')
  
  #remove poor quality
  xcdf <- subset(xcdf, Quality %in% c("A", "B"))
  
  #calculate clip duration from chr field
  bits <- stringr::str_split_fixed(xcdf$Length, ":", 2)
  xcdf$mins <- as.numeric(bits[,1])
  xcdf$secs <- as.numeric(bits[,2])
  xcdf$duration <- (xcdf$mins * 60) + xcdf$secs
  
  #remove clips <10s
  xcdf <- subset(xcdf, duration > 10)
  
  #check there are still clips...
  if(nrow(xcdf)==0) return('fail')
  
  #order remaining clips
  xcdf <- xcdf[order(-xcdf$duration),]
  
  #construct the output path
  download_loc <- file.path(path_out, paste0(spinfo$code2ltr, " - ", spinfo$english_name ))
  dir.create(download_loc, recursive = TRUE, showWarnings = FALSE)
  
  #limit size of download
  xcdf <- xcdf[1:min(n, nrow(xcdf)),]
  
  #run download query
  xc <- query_xc(X = xcdf, 
                 file.name = c("Genus", "Specific_epithet", "Date", "Country"), 
                 parallel = 1, 
                 path = download_loc, 
                 pb = TRUE)
  
  #format file names
  audios <- list.files(path = download_loc, full.names = TRUE, pattern = "*.mp3")
  
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
  cat('Convert MP3s to WAVs...\n\n')
  mp3s <- list.files(path = download_loc, full.names = TRUE, pattern = "*.mp3")
  for(m in 1:length(mp3s)) {
    #cat(audio[m],'\n')
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
  }
  
  
  #format file names
  cat('Format file names...\n\n')
  #split the names  
  audios <- list.files(path = download_loc, full.names = FALSE)
  bits <- as.data.frame(str_split_fixed(audios, pattern = '[-.]', n = Inf), stringsAsFactors = FALSE)
  dim(bits)
  
  if(dim(bits)[2] != 8) stop("filenames do not match expected format")
  
  names(bits) <- c('genus','species','location','year','month','day','xc', 'ext')
  bits$location <- gsub(" ","",bits$location)
  bits$location <- gsub(",","",bits$location)
  bits$location <- gsub("-","",bits$location)
  #bits$xc <- substr(bits$xc,1,6)
  
  #create newname
  bits$newname <- with(bits, paste0(year,month,day,'-0000-', location, "-XenoCantoContributor-XX-", code2ltr, "-(XC", xc, ").wav"))   
  
  #rename files
  file.rename(from = file.path(download_loc, audios), 
              to = file.path(download_loc,bits$newname))
  
  # #split into month folders for easier way of getting seasonal coverage of clips
  # cat('Splitting into month folders...\n\n')
  # months <- unique(bits$month)
  # for(i in 1:length(months)) {
  #   newloc <- file.path(download_loc, months[i])
  #   dir.create(newloc, recursive = TRUE)
  #   tomove <- subset(bits, month == months[i])
  #   file.rename(from = file.path(download_loc, tomove$newname),
  #               to = file.path(newloc, tomove$newname))
  # }
  
  #make empty default label file
  cat('Make empty labels...\n\n')
  audio <- list.files(download_loc, pattern = "*wav", full.names = TRUE, recursive = TRUE)
  for(a in 1:length(audio)) {
    file_txt <- gsub('.wav', '.txt', audio[a])
    writeLines(text = '', con = file_txt)
  }


}
