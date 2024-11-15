Demo stratified sampling clips based on BirdNET output
================

Example code for reading BirdNET output and exporting a stratified
sample of detections in species subfolders. Here stratification is done
by site, week and species. The method could be extended to include
sampling by confidence score, month, time of day or any other categories
as required.

``` r
#devtools::install_github('BritishTrustForOrnithology/AcousticTools')
library(AcousticTools)

#collate some BirdNET outputs
df <- AcousticTools::read_birdnet_results(folder = "H:/Leiothrix Recordings")
```

    ## Folder contains individual Audacity files

``` r
#this location has too many for a simple example so I'm just going to limit it to 3 locations and 3 species for simplicity
df <- subset(df, grepl('ST403|ST7747|ST7847', df$original_wav))
df <- subset(df, birdnet_english_name %in% c('Barn Owl', 'Black-crowned Night-Heron', 'Brown Creeper'))
head(df)
```

    ##                                                          original_wav start
    ## 31   H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav   132
    ## 2115 H:/Leiothrix Recordings/ST4037/20240716-211902-ST4037-BTO-XM.wav  1377
    ## 2126 H:/Leiothrix Recordings/ST4037/20240716-211902-ST4037-BTO-XM.wav  1422
    ## 2133 H:/Leiothrix Recordings/ST4037/20240716-211902-ST4037-BTO-XM.wav  1434
    ## 2350 H:/Leiothrix Recordings/ST4037/20240716-211902-ST4037-BTO-XM.wav  2088
    ## 2875 H:/Leiothrix Recordings/ST4037/20240717-041600-ST4037-BTO-XM.wav   585
    ##       end birdnet_scientific_name      birdnet_english_name  score lat lon week
    ## 31    135       Certhia americana             Brown Creeper 0.1393  NA  NA   NA
    ## 2115 1380               Tyto alba                  Barn Owl 0.2209  NA  NA   NA
    ## 2126 1425               Tyto alba                  Barn Owl 0.7851  NA  NA   NA
    ## 2133 1437   Nycticorax nycticorax Black-crowned Night-Heron 0.1190  NA  NA   NA
    ## 2350 2091               Tyto alba                  Barn Owl 0.1176  NA  NA   NA
    ## 2875  588   Nycticorax nycticorax Black-crowned Night-Heron 0.8613  NA  NA   NA
    ##      overlap sensitivity min_conf species_list model
    ## 31        NA          NA       NA           NA    NA
    ## 2115      NA          NA       NA           NA    NA
    ## 2126      NA          NA       NA           NA    NA
    ## 2133      NA          NA       NA           NA    NA
    ## 2350      NA          NA       NA           NA    NA
    ## 2875      NA          NA       NA           NA    NA

Currently we have all detections. In this example I’m going to do some
stratified sampling by site and week. First, split the file names into
component parts. This assumes files are in recognised
YYYYMMDD-HHMMSS-Location-Recordist-Microphone.wav format. First split
the filename using stringr::str_split_fixed.

``` r
library(stringr)
```

    ## Warning: package 'stringr' was built under R version 4.2.3

``` r
#split the original filename parts
bits <- setNames(as.data.frame(stringr::str_split_fixed(string = basename(df$original_wav), pattern = "-|\\.", n = Inf)), 
                 c('date_str', 'time_str', 'loc', 'rec', 'mic','ext'))
head(bits)
```

    ##   date_str time_str    loc rec mic ext
    ## 1 20240716   140000 ST4037 BTO  XM wav
    ## 2 20240716   211902 ST4037 BTO  XM wav
    ## 3 20240716   211902 ST4037 BTO  XM wav
    ## 4 20240716   211902 ST4037 BTO  XM wav
    ## 5 20240716   211902 ST4037 BTO  XM wav
    ## 6 20240717   041600 ST4037 BTO  XM wav

``` r
#join to the main df
df <- cbind(df, bits)
```

As before, I recommend extracting the date and time as a datetime
variable. This allows for making the detection times, but also means we
can use the date objects to do temporal sampling, e.g. here making a
week number variable for later stratification

Then the detection offset (start) can be added to get the start datetime
of the detection. Now convert this to a date using lubridate and add the
start time offset for the detection.

``` r
library(lubridate)
```

    ## Warning: package 'lubridate' was built under R version 4.2.3

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
#extract the datetime of the original recording
df$recording_start_dt <- as.POSIXct(paste(df$date_str, df$time_str), format = "%Y%m%d %H%M%S")

#add seconds offset to get detecion datetime
df$detection_dt <- df$recording_start_dt + df$start

#make a week number variable
df$weeknum <- lubridate::week(df$detection_dt)

head(df[,c("original_wav", "start", "recording_start_dt", "detection_dt", "weeknum")])
```

    ##                                                          original_wav start
    ## 31   H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav   132
    ## 2115 H:/Leiothrix Recordings/ST4037/20240716-211902-ST4037-BTO-XM.wav  1377
    ## 2126 H:/Leiothrix Recordings/ST4037/20240716-211902-ST4037-BTO-XM.wav  1422
    ## 2133 H:/Leiothrix Recordings/ST4037/20240716-211902-ST4037-BTO-XM.wav  1434
    ## 2350 H:/Leiothrix Recordings/ST4037/20240716-211902-ST4037-BTO-XM.wav  2088
    ## 2875 H:/Leiothrix Recordings/ST4037/20240717-041600-ST4037-BTO-XM.wav   585
    ##       recording_start_dt        detection_dt weeknum
    ## 31   2024-07-16 14:00:00 2024-07-16 14:02:12      29
    ## 2115 2024-07-16 21:19:02 2024-07-16 21:41:59      29
    ## 2126 2024-07-16 21:19:02 2024-07-16 21:42:44      29
    ## 2133 2024-07-16 21:19:02 2024-07-16 21:42:56      29
    ## 2350 2024-07-16 21:19:02 2024-07-16 21:53:50      29
    ## 2875 2024-07-17 04:16:00 2024-07-17 04:25:45      29

Now do the stratified sampling. This can be done in nested loops but
this gets messy the more strata there are. So I’ve tried to make this
more easily scalable. In this example there are two methods for
sampling: ‘random’ sampling or ‘tophits’, where the latter just gives
the N highest scoring detections. Ultimately, I can pass the output
locations to the extract_chunk function which will automatically make
any required folders.

``` r
#decide how many chunks per stratum
nchunks <- 5

#strata variables
strata_vars <- c('loc', 'weeknum', 'birdnet_english_name')

#get the unique permutations of these - essentially the folders we will need
strata_col_indx <- which(names(df) %in% strata_vars)

strata <- unique(df[,strata_col_indx])

#how to select chunks
method <- 'tophits'
#method <- 'random'


#iterate over strata

#make a list to hold outputs
strata_out <- list()

for(i in 1:nrow(strata)) {
  #get detections for this stratum
  this_stratum <- strata[i,]
  
  #this line will need modifying to match whatever strata vars the filtering is being done on
  temp <- subset(df, loc == this_stratum$loc & weeknum == this_stratum$weeknum & birdnet_english_name == this_stratum$birdnet_english_name)
  
  #if number of detections <= nchunks, keep all
  if(nrow(temp) <= nchunks) {
    stratum_out <- temp
  }
  
  #if number of detections > nchunks, use 'method' to select nchunks
  if(nrow(temp) > nchunks) {
    
    #random sample
    if(method == 'random') {
      stratum_out <- temp[sample(nrow(temp), nchunks), ]
    } 
    #top scoring
    if(method == 'tophits') {
      temp <- temp[order(-temp$score),]
      stratum_out <- temp[1:nchunks,]
    } 
  }
  #store output for this stratum
  strata_out[[i]] <- stratum_out
  #tidy up
  rm(list=c('stratum_out', 'this_stratum', 'temp'))
  
} #end stratum loop
  
#unpack all stratum outputs to a flat df
df_sampled <- do.call(rbind, strata_out)
```

Now we can use the detection datetime to make a unique filename. And we
can use the locs and species names to make folders

``` r
#make the new filename. Use __ to infer unknown species at this point
df_sampled$newfilename <- paste0(format(df_sampled$detection_dt, "%Y%m%d-%H%M%S"),
                         '-',
                         df_sampled$loc,
                         '-',
                         df_sampled$rec,
                         '-',
                         df_sampled$mic,
                         '-__.',
                         df_sampled$ext)
                         

#where will chunks be saved?
path_export <- 'C:/exports'

#make the full path and filename of each clip to be exported
#this line will need to modified for different strata, to make a folder level for each stratum variable
#here I've done site > species > week, but could be in different orders
df_sampled$chunk_fullname <- file.path(path_export, df_sampled$loc, df_sampled$birdnet_english_name, df_sampled$weeknum, df_sampled$newfilename)
head(df_sampled$chunk_fullname)
```

    ## [1] "C:/exports/ST4037/Brown Creeper/29/20240717-045236-ST4037-BTO-XM-__.wav"
    ## [2] "C:/exports/ST4037/Brown Creeper/29/20240720-060220-ST4037-BTO-XM-__.wav"
    ## [3] "C:/exports/ST4037/Brown Creeper/29/20240721-063514-ST4037-BTO-XM-__.wav"
    ## [4] "C:/exports/ST4037/Brown Creeper/29/20240717-130118-ST4037-BTO-XM-__.wav"
    ## [5] "C:/exports/ST4037/Brown Creeper/29/20240717-081350-ST4037-BTO-XM-__.wav"
    ## [6] "C:/exports/ST4037/Barn Owl/29/20240716-214244-ST4037-BTO-XM-__.wav"

Now we can do the exporting using AcousticTools::extract_chunk. As we
pass in the file_chunk which contains the site/species/weeknum this
ensures all the chunks are made in their respective folders.
extract_chunks automatically makes any folders as needed.

``` r
#iterate over the required sampled chunks
for(i in 1:nrow(df_sampled)) {
  AcousticTools::extract_chunk(file_wav = df_sampled$original_wav[i],
                               file_chunk = df_sampled$chunk_fullname[i],
                               start = df_sampled$start[i], 
                               end = df_sampled$end[i], 
                               chunk_duration = 5,
                               verbose = TRUE)
  
}
```
