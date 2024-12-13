Demo extracting clips based on BirdNET output
================

Example code for reading BirdNET output and exporting detections in
species subfolders

``` r
#devtools::install_github('BritishTrustForOrnithology/AcousticTools')
library(AcousticTools)

#read a BirdNET output
df <- AcousticTools::read_birdnet_results(folder = "H:/Leiothrix Recordings/ST4037")
```

    ## Folder contains individual Audacity files

``` r
head(df)
```

    ##                                                       original_wav start end
    ## 1 H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav    15  18
    ## 2 H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav    21  24
    ## 3 H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav    24  27
    ## 4 H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav    30  33
    ## 5 H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav    33  36
    ## 6 H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav    33  36
    ##         birdnet_scientific_name birdnet_english_name  score lat lon week
    ## 1                           Gun                  Gun 0.1106  NA  NA   NA
    ## 2             Muscicapa striata   Spotted Flycatcher 0.1904  NA  NA   NA
    ## 3             Muscicapa striata   Spotted Flycatcher 0.6818  NA  NA   NA
    ## 4             Muscicapa striata   Spotted Flycatcher 0.7349  NA  NA   NA
    ## 5 Coccothraustes coccothraustes             Hawfinch 0.1544  NA  NA   NA
    ## 6             Muscicapa striata   Spotted Flycatcher 0.1177  NA  NA   NA
    ##   overlap sensitivity min_conf species_list model
    ## 1      NA          NA       NA           NA    NA
    ## 2      NA          NA       NA           NA    NA
    ## 3      NA          NA       NA           NA    NA
    ## 4      NA          NA       NA           NA    NA
    ## 5      NA          NA       NA           NA    NA
    ## 6      NA          NA       NA           NA    NA

Currently we have all detections. We might want to do some filtering or
sampling. For this simple example I’m just going to limit to high
scoring detections. More complex approaches, like site- or species-based
stratified sampling could be adopted.

``` r
#keep all high scoring detections 
df <- subset(df, score >=0.9)
```

Assuming files are in recognised YYYYMMDD-HHMMSS format I recommend
extracting the date and time as a datetime variable. Then the detection
offset (start) can be added to get the start datetime of the detection.
First split the filename using stringr::str_split_fixed.

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
    ## 2 20240716   150000 ST4037 BTO  XM wav
    ## 3 20240716   160000 ST4037 BTO  XM wav
    ## 4 20240716   160000 ST4037 BTO  XM wav
    ## 5 20240716   170000 ST4037 BTO  XM wav
    ## 6 20240716   201900 ST4037 BTO  XM wav

``` r
#join to the main df
df <- cbind(df, bits)
```

Now convert this to a date using lubridate and add the start time offset
for the detection.

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

head(df[,c("original_wav", "start", "recording_start_dt", "detection_dt")])
```

    ##                                                          original_wav start
    ## 78   H:/Leiothrix Recordings/ST4037/20240716-140000-ST4037-BTO-XM.wav   486
    ## 296  H:/Leiothrix Recordings/ST4037/20240716-150000-ST4037-BTO-XM.wav   240
    ## 500  H:/Leiothrix Recordings/ST4037/20240716-160000-ST4037-BTO-XM.wav   267
    ## 540  H:/Leiothrix Recordings/ST4037/20240716-160000-ST4037-BTO-XM.wav   360
    ## 787  H:/Leiothrix Recordings/ST4037/20240716-170000-ST4037-BTO-XM.wav   447
    ## 1075 H:/Leiothrix Recordings/ST4037/20240716-201900-ST4037-BTO-XM.wav   132
    ##       recording_start_dt        detection_dt
    ## 78   2024-07-16 14:00:00 2024-07-16 14:08:06
    ## 296  2024-07-16 15:00:00 2024-07-16 15:04:00
    ## 500  2024-07-16 16:00:00 2024-07-16 16:04:27
    ## 540  2024-07-16 16:00:00 2024-07-16 16:06:00
    ## 787  2024-07-16 17:00:00 2024-07-16 17:07:27
    ## 1075 2024-07-16 20:19:00 2024-07-16 20:21:12

Now we can use the detection datetime to make a unique filename. In this
example we will put chunks in species folders.

``` r
#make the new filename. Use __ to infer unknown species at this point
df$newfilename <- paste0(format(df$detection_dt, "%Y%m%d-%H%M%S"),
                         '-',
                         df$loc,
                         '-',
                         df$rec,
                         '-',
                         df$mic,
                         '-__.',
                         df$ext)
                         

#where will chunks be saved?
path_export <- 'C:/exports'

#make the full path and filename of each clip to be exported
df$chunk_fullname <- file.path(path_export, df$birdnet_english_name, df$newfilename)
head(df$chunk_fullname)
```

    ## [1] "C:/exports/Redwing/20240716-140806-ST4037-BTO-XM-__.wav"                  
    ## [2] "C:/exports/Redwing/20240716-150400-ST4037-BTO-XM-__.wav"                  
    ## [3] "C:/exports/Redwing/20240716-160427-ST4037-BTO-XM-__.wav"                  
    ## [4] "C:/exports/Redwing/20240716-160600-ST4037-BTO-XM-__.wav"                  
    ## [5] "C:/exports/Marsh Tit/20240716-170727-ST4037-BTO-XM-__.wav"                
    ## [6] "C:/exports/Chestnut-backed Chickadee/20240716-202112-ST4037-BTO-XM-__.wav"

Now we can do the exporting using AcousticTools::extract_chunk. In this
example I will export 5 second chunks centred on the 3 second BirdNet
detections.

``` r
#iterate over chunks - just do first 10 for demo. 
n <- nrow(df)
n <- 10
for(i in 1:n) {
  AcousticTools::extract_chunk(file_wav = df$original_wav[i],
                               file_chunk = df$chunk_fullname[i],
                               start = df$start[i], 
                               end = df$end[i], 
                               chunk_duration = 5,
                               verbose = TRUE)
  
}
```

## Fully anonymised chunk names

If you want to export fully anonymised files this can be done as
follows. This date time steps can be ignored in this case. **Remember to
export the lookup so you know what L4G4Y06Y3YF2TFF.wav actually was!**

``` r
library(stringi)
```

    ## Warning: package 'stringi' was built under R version 4.2.2

``` r
#how many files need anonymised names
num_files <- nrow(df)
#make random strings of 15 characters and/or numbers
df$randstrings <- stringi::stri_rand_strings(n = num_files, length = 15, pattern = "[A-Z0-9]")
#make into filenames
df$chunk_fullname_anonymised <- file.path(path_export, paste0(df$randstrings, ".wav"))
head(df$chunk_fullname_anonymised)
```

    ## [1] "C:/exports/5ITCNU7P6WCGX8G.wav" "C:/exports/RFY9ZO9OD85175D.wav"
    ## [3] "C:/exports/LH23QWOAT4NIQW5.wav" "C:/exports/PG296924EFKNAJO.wav"
    ## [5] "C:/exports/OIRBXTYH94N5776.wav" "C:/exports/PG18L4TYHMXA55U.wav"

``` r
#remember to save the lookup!
save(df, file = file.path(path_export, "detection_to_anonymised_names_lookup.Rdata"))
```
