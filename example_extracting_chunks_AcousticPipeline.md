Demo extracting clips based on Acoustic Pipeline output
================

Example code for reading Acoustic Pipeline output and exporting
detections in species subfolders

``` r
#devtools::install_github('BritishTrustForOrnithology/AcousticTools')
library(AcousticTools)

#read and format an Acoustic Pipeline output
#Need to know the original audio filename format. In this case, files had hyphen separators
df <- AcousticTools::read_pipeline_results(file_csv = "C:/Users/simon.gillings/Downloads/sample_AP_output.csv", date_time_sep = '-')
head(df)
```

    ##                                                      recording_file_name
    ## 1 20241108_190132-(52~12345+~12345)-20241108-190000-Cambridge-BTO-PS.wav
    ## 2 20241108_190304-(52~12345+~12345)-20241108-190000-Cambridge-BTO-PS.wav
    ## 3 20241108_190456-(52~12345+~12345)-20241108-190000-Cambridge-BTO-PS.wav
    ## 4 20241108_191228-(52~12345+~12345)-20241108-190000-Cambridge-BTO-PS.wav
    ## 5 20241108_191444-(52~12345+~12345)-20241108-190000-Cambridge-BTO-PS.wav
    ## 6 20241108_191508-(52~12345+~12345)-20241108-190000-Cambridge-BTO-PS.wav
    ##                     original_file_name original_file_part latitude longitude
    ## 1 20241108-190000-Cambridge-BTO-PS.WAV                  0 52.12345   0.12345
    ## 2 20241108-190000-Cambridge-BTO-PS.WAV                  0 52.12345   0.12345
    ## 3 20241108-190000-Cambridge-BTO-PS.WAV                  0 52.12345   0.12345
    ## 4 20241108-190000-Cambridge-BTO-PS.WAV                  0 52.12345   0.12345
    ## 5 20241108-190000-Cambridge-BTO-PS.WAV                  0 52.12345   0.12345
    ## 6 20241108-190000-Cambridge-BTO-PS.WAV                  0 52.12345   0.12345
    ##   species scientific_name english_name species_group score warnings   call_type
    ## 1     377  Turdus iliacus      Redwing         Birds  0.81       NA Flight call
    ## 2     377  Turdus iliacus      Redwing         Birds  0.89       NA Flight call
    ## 3     377  Turdus iliacus      Redwing         Birds  0.94       NA Flight call
    ## 4     377  Turdus iliacus      Redwing         Birds  0.96       NA Flight call
    ## 5     377  Turdus iliacus      Redwing         Birds  0.98       NA Flight call
    ## 6     377  Turdus iliacus      Redwing         Birds  0.98       NA Flight call
    ##   actual_date survey_date     time                  classifier_name user_id
    ## 1  08/11/2024  08/11/2024 19:01:32 NocturnalFlightCalls2D_v2.5.onnx  ANUSER
    ## 2  08/11/2024  08/11/2024 19:03:04 NocturnalFlightCalls2D_v2.5.onnx  ANUSER
    ## 3  08/11/2024  08/11/2024 19:04:56 NocturnalFlightCalls2D_v2.5.onnx  ANUSER
    ## 4  08/11/2024  08/11/2024 19:12:28 NocturnalFlightCalls2D_v2.5.onnx  ANUSER
    ## 5  08/11/2024  08/11/2024 19:14:44 NocturnalFlightCalls2D_v2.5.onnx  ANUSER
    ## 6  08/11/2024  08/11/2024 19:15:08 NocturnalFlightCalls2D_v2.5.onnx  ANUSER
    ##            upload_key             batch_name                     project_name
    ## 1 2024111907210QWERTY 20241108 Cambridge NFC Nocturnal Bird Migration Project
    ## 2 2024111907210QWERTY 20241108 Cambridge NFC Nocturnal Bird Migration Project
    ## 3 2024111907210QWERTY 20241108 Cambridge NFC Nocturnal Bird Migration Project
    ## 4 2024111907210QWERTY 20241108 Cambridge NFC Nocturnal Bird Migration Project
    ## 5 2024111907210QWERTY 20241108 Cambridge NFC Nocturnal Bird Migration Project
    ## 6 2024111907210QWERTY 20241108 Cambridge NFC Nocturnal Bird Migration Project
    ##            file_start     detection_start offset
    ## 1 2024-11-08 19:00:00 2024-11-08 19:01:32     92
    ## 2 2024-11-08 19:00:00 2024-11-08 19:03:04    184
    ## 3 2024-11-08 19:00:00 2024-11-08 19:04:56    296
    ## 4 2024-11-08 19:00:00 2024-11-08 19:12:28    748
    ## 5 2024-11-08 19:00:00 2024-11-08 19:14:44    884
    ## 6 2024-11-08 19:00:00 2024-11-08 19:15:08    908

Currently we have all detections. We might want to do some filtering or
sampling. For this simple example I’m just going to limit to high
scoring detections. More complex approaches, like site- or species-based
stratified sampling could be adopted. See [this
example](https://github.com/BritishTrustForOrnithology/AcousticTools/blob/main/example_stratified_sampling_chunks.md)
for one approach

``` r
#keep all high scoring detections 
df <- subset(df, score >=0.95)
```

We have already computed the date and time of the detection. To make
unique clips we need to decide on a file name format. I always use:
YYYYMMDD-HHMMSS-Locname-Recordist-EquipmentCode-Species.wav

You can create location names according to your study design. Here for
simplicity I will use the sitename “Cambridge” for simplicity

Now we can use the detection datetime to make a unique filename. In this
example we will put chunks in species folders. And the recordings were
made by BTO, so recordist is BTO. The equipment was a mono field
recorder, for which I use the code XM.

``` r
df$locname <- 'Cambridge'
df$recordist <- 'BTO'
df$equipment <- 'XM'

#make the new filename. Use __ to infer unknown species at this point
df$newfilename <- paste0(format(df$detection_start, "%Y%m%d-%H%M%S"),
                         '-',
                         df$locname,
                         '-',
                         df$recordist,
                         '-',
                         df$equipment,
                         '-__.wav')
                         

#where will chunks be saved?
path_export <- 'C:/exports'

#make the full path and filename of each clip to be exported
df$chunk_fullname <- file.path(path_export, df$english_name, df$newfilename)
head(df$chunk_fullname)
```

    ## [1] "C:/exports/Redwing/20241108-191228-Cambridge-BTO-XM-__.wav"
    ## [2] "C:/exports/Redwing/20241108-191444-Cambridge-BTO-XM-__.wav"
    ## [3] "C:/exports/Redwing/20241108-191508-Cambridge-BTO-XM-__.wav"
    ## [4] "C:/exports/Redwing/20241108-191528-Cambridge-BTO-XM-__.wav"
    ## [5] "C:/exports/Redwing/20241108-191532-Cambridge-BTO-XM-__.wav"
    ## [6] "C:/exports/Redwing/20241108-191536-Cambridge-BTO-XM-__.wav"

Now we can do the exporting using AcousticTools::extract_chunk. In this
example I will export 7 second chunks centred on the 4 second Pipeline
detections. We pass
df$offset as the start time of the detection, and knowing this classifier has a 4-second window, set end as df$offset+4.
The function handles if the chunk falls at the start or end of the audio
file and pads the clip with silence as required to make the 7s clip.

``` r
#iterate over chunks - just do first 10 for demo. 
n <- nrow(df)
n <- 10
for(i in 1:n) {
  #not run as I don't have the audio to hand!
  # AcousticTools::extract_chunk(file_wav = df$original_filename[i],
  #                              file_chunk = df$chunk_fullname[i],
  #                              start = df$offset[i], 
  #                              end = df$offset[i] + 4, 
  #                              chunk_duration = 7,
  #                              verbose = TRUE)
  
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

    ## [1] "C:/exports/LPAMRLXA6K8QII3.wav" "C:/exports/ACCJT478SCE4W47.wav"
    ## [3] "C:/exports/SALI1YI2BGCLH1C.wav" "C:/exports/3UXINZRACN750SM.wav"
    ## [5] "C:/exports/5GK9P5JT4J2RUPI.wav" "C:/exports/2W991FTP57QNC9C.wav"

``` r
#remember to save the lookup!
save(df, file = file.path(path_export, "detection_to_anonymised_names_lookup.Rdata"))
```
