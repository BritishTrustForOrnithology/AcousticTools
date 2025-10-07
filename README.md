# AcousticTools
R package for supporting BTO acoustic data analyses. 

## Installation

``` r
devtools::install_github('BritishTrustForOrnithology/AcousticTools')
```

## Main tools

The main functions so far are for:

* renaming audio files in more helpful formats.
* getting the duration of audio files without reading the whole file
* reading and manipulating BTO Acoustic Pipeline results csv files
* reading and manipulating BirdNET outputs
* saving audio chunks for species detections in species folders
* creating empty Audacity label files for each audio files
* downloading and manipulating audio clips from xeno-canto
* checking and validating Audacity label files
* organising audio/label files into species folders

## Assumptions

In several cases these functions assume that audio files are (or will be) in a standard naming format, consisting of:

YYYYMMDD-HHMMSS-LocationName-RecordistName-EquipmentCode-SpeciesCode.wav 
or
YYYYMMDD-HHMM-LocationName-RecordistName-EquipmentCode-SpeciesCode.wav 

Using a consistent format makes it easier to write functions that know where to look for dates, times and species information. Also, making files in a common format with location name etc encoded in the filename reduces downstream confusion if similarly named files are divorced from their parent folders.

## Usage

### Labelled audio management

After labelling (annotating) audio files the wav+txt files need to be moved into relevant taxa and species folders. At the same time it is sensible to run some QA checks on the label files, e.g. to check for missing times, incorrect species codes, or incorrect channel labels. There are two functions to assist with this:

* check_labels() will return a list of common errors in a folder of labels.
* organise_labelled_clips() will move all audio+txt files from one folder to the relevant species folders


### Possible Acoustic Pipeline workflow

The following workflow could be used with the Pipeline:

1. Use rename_songmeter_files() to convert all audio files to the preferred name 
format. This will make it easier later if any sampling is required by date/time
2. Run the files through the BTO Acoustic Pipeline. Save (or download) the 
results csv file. 
3. Run read_pipeline_results() to read and manipulate the outputs into a standard 
format. This will return a dataframe. Save for next step
4. Apply user-selected protocols to shortlist clips for verification. e.g. all 
of species A and B, but only a 10% sample stratified by site, for species C. Use 
this method to produce a dataframe where each row is a detection you want to 
export. 
5. For each row, make a unique filename for what you want that chunk to be 
called.
6. Now use extract_chunk() applied to each row of the dataframe produced in 5) 
to produce short audio chunks for manual checking

A full working example of this approach can be found [here](https://github.com/BritishTrustForOrnithology/AcousticTools/blob/main/example_extracting_chunks_AcousticPipeline.md)


### Possible BirdNET workflow

1. Use rename_songmeter_files() to convert all audio files to the preferred name 
format. This will make it easier later if any sampling is required by date/time
2. Run BirdNET using the BirdNET GUI and select one of R, CSV or Audacity output 
formats
3. Run read_birdnet_results() to read and collate the outputs into a standard 
format. This will return a dataframe. Save for next step
4. Apply user-selected protocols to shortlist clips for verification. e.g. all 
of species A and B, but only a 10% sample stratified by site, for species C. Use 
this method to produce a dataframe where each row is a detection you want to 
export. 
5. For each row make a unique filename for what you want that chunk to be 
called.
6. Now use extract_chunk() applied to each row of the dataframe produced in 5) 
to produce short audio chunks for manual checking

A full working eample of this approach can be found [here](https://github.com/BritishTrustForOrnithology/AcousticTools/blob/main/example_extracting_chunks_BirdNET.md)

A more involved work eample using stratified sampling can be found [here](https://github.com/BritishTrustForOrnithology/AcousticTools/blob/main/example_stratified_sampling_chunks_BirdNET.md)

Simon Gillings
v2 December 2024
