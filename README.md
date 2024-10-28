# AcousticTools
R package for BTO internal use offering various functions for working with acoustic data

## Installation

Remember that if you want to install this directly from Github using
devtools::install_github you will first need to create a Personal Access
Token. Basic instructions are:

-   Go to <https://github.com/settings/tokens>
-   Generate a new token, call it devtools and tick repo to define its
    scope
-   Before navigating away be sure to copy the token as it will not be
    shown again (if you forget you’ll need to delete this one and make a
    new one)
-   Now go to R and add the token to the .Renviron file. The easiest way
    is to use usethis::edit_r\_environ() (you may need to install the
    usethis package)
-   Using this command opens the .Renviron file for editing. Add the
    following line, substituting your token code, followed by two
    carriage returns. Then save the file, close it and restart R:
    GITHUB_PAT=blahblahblahblahblahblahblahblahblahblahblahblah

Once you’ve done this you can install the package directly:

``` r
devtools::install_github('BritishTrustForOrnithology/AcousticTools')
```


## Usage

### Possible BirdNET workflow

The following workflow could be used with BirdNET.

1. Use rename_songmeter_files() to convert all audio files to the preferred name 
format. This will make it easier later if any sampling is required by date/time
2. Run BirdNET using the BirdNET GUI and select one of R, CSV or Audacity output 
formats
3. Run read_birdnet_results() to read and collate the outputs into a standard 
format. This will return a dataframe. Save for next step
4. Apply user-selected protocols to shortlist clips for verification. e.g. all 
of species A and B, but only a 10% sample stratified by site, for species C. Use 
this method to produce a dataframe where each row is a detection you want to 
export. For each row make a unique filename for what you want that chunk to be 
called.
5. Now use extract_chunks() applied to each row of the dataframe produced in 4) 
to produce short audio chunks for manual checking


Simon Gillings
October 2024