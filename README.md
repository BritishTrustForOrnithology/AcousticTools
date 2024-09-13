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