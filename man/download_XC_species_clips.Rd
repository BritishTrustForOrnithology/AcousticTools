% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_xc_species_clips.R
\name{download_XC_species_clips}
\alias{download_XC_species_clips}
\title{Download Species Recordings from xeno-canto}
\usage{
download_XC_species_clips(
  scientific_name = NULL,
  species_code = NULL,
  path_out,
  month_folders = TRUE,
  n,
  type = NULL,
  min_length = 10,
  max_length = NULL,
  min_quality = NULL,
  no_background = FALSE,
  seen_only = FALSE,
  no_nestlings = TRUE,
  no_noderivs = FALSE,
  no_noncomms = FALSE,
  sample = "random",
  make_labels = TRUE,
  used_clips = NULL,
  xc_key = NULL
)
}
\arguments{
\item{scientific_name, }{string, the scientific name of the required species}

\item{species_code, }{string, code to use when making new filenames}

\item{path_out, }{string, a folder where files are to be saved.}

\item{month_folders, }{bool, whether to put files in month folders.}

\item{n, }{numeric, the number of clips to return}

\item{type, }{character, a predefined vocalisation type as used in XC}

\item{min_length, }{numeric, minimum clip length in seconds, default 10.}

\item{max_length, }{numeric, maximum clip length in seconds.}

\item{min_quality, }{character, minimum acceptable quality code (A:E).}

\item{no_background, }{bool, exclude clips with named background species.}

\item{seen_only, }{bool, only retain clips where focal species was seen.}

\item{no_nestlings, }{bool, exclude clips of nestlings.}

\item{sample, }{character, strategy for sampling clips: random, short
(favouring short clips), long (favouring long clips).}

\item{make_labels, }{bool, whether to make empty Audacity label files.}

\item{used_clips, }{character vector, XC ID numbers of clips that have already
been labelled. These should be supplied as a character vector consisting of
just the numbers (i.e. omitting the XC prefix)}

\item{xc_key, }{character, your API access key}

\item{drop_noderivs, }{bool, exclude clips with a No Derivatives license.}

\item{drop_noncomms, }{bool, exclude clips with a Non-commercial license.}
}
\description{
Uses the xeno-canto API (v3) to download filtered samples
of clips for a species, renamed and organised into folders.
}
\details{
Pass in a species' scientific name and a number of clips desired. If there are
more clips than you want, three variants of sampling are offered: random, or
preferentially by file duration. Various filtering options are offered including
by clip duration, quality, license, whether seen, whether there are background
species, and whether nestlings.

All filenames are renamed to a standard format:
YYYYMMDD-HHMM-Country-XenoCantoContributor-XX-SpeciesCode-(XCref).wav

Optionally audio clips will be placed in folders by month, which can be helpful
when labeling to get a seasonal spread of vocalisations.

Optionally you can opt to export empty Audacity label files. These can make
audio labeling quicker as you can overwrite a pre-named file than have to
create a label file to match the audio file.
}
\examples{
\dontrun{
download_XC_species_clips(scientific_name = 'Galerida theklae', 
species_code = 'THELA', path_out = 'E:/Acoustics/THELA', 
month_folders = TRUE, n = 50, min_length = 10, min_quality = 'C', 
no_background = TRUE, no_nestlings = TRUE, drop_noncomms = TRUE,
sample = 'random', make_labels = TRUE)
#' 
}

}
