% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_fixed_labels.R
\name{make_fixed_labels}
\alias{make_fixed_labels}
\title{Create fixed position label files}
\usage{
make_fixed_labels(
  position = NULL,
  label = NULL,
  overwrite = FALSE,
  subfolders = TRUE
)
}
\arguments{
\item{position}{= the position (in seconds) where the label should be created}

\item{label}{= the label code to be assigned at position}

\item{overwrite}{= whether to overwrite existing label files (default FALSE)}

\item{subfolders}{= whether to make labels for audio in subfolders (default TRUE)}
}
\description{
For every audio file in a folder, create a label file of the same name with
the specified label position and code.
}
\details{
This function is useful as a preparatory step when labelling audio files that
have been pre-verified. For a given folder of audio files the function will
make an label file with the same base name, using the provided position
(seconds) and label. Optionally the user can specify to overwrite any existing label
files, and to scan recursively.
}
