% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_pipeline_results.R
\name{read_pipeline_results}
\alias{read_pipeline_results}
\title{Read and format BTO Acoustic Pipeline results file}
\usage{
read_pipeline_results(file_csv, date_time_sep = "_")
}
\arguments{
\item{file_csv}{= string, the path and filename of the results csv file}

\item{date_time_sep}{= the character used to separate date and time in the
original file names. Usually hyphen or underscore. Needed to correctly parse
original filenames}
}
\value{
a dataframe. Note that offset is the time (seconds) of the start of
the audio chunk in which the detection is located. Chunk length is determined
by the classifier used. For example, an offset of 10, for a classifier that
has a 4-second chunk length, means the detection is between 10 and 14 seconds
into the file.
}
\description{
Read and prepare the standard results csv file generated by the Acoustic Pipeline.
}
\details{
After reading the file certain date time fields are manipulated. For example,
the recording start is inferred from the original file name. This and the
detection date time are used to infer the offset in the file of each detection.
For this users need to supply whether dates and times are separated by a hyphen
(-) or underscore (_) to ensure correct creation of start times.
}
