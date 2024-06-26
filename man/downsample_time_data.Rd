% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/downsample_time_data.R
\name{downsample_time_data}
\alias{downsample_time_data}
\title{Downsample frequency to reduce number of samples and data size}
\usage{
downsample_time_data(data, pupil, timebin_size, option = c("mean", "median"))
}
\arguments{
\item{data}{your data of class PupillometrySENDA}

\item{pupil}{a column name denoting pupil size}

\item{timebin_size}{the size of the new timebin you wish to use}

\item{option}{what should be calculated in each timebin - mean or median. Defaults to mean.}
}
\value{
A downsampled dataframe of class PupillometrySENDA
}
\description{
This function is useful if you were sampling at a very high frequency (eg 500Hz)
causing the data size to be hard to manage, and high autocorrelation.
Careful decisions should be made about the time bin size and appropriateness
of this function, with respect to the data type.
}
\examples{
data(pupil_data)
Sdata <- make_pupillometrysenda_data(data = pupil_data,
subject = "ID",
trial = "Trial",
time = "Time",
condition = "Type")
new_data <- downsample_time_data(data = Sdata,
pupil = "LPupil",
timebin_size = 50,
option = "mean")
}
