% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_pupillometrysenda_data.R
\name{make_pupillometrysenda_data}
\alias{make_pupillometrysenda_data}
\title{Prepare data for pre-processing in PupillometrySENDA}
\usage{
make_pupillometrysenda_data(data, subject, trial, time, condition)
}
\arguments{
\item{data}{a raw, long form dataframe organised by subject, trial, and time.
if your data is not long form, look at tidyr for examples of conversion.}

\item{subject}{column name indicating subject ID}

\item{trial}{column name indicating trial ID. This should be unique for participants}

\item{time}{column name indicating time column (should be numeric)}

\item{condition}{column name indicating experimental condition}
}
\value{
A dataframe ready to use in PupillometrySENDA
}
\description{
This should be the first function you run as part of using PupillometrySENDA.
This will make sure your data is in the right format for processing.
This package is designed to deal with data at it comes out of the eyetracker
in a long-form csv style format. Thus data input here would be a long
dataframe, wherein each row is a single frame collected by the eyetracker.
}
\examples{
Sdata <- make_pupillometrysenda_data(data,
subject = "ID",
trial = "Trial",
time = "Time",
condition = "Type")

}
