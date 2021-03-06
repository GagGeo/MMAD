% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mmr_reglin.R
\name{mmr_reglin}
\alias{mmr_reglin}
\title{Cleaning of normal variables}
\usage{
mmr_reglin(vect.linear, df)
}
\arguments{
\item{vect.linear}{character vector (normally distributed variables)}

\item{df}{data.frame}
}
\value{
data.frame
}
\description{
This function apply a regression (using glm()) on normally distributed
variables. Then the residuals are added in the original data.frame
}
