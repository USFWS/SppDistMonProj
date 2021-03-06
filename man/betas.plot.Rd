% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_functions.R
\name{betas.plot}
\alias{betas.plot}
\title{Plot estimates and CIs of linear model coefficients for species occurrence and detection probalities}
\usage{
betas.plot(betas, years, modranks)
}
\arguments{
\item{betas}{List of length equal to number of survey years.  Each element contains a ist of length 2
containing estimates of model parameters for (1) occurrence probability and (2) detection probability}

\item{years}{numeric vector containing survey years}

\item{modranks}{dataframe containing model rankings for all survey years}
}
\description{
Plot estimates and CIs of linear model coefficients for species occurrence and detection probalities
}
