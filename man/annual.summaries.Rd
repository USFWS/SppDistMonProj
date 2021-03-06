% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_functions.R
\name{annual.summaries}
\alias{annual.summaries}
\title{Create year-specific summary table and figures and LaTeX code for QC report}
\usage{
annual.summaries(occu, site, year, unmkd, naiveoccu, habcov, figdir.path)
}
\arguments{
\item{occu}{dataframe containing raw species occurrence data}

\item{site}{dataframe containing raw site-level data}

\item{year}{numeric value for survey year}

\item{unmkd}{\code{unmarkedFrameOccu} object used by unmarked package to fit single-season occupancy model}

\item{naiveoccu}{vector containing values of 0 or 1 for each surveyed site indicating whether
the species was observed (1) or unobserved (0)}

\item{habcov}{rasterLayer object containing habitat covariate values}

\item{figdir.path}{directory path for temporary storage of report figures}
}
\value{
List of length equal to number of survey years processed. Each element is contains a NULL value.
}
\description{
Create year-specific summary table and figures and LaTeX code for QC report
}
