
#' Create an analysis report
#'
#' @param interface if \code{TRUE}, user inputs the report parameters using a shiny interface
#' @param data input data
#' @param year a numeric value indicating the year of the desired report
#' @param author the name of the report author
#' @param output_file output file name in \link{render}. Default is "report.html".
#' @param output_dir output directory for report in \link{render}. Default is user's current directory.
#' @param \dots other arguments to be passed to \link{render}.
#'
#' @return
#' @export
#'
#' @example
#' \dontrun{
#' analysis.report(data = "data.RData",
#'                 year = 2019)
#' }

# https://github.com/boxuancui/DataExplorer/blob/master/inst/rmd_template/report.rmd

analysis.report <- function(interface = T,
                            data,
                            year = 2019,
                            author = "Cliff Clavin",
                            email = "cliff@fws.gov",
                            phone = "555-867-5309",
                            output_file = "report.pdf",
                            output_dir = getwd(),
                            ...) {

  report_dir <- system.file("rmd/analysis_report.rmd", package = "SppDistMonProj")

  if (interface == TRUE){
    # Render report into html
    suppressWarnings(rmarkdown::render(
      input = report_dir,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = "ask",
      ...
    ))
  }

  if (interface == FALSE){
    # Render report into html
    suppressWarnings(rmarkdown::render(
      input = report_dir,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(data = data,
                    year = year,
                    author = author,
                    email = email,
                    phone = phone),
      ...
    ))
  }

  # Open report
  report_path <- path.expand(file.path(output_dir, output_file))
  browseURL(report_path)
}


# ----

#' Create a quality control report
#'
#' @param interface if \code{TRUE}, user inputs the report parameters using a shiny interface
#' @param o_dir directory path to a folder containing the input occurrance Rdata files
#' @param s_dir directory path to a folder containing the input sample site Rdata files
#' @param hab_data an ASC raster file containing the input habitat covariate data
#' @param author the name of the report author
#' @param output_file output file name in \link{render}. Default is "report.html".
#' @param output_dir output directory for report in \link{render}. Default is user's current directory.
#' @param \dots other arguments to be passed to \link{render}.
#'
#' @return
#' @export
#'
#' @example
#' \dontrun{
#' qc.report(interface = FALSE, o_dir = "./data/raw", s_dir = "./data/raw",
#' hab_data = "./resources/data/geodata/habcov.asc)
#' }

qc.report <- function(interface = T,
                      o_dir = "C:/Users/mcobb/Desktop/r_webinar/docs/mccrea/demo/data/raw",
                      s_dir = "C:/Users/mcobb/Desktop/r_webinar/docs/mccrea/demo/data/raw",
                      hab_data = "./resources/data/geodata/habcov.asc",
                      author = "Cliff Clavin",
                      email = "cliff@fws.gov",
                      phone = "555-867-5309",
                      output_file = "report.pdf",
                      output_dir = getwd(),
                      ...) {

  report_dir <- system.file("rmd/qc_report.rmd", package = "SppDistMonProj")

  if (interface == TRUE){
    # Render report into html
    suppressWarnings(rmarkdown::render(
      input = report_dir,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = "ask",
      ...
    ))
  }

  if (interface == FALSE){
    # Render report into html
    suppressWarnings(rmarkdown::render(
      input = report_dir,
      output_file = output_file,
      output_dir = output_dir,
      intermediates_dir = output_dir,
      params = list(o_dir = o_dir,
                    s_dir = s_dir,
                    hab_data = hab_data,
                    author = author,
                    email = email,
                    phone = phone),
      ...
    ))
  }

  # Open report
  report_path <- path.expand(file.path(output_dir, output_file))
  browseURL(report_path)
}
