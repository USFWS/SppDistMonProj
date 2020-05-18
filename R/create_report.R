#' Create report
#'
#' @param data input data
#' @param year a numeric value indicating the year of the desired report
#' @param author the name of the report author
#' @param output_file output file name in \link{render}. Default is "report.html".
#' @param output_dir output directory for report in \link{render}. Default is user's current directory.
#' @param \dots other arguments to be passed to \link{render}.
#' @keywords create_report
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

analysis.report <- function(data,
                            year = 2019,
                            author = "Cliff Clavin",
                            email = "cliff@fws.gov",
                            phone = "555-867-5309",
                            output_file = "report.pdf",
                            output_dir = getwd(),
                            # config = configure_report(),
                            #report_title = "Data Profiling Report",
                            ...) {
  ## Check if input is data.table
  #if (!is.data.table(data)) data <- data.table(data)

  ## Get directory of report markdown template
  # report_dir <- "inst/rmd/analysis_report.Rmd"
  report_dir <- system.file("rmd/analysis_report.rmd", package = "SppDistMonProj")
  ## Render report into html
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
    # set_title = report_title),  # For flexible title
    ...
  ))
  ## Open report
  report_path <- path.expand(file.path(output_dir, output_file))
  browseURL(report_path)
}