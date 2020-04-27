

#' Create a file directory for a new refuge I&M project
#'
#' @description A function to create a new file directory structure. The directory structure is based off the Alaska I&M biometric project directory template.
#'
#' @param dir.name Location to create new root project folder. Default is the current working directory.
#' @param proj.name The name of the project to be used as the name of the root project folder.
#' @param moreFolders List or vector containing character strings specifying full directory paths for additional
#' folders to be included in project directory.
#'
#' @example \dontrun{create.dir(proj.name = "bear_survey")}
#' @export

create.dir <- function(proj.name = NULL, dir.name = NULL, moreFolders = NULL) {

    ## project name
    if(is.null(proj.name))proj.name = "myproject"

    ## root project folder
    if(is.null(dir.name))dir.name = "."
    root.dir = paste0(dir.name, "/", proj.name)
    dir.create(root.dir)

    ## primary project subfolders
    f <- paste0(root.dir, "/", c("admin", "code", "data", "incoming", "metadata", "output", "products",
                                 "protocols", "resources"))
    lapply(f, dir.create)

    ## /admin
    f <- paste0(root.dir, "/admin/", c("contracts", "meetings", "proposals", "purchasing", "training", "travel"))
    lapply(f, dir.create)

    ## /code
    f <- paste0(root.dir, "/code/", c("functions"))
    lapply(f, dir.create)

    ## /data
    f <- paste0(root.dir, "/data/", c("derived", "raw"))
    lapply(f, dir.create)

    ## /output
    f <- paste0(root.dir, "/output/", c("figures", "raw_analysis", "tables"))
    lapply(f, dir.create)

    ## /products
    f <- paste0(root.dir, "/products/", c("conceptual_model", "posters", "presentations", "publications",
                                          "reports"))
    lapply(f, dir.create)

    ## /protocols
    f <- paste0(root.dir, "/protocols/", c("draft_elements"))
    lapply(f, dir.create)

    ## /resources
    f <- paste0(root.dir, "/resources/", c("data", "geodata", "images", "publications", "reports"))
    lapply(f, dir.create)

    f <- paste0(root.dir, "/resources/", c("data", "geodata", "images", "publications", "reports"))
    lapply(f, dir.create)

    ## user-provided additional folders to be created
    if(!is.null(moreFolders))lapply(moreFolders, dir.create)
}


#### ------------------------------------------------------------------


#' Delete all raw and derived data and products
#'
#' @export
#'
#' @example \dontrun{startover()}

startover <- function(){

    files <- list.files("../data/raw", full.names = TRUE)
    file.remove(files[!grepl("html", files)])

    files <- list.files("../data/derived", full.names = TRUE)
    file.remove(files[!grepl("html", files)])

    files <- list.files("../data/final", full.names = TRUE)
    file.remove(files[!grepl("html", files)])

    files <- list.files("../incoming", full.names = TRUE)
    file.remove(files[!grepl("html", files)])

    files <- list.files("../output/raw_analysis", full.names = TRUE)
    file.remove(files[!grepl("html", files)])

    files <- list.files("../products/reports/annual_analysis", full.names = TRUE)
    file.remove(files[!grepl("html", files)])

    files <- list.files("../products/reports/annual_qc", full.names = TRUE)
    file.remove(files[!grepl("html", files)])

    files <- list.files("../resources/data/geodata", full.names = TRUE)
    file.remove(files[!grepl("html", files)])
}

