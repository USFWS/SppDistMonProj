

#' Create Spatial Raster Object Containing Simulated Habitat Covariate Values
#'
#' @description
#' Create spatial raster object for a spatially correlated habitat covariate used to simulate species occurrence.
#'
#' @param seed A single value used to set seed for replicability.
#' @param ngr A single value for number of rows and columns in site grid, e.g., ngr = 10 creates a 10x10 grid.
#'
#' @return A RasterLayer as from \code{\link{raster}}.
#' @export
#'
#' @examples
#' create.raster(ngr = 10)

create.raster <- function(seed = 1234, ngr = 10){
    set.seed(seed)
#    ngr <- 10 # number of rows and columns in grid
    ncells <- ngr^2
    gr <- expand.grid(1:ngr, 1:ngr) # coords for all grid cells
    Dmat <- as.matrix(dist(gr)) # distance matrix
    V <- exp(-Dmat / 0.3125) # vcv matrix
    C <- t(chol(V)) %*% rnorm(ngr^2) # MVN covariate w/ mu=0,vcv=V, & pairwise correlations decaying exponentially
    ## create SpatialGridDataFrame w/ binary covariate
    grtop <- sp::GridTopology(cellcentre.offset = c(1,1), cellsize = c(1,1), cells.dim = c(ngr,ngr))
    sgr <- sp::SpatialGrid(grtop)
    sgrdf <- sp::SpatialGridDataFrame(sgr, data.frame(habcov = C))
    ## transform SpGridDF to RasterLayer
    rast <- raster::raster(sgrdf)
    ##coordsf <- coordinates(rast)
    return(rast)
}


#### ------------------------------------------------------------------


#' Create Year-Specific Site Data for Random Sample of Survey Sites
#'
#' @description
#' Create site-level data object for sites to be surveyed conditional on year.
#'
#' @param year Numeric value for survey year.
#' @param habcov Data.frame containing IDs, x-y coordinates, and habitat covariate values for all sites.
#' @param nsites Number of sites to randomly select from all sites that will be surveyed.
#'
#' @details Note: year is used to set the seed for random site selection process.
#' @return A data.frame containing IDs, x-y coordinates, and habitat covariate values for selected survey sites.
#' @export
#'
#' @examples
#' rast <- create.rast()
#' habcov <- data.frame(id = as.factor(1:ncell(rast)), rasterToPoints(rast))
#' site.data <- create.site.data(year = 2020, habcov = habcov, nsites = 20)
#' str(site.data)
#'

create.site.data <- function(year, habcov, nsites){
    set.seed(year)
    site.data <- habcov[sample(1:nrow(habcov), nsites),]
    return(site.data)
}


#### ------------------------------------------------------------------


#' Simulate Species Occurrence and Observation Data
#'
#' @param year Numeric value for survey year.
#' @param habcov Data.frame containing IDs, x-y coordinates, and habitat covariate values for all sites.
#' @param site.data Data.frame containing site-level data for surveyed sites.
#' @param nocc Number of survey occasions conducted per site.
#' @param p Data.frame containing detection probabilities.
#' @param observer.names Vector of character strings containing names of observers conducting surveys.
#' @param vis.probs Vector of length 2 containing probability of "good" and "poor" visibility occurring any given survey.
#' @param meanoccu Value between 0 and 1 used as intercept of linear model for species occurrence probability.
#' @param beta1 Value used as slope coefficient for relationship between species occurrence probability and a habitat covariate.
#'
#' @details Argument value for \code{p} should have 2 rows and 2 columns. Rows correspond to 2 levels of detection
#' probability corresponding to "good" (row 1) and "poor" (row2) visibility. Column 1 contains
#' category names (good/poor) as character strings and column 2 contains numeric values for p.
#'
#' Default value for \code{meanoccu} argument is NULL which results in a random value selected
#' from a set of values ranging between 0.6 and 0.9 at 0.05 increments. Actual intercept value used in data
#' simulation is transformed to the logit scale.
#'
#' Default value for \code{beta1} is 2 on the logit scale.
#' @return Data.frame containing all simulated species occurrence and observation data.
#' @export
#'
#' @examples
#' nocc <- 5
#' p <- data.frame(label = c("good","poor"), p = c(0.7,0.3))
#' occu.data <- sim.occu.data(year = 2020, habcov = habcov, site.data = site.data, nocc = nocc, p = p)
#' str(occu.data)
#'

sim.occu.data <- function(year, habcov, site.data, nocc, p, observer.names = NULL, vis.probs = c(0.7, 0.3),
                          meanoccu = NULL, beta1 = 2){
    set.seed(year)
    nsites <- nrow(site.data)
    if(is.null(meanoccu)) meanoccu <- sample(seq(0.6,0.9,0.05), 1)
    if(is.null(observer.names)) observer.names <- c("peterson_norman", "tortelli_carla", "malone_samuel",
                                                    "clavin_clifford", "pantusso_ernie", "chambers_diane")
    observers <- as.factor(sample(observer.names, nocc * nsites, replace = TRUE))
    visibility <- as.factor(sample(c("good","poor"), nocc * nsites, replace = TRUE, prob = vis.probs))
    beta0 <- qlogis(meanoccu)
    beta1 <- 2
    lpsi <- beta0 + habcov$habcov * beta1
    psi <- exp(lpsi) / (exp(lpsi) + 1)
    trueoccu <- rbinom(n = nrow(habcov), size = 1, prob = psi)
    occu.data <- do.call("rbind",
                        replicate(n = nocc,
                                  expr = data.frame(id = as.factor(1:nrow(habcov)),
                                                    observer = observers,
                                                    trueoccu = trueoccu)[site.data$id,],
                                  simplify = FALSE))
    occu.data$occasion <- sort(rep(1:nocc,nsites))
    occu.data$viscov <- visibility
    occu.data$p <- p[as.numeric(occu.data$viscov), "p"]
    occu.data$y <- occu.data$trueoccu * rbinom(n = nrow(occu.data), size = 1, prob = occu.data$p)
    return(occu.data)
}


#### ------------------------------------------------------------------


#' Transform Occu.data Object to Survey123 Format
#'
#' @description Transforms a occu.data object to a data.frame that matches the format of data exported from Survey123.
#' @param occu.data Data.frame containing species occurrence and observation data.
#' @param year Numeric value for survey year.
#' @export
#'
#' @return A data.frame formatted the same as data exported from Survey123.
#'

df2s123df <- function(occu.data, year){
    nr <- nrow(occu.data)
    s123df <- data.frame(integer(nr), character(nr), character(nr), character(nr), character(nr),
                           character(nr), character(nr), character(nr), integer(nr), integer(nr),
                           character(nr), character(nr), double(nr), double(nr),
                           stringsAsFactors=FALSE)
    dimnames(s123df)[[2]] <- row.names <- c("ObjectID", "GlobalID", "CreationDate", "Creator", "EditDate",
                                             "Editor", "Observer name", "Date","Survey occasion", "Site ID",
                                             "Visibility", "Observed species occurrence", "x", "y")
    chars <- c(letters, 0:9)
    fn1 <- function(chars){paste(paste0(sample(chars, 8, replace = TRUE), collapse = ""),
                                   paste0(sample(chars, 4, replace = TRUE), collapse = ""),
                                   paste0(sample(chars, 4, replace = TRUE), collapse = ""),
                                   paste0(sample(chars, 4, replace = TRUE), collapse = ""),
                                   paste0(sample(chars, 12, replace = TRUE), collapse = ""), sep = "-")
    }
    s123df$ObjectID <- 1:nr
    s123df$GlobalID <- replicate(nr, fn1(chars))
    s123df$CreationDate <- as.POSIXct(paste0("6/",occu.data$occasion,"/",years," 12:00"),
                                        format = "%m/%d/%Y %H:%M", tz = "UTC")
    s123df$Creator <- "jared_laufenberg@fws.gov_fws"
    s123df$EditDate <- as.POSIXct(paste0("6/",occu.data$occasion,"/",years," 12:00"),
                                        format = "%m/%d/%Y %H:%M", tz = "UTC")
    s123df$Editor <- "jared_laufenberg@fws.gov_fws"
    s123df$'Observer name' <- as.character(occu.data$observer)
    s123df$Date <- as.POSIXct(paste0("6/",occu.data$occasion,"/",years," 12:00"),
                                        format = "%m/%d/%Y %H:%M", tz = "UTC")
    s123df$'Survey occasion' <- occu.data$occasion
    s123df$'Site ID' <- occu.data$id
    s123df$Visibility <- as.character(occu.data$viscov)
    s123df$'Observed species occurrence' <- ifelse(occu.data$y==1,"yes","no")
    s123df$x <- 0
    s123df$y <- 0
    return(s123df)
}


#### ------------------------------------------------------------------


#' Simulate, Format, and Save Survey Site and Observation Data
#'
#' @param year Numeric value for survey year.
#' @param habcov Data.frame containing IDs, x-y coordinates, and habitat covariate values for all sites.
#' @param nsites Number of sites to randomly select from all sites that will be surveyed.
#' @param nocc Number of survey occasions conducted per site.
#' @param p Data.frame containing detection probabilities.
#' @param showcoords Logical value for whether to display coordinates for species location (TRUE)
#' or site name (FALSE) as main title.
#' @param cex Symbol size.
#' @param ngood Number of symbols used under good visibility conditions.
#' @param npoor Number of symbols used under poor visibility conditions.
#' @param write.data Logical value specifying whether to save site and occurrence data sets to raw data folder.
#' @param write.images Logical value specifying whether to save images for class exercise.
#'
#' @return List of length 3 containing site.data, occu.data, and s123.data.
#' @export
#'
one.step <- function(year, habcov, nsites, nocc, p, showcoords, cex, ngood, npoor,
                     write.data = FALSE, write.images = FALSE, return.data = FALSE){
    site.data <- create.site.data(year, habcov, nsites)
    occu.data <- sim.occu.data(year, habcov, site.data, nocc, p)
    s123.data <- df2s123df(occu.data, years)
    if(write.data){
        write.csv(s123.data, file = paste0("../data/raw/occurrence_data_", year, "_raw.csv"),
                  row.names = FALSE)
        write.csv(site.data, file = paste0("../data/raw/sample_site_data_", year, "_raw.csv"),
                  row.names = FALSE)
    }
    if(write.images){
        df <- occudata.i[1:nsites,]
        mapply(trial.plot, good = df$viscov=="good", z = df$trueoccu,
               fname = paste0("Year", year, "_Site", df$id, "_",
                              as.character(df$viscov), "_", df$trueoccu),
               seed = 1:nsites, site = paste0("Site ", df$id),
               MoreArgs = list(path = "../products/presentations", showcoords = showcoords,
                               ngood = ngood, npoor = npoor, cex = cex))
    }
    if(return.data)
    return(data.list = list(site.data = site.data, occu.data = occu.data, s123.data = s123.data))
}


