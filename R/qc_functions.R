

#' Create Year-Specific Summary Table and Figures and LaTeX Code for QC Report
#'
#' @param year numeric value for survey year
#' @param naiveoccu vector containing values of 0 or 1 for each surveyed site indicating whether
#' the species was observed (1) or unobserved (0)
#' @param habcov rasterLayer object containing habitat covariate values
#' @param figdir.path directory path for temporary storage of report figures
#' @param occu dataframe containing raw species occurrence data
#' @param site dataframe containing raw site-level data
#' @param unmkd \code{unmarkedFrameOccu} object used by unmarked package to fit single-season occupancy model
#'
#' @return List of length equal to number of survey years processed. Each element is contains a NULL value.
#' @export

annual.summaries <- function(occu, site, year, unmkd, naiveoccu, habcov, figdir.path){
    ##
    cat("\\subsection{Summary information for species occurrence data collected in ", year, "}\n", sep = "")
    cat("\\clearpage\n")
    cat("\\subsubsection{Summary table}\n\n\n", sep = "")
    observer.table = table(occu[,c("id","observer")])
    summary.table = xtable::xtable(observer.table,
                            caption = "Table summarizing which sites were surveyed by each observer",
                            label = "summarytable")
    add.to.row = list(pos = list(0), command = NULL)
    command = paste0("\\hline\n\\endhead\n",
                      "\\hline\n",
                      "\\multicolumn{", dim(summary.table)[2] + 1, "}{l}",
                      "{\\footnotesize Continued on next page}\n",
                      "\\endfoot\n",
                      "\\endlastfoot\n")
    add.to.row$command = command
    xtable::print.xtable(summary.table, tabular.environment = "longtable", add.to.row = add.to.row,
          floating = FALSE, caption.placement = "top", include.rownames = TRUE, hline.after = c(-1))
    ##
    cat("\\subsubsection{Site-specific naive occupancy}\n",
        sep = "")
    png(paste0(figdir.path, "naiveoccu_", year, ".png"), width = 6.5, height = 6.5, units = "in", res = 96)
    rasterpts = data.frame(raster::rasterToPoints(habcov))
    ss = as.numeric(names(naiveoccu))
    raster::plot(habcov, xlab = "X", ylab = "Y", asp = 1)
    points(rasterpts$x[ss], rasterpts$y[ss], pch = c(21,16)[naiveoccu + 1],
           col = "blue", bg = c(NA,"blue")[naiveoccu + 1], cex = 2)
    dev.off()
    cat("\\includegraphics[width=\\maxwidth]{figure/naiveoccu_", year, ".png}\n", sep = "")
    cat("\\newline\n")
    cat("Naive occupancy in ", year, "\n", sep = "")
    cat("\\clearpage\n")
    ##
    cat("\\subsubsection{Site-specific number of occasions with positive detection}\n",
        sep = "")
    png(paste0(figdir.path, "sitedets_", year, ".png"), width = 6.5, height = 6.5, units = "in", res = 96)
    raster::plot(habcov, alpha = 1, xlab = "X", ylab = "Y", asp = 1)
    labs = rowSums(slot(unmkd, "y"))
    ss = as.numeric(names(labs))
    rasterpts = data.frame(rasterToPoints(habcov))
    text(rasterpts$x[ss], rasterpts$y[ss], labels = labs, cex = 0.75, font = 2)
    dev.off()
    cat("\\includegraphics[width=\\maxwidth]{figure/sitedets_", year, ".png}\n", sep = "")
    cat("\\newline\n")
    cat("Number of detections at each site in ", year, "\n", sep = "")
    cat("\\clearpage\n")
    ##
    cat("\\subsubsection{Occasion-specific survey visibility conditions}\n",
        sep = "")
    png(paste0(figdir.path, "visibility_",year, ".png"), width = 6.5, height = 6.5, units = "in", res = 96)
    mean.vis = sapply(split(occu, occu$occasion),
                       function(x)mean(ifelse(x$visibility=="good",1,0)))
    barplot(mean.vis, names.arg = paste0("Occasion ", 1:length(mean.vis)), xlab = "Survey occasion",
            ylab = "Proportion of surveys with good visibility", ylim = c(0,1))
    dev.off()
    cat("\\includegraphics[width=\\maxwidth]{figure/visibility_", year, ".png}\n", sep = "")
    cat("\\newline\n")
    cat("Occasion-specific proportions of surveys with good visibility in ", year, "\n", sep = "")
    cat("\\clearpage\n")
    ##
    cat("\\subsubsection{Sample distribution of habitat covariate}\n",
        sep = "")
    png(paste0(figdir.path, "sample_habcov_",year, ".png"), width = 6.5, height = 6.5, units = "in", res = 96)
    hist(site$habcov, breaks = seq(-3,3,0.2), ylim = c(0,10),
         main = "", xlab = "Habitat covariate")
    dev.off()
    cat("\\includegraphics[width=\\maxwidth]{figure/sample_habcov_", year, ".png}\n", sep = "")
    cat("\\newline\n")
    cat("Sample distribution of habitat covariate for surveyed sites in ", year, "\n", sep = "")
    cat("\\clearpage\n")
    ##
    cat("\\subsubsection{Habitat covariate vs observed occurrence}\n",
        sep = "")
    png(paste0(figdir.path, "habcov_vs_naiveoccu_",year, ".png"),
        width = 6.5, height = 6.5, units = "in", res = 96)
    plot(site$habcov, naiveoccu, xlim = c(-3,3),
         ylab = "Observed occurrence", xlab = "Habitat covariate")
    dev.off()
    cat("\\includegraphics[width=\\maxwidth]{figure/habcov_vs_naiveoccu_", year, ".png}\n", sep = "")
    cat("\\newline\n")
    cat("Scatter plot of habitat covariate vs observed occurrence for surveyed sites in ", year, "\n", sep = "")
    cat("\\clearpage\n")
}


#### ------------------------------------------------------------------


#' Extract Year From Data File Name
#'
#' @param filename full file path to data file
#'
#' @return character string
#' @export

extract.year <- function(filename){
    if(grepl("occurrence", filename)) ind = gregexpr("_", filename)[[1]][2]
    if(grepl("sample_site", filename)) ind = gregexpr("_", filename)[[1]][3]
    substr(filename, ind + 1, ind + 4)
}


#### ------------------------------------------------------------------


#' Calculate Naive Species Occurrence for Surveyed Sites
#'
#' @param x list of processed data objects created using the \code{process.data} function
#'
#' @return vector containing values of 0 or 1 for each surveyed site indicating whether the species was observed (1) or unobserved (0)
#' @export

naive.occu <- function(x){
    naiveoccu = apply(slot(x, "y"), 1, max)
    class(naiveoccu) = c("NaiveOccu", "integer")
    naiveoccu
}


#### ------------------------------------------------------------------


#' Create Formatted Data Object for Analysis Using unmarked Package
#'
#' @param sitedf dataframe containing raw site-level data
#' @param occudf dataframe containing raw species occurrence data
#' @param year numeric value for survey year
#'
#' @return Object of class \code{unamrkedFrameOccu}
#' @export

make.unmarkedData <- function(sitedf, occudf, year){
    ymat <- as.matrix(table(occudf[occudf$y=="yes", c("id","occasion")]))
    attributes(ymat)$class <- "matrix"
    ## Create site covariate data.frame for unmarked package
    occ.sites <- as.numeric(rownames(ymat))
    sitecovs <- data.frame(habcov =  sitedf$habcov[match(occ.sites, sitedf$id)])
    ## Create occasion covariate data.frame for unmarked package
    viscov <- matrix(0L, nrow(ymat), ncol(ymat), dimnames = dimnames(ymat))
    for(i in 1:nrow(occudf)){
        viscov[match(occudf$id[i], rownames(viscov)),
               match(occudf$occasion[i], colnames(viscov))] <- as.character(occudf$visibility[i])
    }
    viscov <- as.data.frame(viscov)
    ## Create unmarkedFrameOccu data object for unmarked package
    unmarkedData <- unmarkedFrameOccu(y=ymat,
                                      siteCovs=sitecovs,
                                      obsCovs=list(viscov=viscov))
    unmarkedData
}


#### ------------------------------------------------------------------


#' Process Raw Occurrence and Site-Level Data for Analysis
#'
#' @param occudfs list of dataframes containing annual raw species occurrence data
#' @param sitedfs list of dataframes containing annual raw site-level data
#' @param years numeric vector of survey years
#' @param habcov object of class \code{RasterLayer} containing spatial covariate
#'
#' @return list of length 6 with the following elements: 1) numeric vector of survey years, 2) list of year-specific
#' data objects of class unmarkedFrameOccu, 3) list of year-specific raw species occurrence data.frames, 4) list of
#' year-specific site-level data.frames, 5) list of year-specific numeric vectors containing naive occurrence values,
#' and 6) object of class RasterLayer containing a spatial covariate.
#' @export

process.data <- function(occudfs, sitedfs, years, habcov){
    siteDataList = lapply(sitedfs, function(x){
        class(x) = c("sitedf", "data.frame")
        x
    })
    class(siteDataList) = c("siteDataList", "list")
    names(siteDataList) = paste0("Y", years, "sitedf")
    occuDataList = lapply(occudfs, function(x){
        x = x[,c("Site.ID", "Survey.occasion", "Date", "Visibility",
                 "Observed.species.occurrence", "Observer.name")]
        colnames(x) = c("id", "occasion", "date", "visibility", "y", "observer")
        x$occasion = as.factor(x$occasion)
        x$id = as.factor(x$id)
        class(x) = c("occudf", "data.frame")
        x
    })
    class(occuDataList) = c("occuDataList", "list")
    names(occuDataList) = paste0("Y", years, "occudf")
    ## Create observation data matrix for unmarked package
    unmarkedDataList = mapply(make.unmarkedData, siteDataList, occuDataList, years, SIMPLIFY = FALSE)
    names(unmarkedDataList) <- paste0("Y", years, "unmarkedData")
    class(unmarkedDataList) <- c("unmarkedDataList", "list")
    ## Derive naive species occurrence
    NaiveOccu <- lapply(unmarkedDataList, naive.occu)
    names(NaiveOccu) <- paste0("Y", years, "NaiveOccu")
    class(NaiveOccu) <- c("NaiveOccuList", "list")
    ## Add habitat covariate data
    habcovData <- habcov
    ## Return list of class-specific data objects
    return(list(years = years, unmarkedDataList = unmarkedDataList,
                occuDataList = occuDataList, siteDataList = siteDataList,
                NaiveOccu = NaiveOccu, habcovData = habcovData))#, viscov = viscov))
}
