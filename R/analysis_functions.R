

#' Create Plots of Annual Estimates of Species Occurrence and Detection Probability and LaTeX Code for Analysis Report
#'
#' Estimates are based on top-ranked model and are conditional on estimated
#' relationships between model parameters and covariates when applicable.
#'
#' @param Epsi dataframe containing estimated values for occurrence probabilities, SE,
#' upper & lower CLs, and corresponding habitat covariate values
#' @param Ep dataframe containing estimated values for detection probabilities, SE,
#' upper & lower CLs, and corresponding visibility covariate values
#' @param year numeric value for survey year
#' @param figdir.path directory path for temporary storage of report figures
#'
#' @return List of length equal to number of survey years processed. Each element is contains a NULL value.
#' @export

annual.estimates <- function(Epsi, Ep, year, figdir.path){
    predvals = list(epsi = Epsi, ep = Ep)
    class(predvals) = c("predvals", "list")
    cat("\\subsection{Year ", year, " predicted values}\n", sep = "")
    png(paste0(figdir.path, "predvals_", year, ".png"), width = 9, height = 6, units = "in", res = 96)
    predvals.plot(predvals)
    dev.off()
    cat("\\includegraphics[width=6in]{figure/predvals_", year, ".png}\n", sep = "")
    cat("\\newline\n")
    cat("Model-averaged predicted values for occupancy probability ($\\Psi$) and ",
        "detection probability (\\textit{p}) for ", year, "\n", sep = "")
    #    cat("\\newpage")
}


#### ------------------------------------------------------------------


#' Create Model Selection and Parameter Estimate Tables and LaTeX Code for Analysis Report
#'
#' @param modsel.table dataframe containing model output from \code{unmarked}
#' @param year numeric value for survey year
#'
#' @return List of length equal to number of survey years processed. Each element is contains a NULL value.
#' @export

annual.tables <- function(modsel.table, year){
    cat("\\subsection{Model selection and parameter estimate tables for ", year, "}\n", sep = "")
    modseltable = modsel.table[,c("model","formula","nPars","AIC","delta","AICwt","cumltvWt")]
    indz = sapply(modseltable,class)=="numeric"
    modseltable[,indz] = round(modseltable[,indz], 2)
    summary.table = xtable::xtable(modseltable, caption = paste0("Model selection table for ", year, "."),
                           label = paste0("parmesttable-", year))
    xtable::print.xtable(summary.table, caption.placement = "top", include.rownames = TRUE,
          latex.environments = "flushleft", comment = FALSE)
    ##
    parmest.table = modsel.table[,c("model","p(Int)","SEp(Int)","p(viscovpoor)","SEp(viscovpoor)",
                                    "psi(habcov)","SEpsi(habcov)","psi(Int)","SEpsi(Int)")]
    indz = sapply(parmest.table, class)=="numeric"
    parmest.table[,indz] = round(parmest.table[,indz], 2)
    summary.table = xtable::xtable(parmest.table, caption = paste0("Model parameter estimate table for ", year, "."),
                           label = paste0("parmest.table-", year))
    xtable::print.xtable(summary.table, caption.placement = "top", include.rownames = TRUE,
          latex.environments = "flushleft", comment = FALSE)
    cat("\\clearpage\n")

}


#### ------------------------------------------------------------------


#' Calculate Estimated Proportion of Sites At Which Species Occurs
#'
#' @param topmod object of class \code{unmarkedFitOccu} for single-species occupancy model
#' @param stat statistic (\code{mean} or \code{median}) used to summarize posterior distribution
#' @param n number of sites surveyed
#'
#' @return vector containing posterior mean or median and upper and lower 95% confidence limits
#' @export

calc.pao <- function(topmod, stat = "mode", n){
    re = unmarked::ranef(topmod)
    EBUP = unmarked::bup(re, stat = stat)
    CI = confint(re, level = 0.95)
    PAO = c(Estimate = sum(EBUP), colSums(CI))
    PAO / n
}


#### ------------------------------------------------------------------


#' Extract Estimates of Model Beta Parameters
#'
#' @param mod object of class \code{unmarkedFitOccu} for single-species occupancy model
#'
#' @return list of length 2 containing estimates of model parameters for
#' (1) occurrence probability and (2) detection probability
#'
#' @export

get.betas <- function(mod){
    x = slot(slot(mod, "estimates"), "estimates")
    psibetas = slot(x[[which(names(x)=="state")]], "estimates")
    psibetas = cbind(Estimate = psibetas, confint(mod, type = "state"))
    detbetas = slot(x[[which(names(x)=="det")]], "estimates")
    detbetas = cbind(Estimate = detbetas, confint(mod, type = "det"))
    betas = list(psi = psibetas, p = detbetas)
    class(betas) = c("betasList", "list")
    betas
}


#### ------------------------------------------------------------------


#' Extract \code{unmarkedFitOccu} Object for Top Ranked Model
#'
#' @param x1 year-specific \code{unmarkedFitList} object containing individual results for each model in a priori model set
#' @param x2 year-specific vector containing model rankings
#'
#' @return object of class \code{unmarkedFitOccu}
#' @export

get.topmod <- function(x1, x2){slot(x1, "fits")[[which(x2==1)]]}
