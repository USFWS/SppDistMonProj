

#' Plot Estimates and CIs of Linear Model Coefficients for Species Occurrence and Detection Probalities
#'
#' @param betas List of length equal to number of survey years.  Each element contains a ist of length 2
#' containing estimates of model parameters for (1) occurrence probability and (2) detection probability
#' @param years Numeric vector containing survey years.
#' @param modranks Data.frame containing model rankings for all survey years.
#'
#' @export

betas.plot <- function(betas, years, modranks){
    offset = 0.1
    mods = apply(modranks, 2, function(x)rownames(modranks)[which(x==1)])
    par(mfrow = c(2,2))
    y = as.data.frame(do.call("rbind", lapply(betas, function(x)x$psi["(Intercept)",])))
    colnames(y) = c("est","lcl","ucl")
    plot(years, y$est, type = "p", pch = 16, cex = 0.5, cex.lab = 0.75, axes = FALSE, xlab = "Year",
         ylim = c(-10,10), ylab = expression(paste("Intercept for ", psi)))
    axis(1, at = years, cex.axis = 0.75)
    axis(2, cex.axis = 0.75)
    segments(years, y$est, years, y$lcl, col = gray(0.7), lwd = 0.5)
    segments(years, y$est, years, y$ucl, col = gray(0.7), lwd = 0.5)
    segments(years-offset, y$lcl, years+offset, y$lcl, col = gray(0.7), lwd = 0.5)
    segments(years-offset, y$ucl, years+offset, y$ucl, col = gray(0.7), lwd = 0.5)
    points(years, y$est, pch = 16, cex = 0.5)
    text(years, -2, labels = mods, cex = 0.4)
    ##
    indz = lapply(lapply(betas, function(x)x$psi), nrow)==2
    if(any(indz)){
        y[] = NA
        y[indz,] = do.call("rbind", lapply(betas[indz], function(x)x$psi["habcov",]))
        plot(years, y$est, type = "p", pch = 16, cex = 0.5, cex.lab = 0.75, axes = FALSE, xlab = "Year",
             ylim = c(-10,10), ylab = expression(paste("Coefficient for ", psi)))
        axis(1, at = years, cex.axis = 0.75)
        axis(2, cex.axis = 0.75)
        segments(years, y$est, years, y$lcl, col = gray(0.7), lwd = 0.5)
        segments(years, y$est, years, y$ucl, col = gray(0.7), lwd = 0.5)
        segments(years-offset, y$lcl, years+offset, y$lcl, col = gray(0.7), lwd = 0.5)
        segments(years-offset, y$ucl, years+offset, y$ucl, col = gray(0.7), lwd = 0.5)
        points(years, y$est, pch = 16, cex = 0.5)
        text(years, 0, labels = mods, cex = 0.4)
        if(!all(indz)){
            text(years[is.na(y$est)], rep(3,sum(is.na(y$est))), labels = "NA", cex = 0.75)
        }
    } else {
        plot(years, rep(3, length(years)), type = "p", pch = "", cex = 0.5, cex.lab = 0.75, axes = FALSE, xlab = "Year",
             ylim = c(-10,10),ylab = expression(paste("Coefficient for ", psi)))
        axis(1, at = years, cex.axis = 0.75)
        axis(2, cex.axis = 0.75)
        text(years, 3, labels = "NA", cex = 0.75)
    }
    ##
    y = as.data.frame(do.call("rbind", lapply(betas, function(x)x$p["(Intercept)",])))
    colnames(y) = c("est","lcl","ucl")
    plot(years, y$est, type = "p", pch = 16, cex = 0.5, cex.lab = 0.75, axes = FALSE, xlab = "Year",
         ylim = c(-10,10), ylab = expression(paste("Intercept for ", italic(p))))
    axis(1, at = years, cex.axis = 0.75)
    axis(2, cex.axis = 0.75)
    segments(years, y$est, years, y$lcl, col = gray(0.7), lwd = 0.5)
    segments(years, y$est, years, y$ucl, col = gray(0.7), lwd = 0.5)
    segments(years-offset, y$lcl, years+offset, y$lcl, col = gray(0.7), lwd = 0.5)
    segments(years-offset, y$ucl, years+offset, y$ucl, col = gray(0.7), lwd = 0.5)
    points(years, y$est, pch = 16, cex = 0.5)
    text(years, -2, labels = mods, cex = 0.4)
    ##
    indz = lapply(lapply(betas, function(x)x$p),nrow)==2
    if(any(indz)){
        y[] = NA
        y[indz,] = do.call("rbind",lapply(betas[indz], function(x)x$p["viscovpoor",]))
        plot(years, y$est, type = "p", pch = 16, cex = 0.5, cex.lab = 0.75, axes = FALSE, xlab = "Year",
             ylim = c(-10,10), ylab = expression(paste("Coefficient for ", italic(p))))
        axis(1, at = years, cex.axis = 0.75)
        axis(2, cex.axis = 0.75)
        segments(years, y$est, years, y$lcl, col = gray(0.7), lwd = 0.5)
        segments(years, y$est, years, y$ucl, col = gray(0.7), lwd = 0.5)
        segments(years-offset, y$lcl, years+offset, y$lcl, col = gray(0.7), lwd = 0.5)
        segments(years-offset, y$ucl, years+offset, y$ucl, col = gray(0.7), lwd = 0.5)
        points(years, y$est, pch = 16, cex = 0.5)
        text(years, -2, labels = mods, cex = 0.4)
        if(!all(indz)){
            text(years[is.na(y$est)], rep(0, sum(is.na(y$est))), labels = "NA", cex = 0.75)
        }
    } else {
        plot(years, rep(0,length(years)), type = "p", pch = "", cex = 0.5, cex.lab = 0.75, axes = FALSE, xlab = "Year",
             ylim = c(-10,10), ylab = expression(paste("Coefficient for ", italic(p))))
        axis(1, at = years, cex.axis = 0.75)
        axis(2, cex.axis = 0.75)
        text(years, 0, labels = "NA", cex = 0.75)
    }
}


#### ------------------------------------------------------------------


#' Plot Annual Estimated Proportions of Sites At Which Species Occurs
#'
#' @param PAO Data.frame containing posterior mean or median and upper and lower 95-percent confidence limits for all survey years.
#' @param years Numeric vector containing survey years.
#' @param modranks Data.frame containing model rankings for all survey years.
#'
#' @export

pao.plot <- function(PAO, years, modranks){
    topmods = apply(modranks, 2, function(x)rownames(modranks)[which(x==1)])
    plot(PAO$Year, PAO$Estimate, type = "p", ylim = c(0,1), cex.main = 1, cex.lab = 1, pch = 16,
         xlab = "Year", axes = FALSE,
         ylab = "Proportion of sites occupied")
    axis(1, at = years)
    axis(2)
    xs = as.numeric(PAO$Year)
    offset = 0.1
    segments(xs, PAO$Estimate, xs, PAO[,2], col = gray(0.7), lwd = 0.5)
    segments(xs, PAO$Estimate, xs, PAO[,3], col = gray(0.7), lwd = 0.5)
    segments(xs - offset, PAO[,2], xs + offset, PAO[,2], col = gray(0.7), lwd = 0.5)
    segments(xs - offset, PAO[,3], xs + offset, PAO[,3], col = gray(0.7), lwd = 0.5)
    points(xs, PAO$Estimate, pch = 16, cex = 1)
    text(xs, 0, labels = topmods, cex = 0.5)
}


#### ------------------------------------------------------------------


#' Plot Predicted Values for Probability of Species Occurrence and Detection
#'
#' @param predvals List of length 2 containing predicted values for probability of species
#' occurrence and probability of detection.
#'
#' @export
#'

predvals.plot <- function(predvals){
    par(mfrow = c(1,2))
    plot(Predicted~habcov, predvals$epsi, type = "l", lwd = 1, ylim = c(0,1), cex.main = 0.75, cex.lab = 1, cex.axis = 1,
         xlab = "Habitat covariate",
         ylab = expression(paste("Probability of occurrence (", psi, ")")))
    lines(lower ~ habcov, predvals$epsi, col = gray(0.7), lwd = 1)
    lines(upper ~ habcov, predvals$epsi, col = gray(0.7), lwd = 1)
    plot(1, type = "n", ylim = c(0,1), xlim = c(0,3), axes = FALSE, frame.plot = TRUE,
         cex.main = 0.75, cex.lab = 1, xlab = "Visibility covariate",
         ylab = expression(paste("Probability of detection (", italic(p), ")")))
    xtix = 1:2
    axis(1, at = xtix, labels = as.character(predvals$ep$viscov), cex.axis = 1)
    axis(2, cex.axis = 1)
    xs = 1:2
    offset = 0.1
    segments(xs, predvals$ep$Predicted, xs, predvals$ep$lower, col = gray(0.7), lwd = 0.5)
    segments(xs, predvals$ep$Predicted, xs, predvals$ep$upper, col = gray(0.7), lwd = 0.5)
    segments(xs - offset, predvals$ep$lower, xs + offset, predvals$ep$lower, col = gray(0.7), lwd = 0.5)
    segments(xs - offset, predvals$ep$upper, xs + offset, predvals$ep$upper, col = gray(0.7), lwd = 0.5)
    points(1:2, predvals$ep$Predicted, pch = 16, cex = 0.5)
}


#### ------------------------------------------------------------------


#' Plot and Save Images for Class Exercise Using Survey123
#'
#' @param good logical value for whether visibility condition is good (TRUE) or poor (FALSE)
#' @param z indicator variable for whether site truly is occupied (1) or unoccupied (0)
#' @param path directory path for where image is saved
#' @param fname file name for image
#' @param seed value used to set seed in simulating x-y coordinates for the image
#' @param color color code used for symbol indicating species occurrence
#' @param ngood number of symbols used under good visibility conditions
#' @param npoor number of symbols used under poor visibility conditions
#' @param cex symbol size
#' @param showcoords logical value for whether to display coordinates for species location (TRUE)
#' or site name (FALSE) as main title
#' @param site character string specifiying site name
#'
#' @return No value is returned. Files are written to user-specified folder location.
#' @export

trial.plot <- function(good, z, path = NULL, fname = NULL, seed, color = NULL,
                       ngood = 500, npoor = 1000, cex = 1, showcoords = FALSE, site = ""){
    n = ifelse(good, ngood, npoor)
    boxpch = ifelse(z==1, 0, 1)
    path = ifelse(is.null(path), getwd(), path)
    fname = ifelse(is.null(fname), paste0("/n", n), paste0("/", fname))
    boxcol = ifelse(is.null(color), "black", color)
    set.seed(seed)
    coords = data.frame(x = runif(n, 0, 1),y = runif(n, 0, 1))
    main = ifelse(showcoords, paste0("x = ", coords[n,1],", y = ", coords[n,2]), site)
    png(paste0(path, fname, ".png"), width = 6.5, height = 6.5, units = "in", res = 192)
    plot(coords$x, coords$y, xlim = c(0,1), ylim = c(0,1), main = main,
         col = c(rep("black", n-1), boxcol), cex = cex,
         pch = c(rep(1, n-1), boxpch), xlab = "x coord", ylab = "y coord")
    dev.off()
}

