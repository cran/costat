`ewcrossspec` <-
function (x, y, filter.number = 10, family = "DaubLeAsymm",  
    WPsmooth = TRUE, verbose = FALSE, smooth.filter.number = 10, 
    smooth.family = "DaubLeAsymm", smooth.levels = 3:(nlevels(WPwst) - 
        1), smooth.dev = madmad, smooth.policy = "LSuniversal", 
    smooth.value = 0, smooth.by.level = FALSE, smooth.type = "soft", 
    smooth.verbose = FALSE, smooth.cvtol = 0.01, smooth.cvnorm = l2norm, 
    smooth.transform = I, smooth.inverse = I) 
{
    coarser <- 0
    if (verbose) 
        cat("Smoothing then inversion\n")
    if (verbose) 
      cat("Computing nondecimated wavelet transform of x\n")
#
# Compute NDWT of x and y
#
      xwdS <- wd(x, filter.number = filter.number, family = family, 
            type = "station")
    if (verbose) 
      cat("Computing nondecimated wavelet transform of y\n")
      ywdS <- wd(y, filter.number = filter.number, family = family, 
            type = "station")
#
# Compute cross raw WP
#
	if (verbose) 
            cat("Computing raw wavelet periodogram\n")
        xwdWP <- CrossWP(xwdS, ywdS)
    J <- nlevels(xwdWP)
    if (verbose) 
        cat("Computing A matrix\n")
    rm <- ipndacw(-J, filter.number = filter.number, family = family)
    if (verbose) 
        cat("Computing inverse of A\n")
    irm <- solve(rm)
    if (verbose) 
        cat("Putting wavelet periodogram into a matrix\n")
    WavPer <- matrix(0, nrow = (J - coarser), ncol = 2^J)
    for (j in 1:(J - coarser)) {
        WavPer[j, ] <- accessD(xwdWP, lev = J - j)
    }
    if (WPsmooth == TRUE) {
        if (verbose) {
            cat("Smoothing the wavelet periodogram\n")
            cat("Smoothing level: ")
        }
        for (j in 1:(J - coarser)) {
            if (verbose) 
                cat(J - j)
            WP <- WavPer[j, ]
            WP <- smooth.transform(WP)
            WPwst <- wst(WP, filter.number = smooth.filter.number, 
                family = smooth.family)
            if (verbose == TRUE) 
                cat(".w")
            WPwstT <- threshold.wst(WPwst, levels = smooth.levels, 
                dev = smooth.dev, policy = smooth.policy, value = smooth.value, 
                by.level = smooth.by.level, type = smooth.type, 
                verbose = smooth.verbose, cvtol = smooth.cvtol, 
                cvnorm = smooth.cvnorm)
            if (verbose == TRUE) 
                cat(".t")
            WPwsrR <- AvBasis(WPwstT)
            if (verbose == TRUE) 
                cat(".i")
            WavPer[j, ] <- smooth.inverse(WPwsrR)
        }
        if (verbose == TRUE) 
            cat("\n")
    }
    irm <- irm[1:(J - coarser), 1:(J - coarser)]
    S <- irm %*% WavPer
    xwdS <- xwdWP
    for (j in 1:(J - coarser)) {
        xwdS <- putD(xwdS, lev = J - j, v = S[j, ])
    }
    if (coarser > 0) 
        for (j in (J - coarser + 1):J) xwdS <- putD(xwdS, lev = J - 
            j, v = rep(0, 2^J))
    list(S = xwdS, WavPer = xwdWP, rm = rm, irm = irm)
}

