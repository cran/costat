`COEFbothscale` <-
function (l, plotclustonly=FALSE,...)
{
#
# Like COEFscale but amalgamates a and b coefficients
#

endpar <- l$endpar

ix <- 1:nrow(endpar)

endpar <- endpar[l$convergence==0,]

firstofeachrow <- endpar[, 1]
pon <- firstofeachrow < 0
signmult <- (-1)^pon
endpar2 <- endpar*signmult

#return(list(endpar=endpar, endpar2=endpar2))

endpar <- endpar2


pvals <- l$pvals[l$convergence==0]
ix <- ix[l$convergence==0]
ix <- ix[pvals > 0.05]
endpar <- endpar[pvals > 0.05,]
dimnames(endpar) <- list(as.character(ix), NULL)

lv <- ncol(endpar)/2


# Normalize rows
norm <- function(x) sqrt(sum(x^2))
swst <- apply(endpar, 1, norm)
endpar <- sweep(endpar, 1, swst, FUN="/")

epd <- dist(endpar)

epscale <- cmdscale(epd)

epclust <- hclust(epd)

if (plotclustonly==FALSE)	{
	plot(epscale[,1], epscale[,2])
	scan()
	}

plot(epclust, ...)

l <- list(epscale=epscale, epclust=epclust)
l

}

