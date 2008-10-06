`COEFscale` <-
function (l, plotclustonly=FALSE, plotalphaonly=FALSE) 
{


endpar <- l$endpar

ix <- 1:nrow(endpar)

endpar <- endpar[l$convergence==0,]
pvals <- l$pvals[l$convergence==0]
ix <- ix[l$convergence==0]
ix <- ix[pvals > 0.05]
endpar <- endpar[pvals > 0.05,]
dimnames(endpar) <- list(as.character(ix), NULL)

lv <- ncol(endpar)/2

epalpha <- endpar[, 1:lv]
epbeta <- endpar[,(lv+1):(2*lv)]

# Normalize rows
norm <- function(x) sqrt(sum(x^2))
swst <- apply(epalpha, 1, norm)
epalpha <- sweep(epalpha, 1, swst, FUN="/")

swst <- apply(epbeta, 1, norm)
epbeta <- sweep(epbeta, 1, swst, FUN="/")

ad <- dist(epalpha)
bd <- dist(epbeta)

ascale <- cmdscale(ad)
bscale <- cmdscale(bd)

aclust <- hclust(ad)
bclust <- hclust(bd)

apoints <- ascale
bpoints <- bscale

if (plotclustonly==FALSE)	{
	plot(apoints[,1], apoints[,2])
	plot(bpoints[,1], bpoints[,2])
	}

plot(aclust)

if (plotalphaonly == FALSE)
	plot(bclust)

}

