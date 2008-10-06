`plottstosscan` <-
function(x, inc, ttsobj, xlab="Time", ylab="x"){


r <- range(x)

xax <- inc:(inc+length(x)-1)

plot(xax, x, ylim=r, type="n", xlab=xlab, ylab=ylab)
lines(xax, x, col="gray90")

spread <- r[2] - r[1]

spans <- ttsobj$spans
nspans <- length(spans)

yx <-  r[1] + spread/nspans


for(i in 1:nspans)	{
	startvec <- ttsobj$starlist[[i]]
	pvalvec <- ttsobj$pvallist[[i]]

	yx <- r[1] + i*spread/nspans
	for(j in 1:length(startvec))	{
		if (pvalvec[j] < 0.05)
			text(inc+startvec[j]+spans[i]/2, yx, "x")
		else
			text(inc+startvec[j]+spans[i]/2, yx, "-")
		}
	}

yxvals <- r[1] + (1:nspans)*spread/nspans
axis(4, at=yxvals, lab=spans)
mtext("Span", 4)




}

