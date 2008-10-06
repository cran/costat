`lacv` <-
function(x, filter.number=1, family="DaubExPhase", ...)
{
xews <- ewspec(x, filter.number=filter.number, family=family,  ...)$S
J <- nlevels(xews)
xewsm <- matrix(xews$D, nrow=length(x), ncol=J)
#
# First col of xewsm is finest
#


Psi <- PsiJmat(-J, filter.number=filter.number, family=family)

nc <- ncol(Psi)
L <- (nc-1)/2
dimnames(Psi) <- list(NULL, c(-L:0, 1:L))

lacv <- xewsm %*% Psi

lacv
}

