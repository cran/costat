`coeftofn` <-
function(alpha,beta, n=256,filter.number=1,family="DaubExPhase" ){

lD <- n - 1
alpha <- c(rep(0,lD-length(alpha)), alpha)
beta <- c(rep(0,lD-length(beta)), beta)
bwd <- awd <- wd(rep(0,n), filter.number=filter.number, family=family)
awd$D <- alpha
bwd$D <- beta
alpha <- wr(awd)
beta<-wr(bwd)
l <- list(alpha=alpha, beta=beta)
l




}

