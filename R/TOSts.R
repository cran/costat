`TOSts` <-
function(spec){
#
# Calculate Test Statistic
#

J <- nlevels(spec)
n <- 2^J

m <- matrix(spec$D, nrow=J, ncol=n, byrow=TRUE)
sum(apply(m,1,var))



}

