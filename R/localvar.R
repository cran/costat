`localvar` <-
function(spec){
#
# Calculate Time Varying local variance from corrected periodogram 
#

J <- nlevels(spec)
n <- 2^J

m <- matrix(spec$D, nrow=J, ncol=n, byrow=TRUE)
apply(m,2,sum)



}

