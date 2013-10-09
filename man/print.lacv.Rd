\name{print.lacv}
\alias{print.lacv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print lacv class object
}
\description{Prints information about lacv class object.
}
\usage{
\method{print}{lacv}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The lacv class object you want to print
}
  \item{\dots}{Other arguments
}
}
\value{None
}

\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{
Guy Nason
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{lacv}}, \code{\link{plot.lacv}}, \code{\link{summary.lacv}}
}
\examples{
#
# Make some dummy data, e.g. white noise
#
v <- rnorm(256)
#
# Compute the localized autocovariance (ok, the input is stationary
# but this is just an example. More interesting things could be achieved
# by putting the results of simulating from a LSW process, or piecewise
# stationary by concatenating different stationary realizations, etc.
#
vlacv <- lacv(v, lag.max=30)
#
# Now let's print the lacv object 
#
print(vlacv)
#Class 'lacv' : Localized Autocovariance/correlation Object:
#       ~~~~  : List with 3 components with names
#             lacv lacr date 
#
#
#summary(.):
#----------
#Name of originating time series:  
#Date produced:  Thu Oct 25 12:11:29 2012 
#Number of times:  256 
#Number of lags:  30 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
