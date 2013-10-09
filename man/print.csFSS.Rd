\name{print.csFSS}
\alias{print.csFSS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print a\code{csFSS} object.
}
\description{
Print information about a \code{csFSS} object.
}
\usage{
\method{print}{csFSS}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The \code{csFSS} object you want printed.
}
  \item{\dots}{Other arguments.
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

\seealso{\code{\link{findstysols}}, \code{\link{plot.csFSS}},
	\code{\link{summary.csFSS}}
}
\examples{
#
# Create dummy data
#
x1 <- rnorm(32)
y1 <- rnorm(32)
#
# Find stationary combinations. Note: normally Nsims would be much bigger
#
\dontrun{ans <- findstysols(Nsims=100, tsx=x1, tsy=y1)}
#
# Print this csFSS object
#
\dontrun{print(ans)}
#Class 'csFSS' : Stationary Solutions Object from costat:
#       ~~~~~  : List with 13 components with names
#              startpar endpar convergence minvar pvals tsx tsy tsxname tsyname filter.number
#              family spec.filter.number spec.family 
#
#
#summary(.):
#----------
#Name of X time series:  x1 
#Name of Y time series:  y1 
#Length of input series:  32 
#There are  100  sets of solutions
#Each solution vector is based on  3  coefficients
#Some solutions did not converge, check convergence component for more information.
#Zero indicates successful convergence, other values mean different things and
#you should consult the help page for `optim' to discover what they mean
#For size level:  0.05 
#        0  solutions appear NOT to be stationary
#        97  solutions appear to be stationary
#Range of p-values: ( 0.885 , 0.975 )
#
#Wavelet filter for combinations:  1   DaubExPhase 
#Wavelet filter for spectrum:  1   DaubExPhase 
}
\keyword{ts}
