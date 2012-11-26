\name{getpvals}
\alias{getpvals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Form a particular linear combination of two time series and
	assess the combination's stationarity p-value
}
\description{Given two time series, a set of combination coefficients,
	a function to combine them, this function makes the combination,
	tests the combination for stationarity, and returns the pvalue.
	Effectively, returns "how stationary" the combination is.
}
\usage{
getpvals(par, prodcomb.fn, tsx, tsy, filter.number,
	family=c("DaubExPhase", "DaubLeAsymm"),
	verbose, tos = BootTOS, Bsims = 100, lapplyfn = "mclapply")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{The coefficients used to make the combination via the
	\code{prodcomb.fn} function.
}
  \item{prodcomb.fn}{The function which computes the combination given
	the two time series and the combination parameters.
}
  \item{tsx}{One of the time series.
}
  \item{tsy}{The other time series.
}
  \item{filter.number}{Wavelet smoothness to be used in the time series
	combination.
}
  \item{family}{Wavelet family to be used in the time series combination.
}
  \item{verbose}{Supplied directly to the call to \code{plotBS} function.
}
  \item{tos}{The function the computes a test of stationarity
}
  \item{Bsims}{Number of bootstrap simulations the test uses (if it does)
}
  \item{lapplyfn}{The function used to process lists. Can be the regular
	\code{lapply}. If you have \code{multicore} package then can
	be the \code{mclapply} parallel processing to process the bootstraps
	in parallel.  }
}
\value{A single number between zero and one indicating the p-value from
	the hypothesis test of stationarity of the combination.
}
\references{
Cardinali, A. and Nason, G.P. (2012) Costationarity of Locally 
Stationary Time Series using \code{costat}.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{
G. P. Nason
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{findstysols}}
}
\examples{
#
# Generate two toy time series data sets
#
x1 <- rnorm(32)
y1 <- rnorm(32)
#
# Generate two toy sets of parameters (for combination)
#
tmp.a <- c(1,-1)
tmp.b <- c(0.5, 0.5)
#
# Call the function and find out the degree of stationarity of this
# combination
#
\dontrun{ans <- getpvals(c(tmp.a, tmp.b), prodcomb.fn=prodcomb, tsx=x1, tsy=y1,
        filter.number=1, family="DaubExPhase")}
#
# What is the p-value?
#
\dontrun{ans}
# [1] 0.53
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
