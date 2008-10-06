\name{coeftofn}
\alias{coeftofn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert wavelet coefficients for two time-varying functions
	into two functions with respect to time.}
\description{In much of the costationarity code the combination functions
	are represented in terms of wavelet coefficients. At certain
	points the actual combination functions themselves are required
	(in the time domain) for purposes such as actually forming the
	linear combination. This function turns the coefficients, for
	the two combination functions, into their time domain functional
	representation.
}
\usage{
coeftofn(alpha, beta, n = 256, filter.number = 1, family = "DaubExPhase")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{One set of coefficients for one of the combination functions}
  \item{beta}{The other set of coefficients}
  \item{n}{The length of resulting function that you require}
  \item{filter.number}{The type of wavelet (the number of vanishing moments)}
  \item{family}{The type of wavelet (the wavelet family)}
}
\details{A degree of efficiency is built into the code. Typically, for
	forming stationary linear combinations then only a few (or at least
	a medium number) of coarser scale coefficients need to be
	manipulated (eg modified in the optimizer). However, the
	actual length of the function (time series length) is typically
	much longer (e.g. n=256, n=512, or higher). So, this function
	pads out the small number of coarse coefficients with zeros
	before forming the combination functions which end up at the
	correct length, n.  
}
\value{A list containing two components:
  \item{alpha}{A vector, of length n, containing one of the time-varying
	combination functions}
  \item{beta}{Same as \code{alpha}, but contains the other combination
	function.}
}
\references{`Costationarity and stationarity tests for stock index returns' by Cardinali and Nason.}
\author{Guy Nason}
\seealso{\code{\link{LCTS}}, \code{\link{LCTSres}}}
\examples{
#
# Very artifical example
#
tmp.a <- c(1, -1)
tmp.b <- c(0.5, 0.5)
#
#
#
\dontrun{ans <- coeftofn(tmp.a, tmp.b)}
#
# Should return two combination functions of length 256
#
\dontrun{length(ans$alpha)}
#[1] 256
}
\keyword{ts}
