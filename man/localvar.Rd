\name{localvar}
\alias{localvar}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the time-localized (unconditional) variance for a time series}
\description{Compute the time localized variance from an evolutionary
	wavelet spectrum of a time series}
\usage{
localvar(spec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{An evolutionary wavelet spectrum, such as that computed
	by \code{ewspec} in WaveThresh.}
}
\details{One can compute the local variance of a time series by first
	computing its evolutionary wavelet spectrum, e.g., by using
	\code{ewspec}, and then applying \code{localvar} on the
	\code{S} component of that returned by \code{ewspec}.
}
\value{A vector representing the local variance estimate at successive
	times.
}

\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{ewspec}}
\examples{
#
# Let's look at a iid standard normal sequence, variance should be 1, always
# for all times.
#
zsim <- rnorm(64)
#
# Note, in the following I use var as the method of deviance estimation,
# as described in the help there it can be more accurate when transformations
# are not used.
#
z.ews <- ewspec(zsim, smooth.dev=var)$S
#
# Compute the local variance
#
z.lv <- localvar(z.ews)
#
# Plot the local variance against time
#
\dontrun{ts.plot(z.lv)}
#
# Should be around 1. Note, the vertical scale of the plot might be
# deceptive, as R plots expand the function to the maximum available
# space. If you look again it should be quite close to 1 (e.g. on the
# example I am looking at now the variance is within +/- 0.15 of 1.
#
# However, it might not be close to 1 because the sample size is quite small,
# only 64, so repeat the above analysis with a larger sample size, e.g. 1024.
#
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
