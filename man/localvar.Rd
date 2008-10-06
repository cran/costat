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

	Can also be used on the output of \code{\link{ewcrossspec}}.
}
\value{A vector representing the local variance estimate at successive
	times.
}
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason, 2008, University of Bristol Technical Report 08:08.}
\author{Guy Nason}
\seealso{\code{ewspec}}
\examples{
#
# Let's look at a iid standard normal sequence, variance should be 1, always
# for all times.
#
z <- rnorm(1024)
#
# Note, in the following I use var as the method of deviance estimation,
# as described in the help there it can be more accurate when transformations
# are not used.
#
\dontrun{z.ews <- ewspec(z, smooth.dev=var)}
#
# Compute the local variance
#
\dontrun{z.lv <- localvar(z.ews)}
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
