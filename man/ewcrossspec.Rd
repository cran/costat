\name{ewcrossspec}
\alias{ewcrossspec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute evolutionary wavelet cross spectrum from two series.}
\description{Compute evolutionary wavelet cross spectrum from two
	series. Like the regular evolutionary wavelet spectrum, but
	indicates the joint simultaneous power in both series at a given
	time and scale. This is the quantity
	\deqn{L_j^{XY}(z)} at the end of section 2 of Cardinali and
	Nason (2008.)
}
\usage{
ewcrossspec(x, y, filter.number = 10, family = "DaubLeAsymm", WPsmooth = TRUE, verbose = FALSE, smooth.filter.number = 10, smooth.family = "DaubLeAsymm", smooth.levels = 3:(nlevels(WPwst) - 1), smooth.dev = madmad, smooth.policy = "LSuniversal", smooth.value = 0, smooth.by.level = FALSE, smooth.type = "soft", smooth.verbose = FALSE, smooth.cvtol = 0.01, smooth.cvnorm = l2norm, smooth.transform = I, smooth.inverse = I)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{One of the time series (must be of dyadic length).}
  \item{y}{The other time series, or same length as \code{x}}
  \item{filter.number}{The wavelet index number underlying the analysis
	spectrum.}
  \item{family}{The wavelet family underlying the analysis spectrum.}
  \item{WPsmooth}{If TRUE then the corrected cross wavelet periodogram
	is smoothed before correction.}
  \item{verbose}{If TRUE then helpful messages are printed as the
	algorithm proceeds.}
  \item{smooth.filter.number}{The wavelet index number for the spectral smoothing.}
  \item{smooth.family}{The wavelet family for the spectral smoothing.}
  \item{smooth.levels}{The levels in the wavelet transform that are to be
	thresholded for the spectral smoothing.}
  \item{smooth.dev}{The deviance measure used in the spectral smoothing.
	Note that, the default argument of \code{madmad} for spectral
	smoothing is not very good. It is often better to use
	\code{var} (although this depends on the transform. If you are
	using the \code{smooth.transform=log}, which pulls the spectral
	coefficients more towards normality, then \code{madmad} might be
	ok/better).
	See help for \code{ewspec} or \code{threshold.wd}}
  \item{smooth.policy}{The type of spectral smoothing}
  \item{smooth.value}{If manual smoothing is chosen, then this variable	
	will contain the threshold.}
  \item{smooth.by.level}{In spectral smoothing, whether the thresholding
	is carried out differently on different levels or one global
	thresholding procedure is carried out}
  \item{smooth.type}{The type of thresholding in the spectral smoothing,
	i.e. hard or soft}
  \item{smooth.verbose}{If TRUE then informative messages during the
	spectral smoothing get printed out}
  \item{smooth.cvtol}{A tolerance associated with cross validation
	for the cross-valiation policy of spectral smoothing}
  \item{smooth.cvnorm}{The norm for goodness in cross validation in
	the spectral smoothing, if that policy is being used}
  \item{smooth.transform}{The transform function to use to transform the
          wavelet periodogram estimate. The wavelet periodogram
          coefficients are typically chi-squared in nature, a 'log'
          transform can pull the coefficients towards normality so that
          a 'smooth.policy' for Gaussian data could be used (e.g.
          'universal').}
  \item{smooth.inverse}{
 the inverse transform of \code{smooth.transform}}	
}
\details{This function computes an estimate of the evolutionary cross
	wavelet spectrum of two time series according to the paper
	by Cardinali and Nason (2008) in the following way:
	\enumerate{
		\item The nondecimated wavelet transform of each
			series is computed.

		\item The raw cross wavelet periodogram is formed by
			the \code{\link{CrossWP}} function.

		\item The raw periodogram is smoothed using (potentially)
			TI-wavelet shrinkage.

		\item The smoothed periodogram is bias-corrected using
		the inverse of the inner product matrix of the
		discrete non-decimated autocorrelation wavelets.
		See the help for \code{ewspec} where the operation
		is very similar.
		}

	Many of the arguments to do with the spectral smoothing are
	actually passed straight through to the wavelet smoothing and
	hence the arguments (without the prefix smooth.) are described
	in the help page for \code{threshold.wd} in the WaveThresh
	package}
\value{A list with the following components:
  \item{S }{The corrected smoothed raw cross wavelet periodogram. This
		is the estimate of the cross wavelet spectrum.}
  \item{WavPer}{The raw cross wavelet periodogram}
  \item{rm}{This is the matrix A from the paper by Nason, von Sachs and
          Kroisandt. Its inverse is used to correct the raw wavelet
          periodogram. This matrix is computed using the \code{ipndacw}
          function. }
  \item{irm}{The inverse of the matrix A from the paper by Nason, von
          Sachs and Kroisandt. It is used to correct the raw wavelet
          periodogram.}
}
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason, 2008, University of Bristol Technical Report 08:08.}
\author{Guy Nason}
\seealso{\code{ewspec}, \code{\link{CrossWP}}}
\examples{
#
# Compute the cross spectrum of x2 and y2
#
\dontrun{x2y2.crossspec <- ewcrossspec(x2, y2)}
#
# Plot the spectral estimate
#
\dontrun{plot(x2y2.crossspec$S)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
