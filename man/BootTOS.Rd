\name{BootTOS}
\alias{BootTOS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform bootstrap stationarity test for time series}
\description{Given a time series this function runs a bootstrap hypothesis
	test to see whether it is stationary. The null hypothesis is that
	the series is stationary, the alternative is that it is not -
	and hence possesses a time-varying evolutionary wavelet spectrum
	if deemed non-stationary. 
}
\usage{
BootTOS(x, Bsims = 100, WPsmooth = TRUE, verbose = FALSE, plot.avspec = FALSE,
        plot.avsim = FALSE, theTS = TOSts, AutoReflect=TRUE, lapplyfn=mclapply)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Time series to test. Must have a power of two length}
  \item{Bsims}{Number of bootstrap simulations to carry out}
  \item{WPsmooth}{Whether or not to carry out wavelet periodogram smoothing}
  \item{verbose}{If \code{TRUE} informative messages are printed}
  \item{plot.avspec}{If \code{TRUE} then the `average' evolutionary
	wavelet spectrum (EWS) is plotted. This is called
	\eqn{\bar{S}_j}{\bar{S}_j} in the Cardinali and Nason paper.}
	
  \item{plot.avsim}{If \code{TRUE} for each bootstrap simulation
	plot the time series of the simulated time series from the
	average EWS (the one that might be plotted by \code{plot.avspec=TRUE}}
  \item{theTS}{Specifies the particular test statistic to be used}
  \item{AutoReflect}{If TRUE then the series is reflected and augmented
	by its end point on the RH-side, and the spectral quantities
	are evaluated on that. Everything returned though applies only
	to the original series, the reflection is merely to ensure that
	the periodic wavelet algorithms can be used on non-periodic data}
  \item{lapplyfn}{List processing function. Parallel processing of
	the bootstrap simulations can be achieved by using the \code{multicore}
	package and the \code{mclapply} function. Sequential processing
	can be achieved using the standard \code{lapply} function.
	So, if you can't run multicore then you should use \code{lapply},
	otherwise try and use the default \code{mclapply} for
	faster execution times.}
}
\details{The details of our testing methodology are set out in the
	Cardinali and Nason paper referenced below.

	Essentially, the testing process works as follows. First, one
	has to define a test statistic. Given a time series this has
	return a statistic that measures `degree of nonstationarity'.
	For example, estimating the EWS, and then computing the
	sum of the sample variances of each scale is such as
	measure (and known as the \eqn{T_{vS}}{T_{vS}} statistic).
	This statistic is zero for a constant spectrum and positive
	for non-constant spectrum (and generally larger for larger variations
	of the spectrum).

	Once a test statistic T is selected then a parametric Monte Carlo
	test can be used. First, T is computed on the series itself.
	Then, for statistical assessment of the `significance' of the test
	statistic the following procedure is carried out. Assuming, for
	a moment that the time series is stationary, we estimate its
	evolutionary wavelet spectrum (EWS) and then average this over
	time (\eqn{\bar{S}_j}{\bar{S}_j}). Then we use the function
	\code{LSWsim} to simulate a time series whose EWS is the
	constant, stationary, spectral estimate. Then we compute our
	test statistic, \eqn{T_b}, on this simulated series. 

	Then we calculate \eqn{T_b} for \code{Bsim-1} simulations. The function
	then returns \code{BSim} numbers. The first is the test statistic
	computed on the actual data. The remaining ones are the test
	statistic computed on the simulated stationary series.

	The idea being that if the time series is really stationary then
	the first value will be comparable to the ones obtained by simulation.
	If the time series is not stationary then the first test statistic
	will be much larger than the ones obtained by simulation (since
	the actual data T will have been computed on a time series with
	varying spectrum, whereas the simulated ones are all computed on
	constant spectra, and their variation is only due to sampling
	variation).

	The test statistic supplied to this function (as argument
	\code{theTS}) should take an EWS object as an argument.
	For example, the WaveThresh function \code{ewspec}
	produces a suitable spectral estimate in its \code{$S}
	argument (both objects are actually examples of a 
	non-decimated wavelet transform object, class \code{wd}).

	The function \code{plotBS} can be used the present the results
	of this function in an interpretable form and calculate the
	p-value of the test, although you should use the
	generic \code{plot} function to call this.
}
\value{A vector of length \code{Bsim}. The first entry is the value
	of the test statistic computed on the data. The remaining
	entries are boostrap values computed on the `averaged' EWS estimate
	with constant spectrum.
}
\references{
Cardinali, A. and Nason, G.P. (2012) Costationarity of Locally 
Stationary Time Series using \code{costat}.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{\link{TOSts}}, \code{\link{plotBS}}}
\examples{
#
# Calculate test of stationarity on example we know to be stationary,
# a series of iid values
#
plot(BootTOS(rnorm(64), Bsims=10), plot=FALSE)
#
# The following text is what gets printed
#
#Realized Bootstrap is  0.04543729 
#p-value is  0.93 
#Series was stationary
#[1] 0.93
#
# The realized bootstrap value is the value of the test statistic on the
# actual data (0.0454 here).
#
# The p-value is also printed (this is just the number of simulated series
# test statistic values less than the actual test statistic) and returned.
#
# The text "Series is stationary" just means that the empirical p-value
# was greater than the nominal test size (alpha=0.05, by default).
#
# Let's now try another example with the series sret: note that if you
# have a slow single core machine, this can take a long time, so we don't
# run it in the examples. However, on a fastish machine it is quick, on
# a fast multicore machine it is really quick!
#
\dontrun{plot(BootTOS(sret))}
#
#Realized Bootstrap is  2.662611e-09 
#p-value is  0 
#Series was NOT stationary
#[1] 0
#
# In contrast to the previous example, the p-value is 0, hence indicative
# of non-stationarity.
#
}
\keyword{ts}
