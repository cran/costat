\name{LCTSres}
\alias{LCTSres}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots solutions that are identified by findstysols}
\description{Plots lots of useful information concerning solutions
	identified using findstysols. It only plots those where the
	optimizer converged. Can additionally return the time-varying
	linear combination associated with any solution if plots are
	turned off.
}
\usage{
LCTSres(res, tsx, tsy, inc = 0, solno = 1:nrow(res$endpar), filter.number = 1,
  family = c("DaubExPhase", "DaubLeAsymm"), plot.it = FALSE,
  spec.filter.number = 1,
  spec.family = c("DaubExPhase", "DaubLeAsymm"), plotcoef = FALSE,
  sameplot = TRUE, norm = FALSE, plotstystat = FALSE,
  plotsolinfo = TRUE, onlyacfs = FALSE,
  acfdatatrans = I, xlab = "Time", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{Solution set returned by findstysols}
  \item{tsx}{The \code{x} time series}
  \item{tsy}{The \code{y} time series}
  \item{inc}{Adds an increment to the x-axis values.}
  \item{solno}{Which solution number to look at. This can be a vector of
	solution numbers. The default is to look at all solutions (which
	can be a lot, depending on how many you've got)}
  \item{filter.number}{The wavelet filter number to use in reconstructing
	the linear combination function}
  \item{family}{The wavelet family to use in reconstructing the linear
	combination function.}
  \item{plot.it}{Currently unused in this function}
  \item{spec.filter.number}{This function computes the linear combination
	time series and also then computes its EWS. 
	The wavelet (\code{spec.filter.number} is the filter number of
	this wavelet) used to compute the EWS can be different to the
	one used to compute the linear combination, as the latter is
	only a means to an end - e.g. in principle, other basis functions
	could be use in the linear combination. Also the spectrum
	computed is only used to assess its constancy, so could be
	a locally stationary Fourier one.
	}
  \item{spec.family}{The family of the wavelet used to compute the spectrum} 
  \item{plotcoef}{If TRUE then only the linear combination functions
	are plotted. If FALSE then a (set of potentially multiple)
	composite plot(s) are produced. These composite plots are what
	are usually most useful.} 
  \item{sameplot}{If TRUE then the linear combination functions are plotted
	on the same plot.}
  \item{norm}{If TRUE then the linear combination functions are normalized
	before plotting if \code{sameplot} is TRUE. This is so as to
	be able to compare the patterns in each function without regard
	to their overall size.}
  \item{plotstystat}{If TRUE (and if \code{plotcoef=FALSE}) this option
	causes the function to plot statistics associated with the
	stationary solution, \eqn{Z_t}. The acf and partial acf are always
	plotted. The time series plot of \eqn{Z_t} and its spectrum are
	optionally plotted too if \code{onlyacfs=FALSE}.}
  \item{plotsolinfo}{If TRUE (and if \code{plotsolinfo=FALSE}) this
	option plots the \eqn{\alpha_t} linear combination function,
	the \eqn{\beta_t} one (ie both of them),
        the stationary linear combination
	\eqn{Z_t},
	and an estimate of the EWS of \eqn{Z_t} computed using the
	\code{spec.filter.number} and \code{spec.family} wavelet.
	The variance associated with \eqn{Z_t} (the minimizing variance from
	the optimizer in \code{\link{findstysols}} and the p-value	
	associated with the solution are displayed as plot titles.}
  \item{onlyacfs}{Only plot the two acfs if \code{plotstystat=TRUE}}
  \item{acfdatatrans}{A function (e.g. \code{log}) to transform the series
	before taking and displaying the acf functions.}
  \item{xlab}{An x label for the time series plots, and spectral plots}
  \item{\dots}{Extra arguments for the acf plots.}
}
\details{The function \code{\link{findstysols}} takes two time series
	and attempts to find time-varying linear combinations of the
	two that are stationary. If one is found, we call it \eqn{Z_t}.
	However, \code{\link{findstysols}} works by numerical optimization,
	typically from random starts, and, generally, there is no unique
	stationary solution. 

	This function takes the results obtained by \code{\link{findstysols}}
	in an object called \code{res} and then for a set of solutions
	already identifed by the user, and supplied to this function
	via \code{solno}, this function takes each identified solution
	in turn and produces a set of plots.

	Determining which solutions are interesting is another problem.
	The \code{\link{COEFbothscale}} is a useful function which
	can analyze all solution sets simultaneously and, usually, arrange
	them into groups which are mutually similar. Then representative
	members from each group can be further analyzed by
	\code{LCTSres}.

	Probably the most useful set of options is
	\code{plotcoef=FALSE} and to issue a
	\code{par(mfrow=c(2,2))} command prior to running
	\code{LCTSres}. This produces the plots, four to a page,
	and enables interesting features to be compared from plot to plot.

	The \code{plotcoef=FALSE} option causes four plots to be produced
	(on the same page if \code{mfrow} is set as the previous paragraph
	suggests). The first two are the (potentially) time-varying linear 
	combination functions, the next is the stationary linear
	combination, \eqn{Z_t}, itself and the final plot is an estimate of
	the \eqn{Z_t}'s evolutionary wavelet spectrum. The titles of the latter
	two plots display the process variance of \eqn{Z_t} (the global
	unconditional variance, because \eqn{Z_t} is assumed to be stationary)
	and the p-value associated the the hypothesis test of stationarity
	of \eqn{Z_t}. The spectral estimate show exhibit near constancy because
	of the stationarity (as assessed by hypothesis test) of \eqn{Z_t}.

	If \code{plotstystat=TRUE} then further plots are produced 
	of the results of various classical time series analyses of \eqn{Z_t}.
	If \code{onlyacfs=TRUE} then only the acf and partial acf of \eqn{Z_t}
	are plotted, otherwise \eqn{Z_t} and its classical spectrum are also
	plotted (remember, \eqn{Z_t}, has tested to be stationary and so these
	classical methods are valid).

	If more than one solution is to be plotted, then the \code{scan()}
	function is employed to pause the plots between plots.
}
\value{The stationary solution, \eqn{Z_t}, associated with the last solution
	to be plotted is returned. Of course, if there is only one
	solution to be plotted then it is the only possibility. Hence,
	if all the \code{plot} arguments are FALSE then no plots are 
	produced and the stationary linear combination of the (last)
	solution number is returned.
}
\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{\link{findstysols}}}
\examples{
#
# See examples in findstysols (the plot method for the results of
# findstysols make use of LCTSres)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
