\name{findstysols}
\alias{findstysols}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Given two time series find some time-varying linear combinations
	that are stationary.
}
\description{Find some time-varying linear combinations of two
	time series that are stationary. The complexity of the
	time-varying combinations is restricted by the \code{Ncoefs}
	argument.
}
\usage{
findstysols(Nsims = 100, Ncoefs = 3, tsx, tsy, sf=100, plot.it = FALSE,
        print.it=FALSE, verbose = FALSE, lctsfn=LCTS, prodcomb.fn=prodcomb,
        filter.number=1, family=c("DaubExPhase", "DaubLeAsymm"),
	my.maxit=500, spec.filter.number=1,
	spec.family=c("DaubExPhase","DaubLeAsymm"),
        optim.control=list(maxit=my.maxit, reltol=1e-6),
        irng=rnorm, lapplyfn=lapply, Bsims=200, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Nsims}{Number of searches attempted} 
  \item{Ncoefs}{Number of Haar wavelet coefficients to use. Must be
	>= 1. Should only increase in steps of powers of two. E.g.
	can only supply the values 1, 3, 7, 15, etc. So, "1" means
	only one coarse scale coefficient (corresponds to piecewise constant
	with one centrally located jump), "3" means one coarse, and two
	next coarse scale coefficients (corresponds to piecewise constant
	with 4 equally sized piece with jumps at 1/4, 1/2 and 3/4), "7"
	means one coarse, two next coarse, four next coarse, and so on. }
  \item{tsx}{One of the time series}
  \item{tsy}{The other time series,  values at the same time locations
	as \code{tsx}}
  \item{sf}{A scale factor to multiply both time series by (not really
	of much use)}
  \item{plot.it}{If \code{TRUE} then the \code{plot.it} argument passed
	to \code{\link{LCTS}} via \code{optim} is made \code{TRUE}.
	This has the effect of plotting the results of every trial in
	the optimation (what actually is plotted is described in the
	help to \code{\link{LCTS}}}
  \item{print.it}{Not currently used in this function, reserved for
	future use}
  \item{verbose}{If \code{TRUE} then helpful messages get printed.}
  \item{lctsfn}{The function to compute the `linear combination
	test of stationarity'. I.e. it is the function that
	combines the two series and returns the value of the test
	statistic on the combination.}
  \item{prodcomb.fn}{The function that can produce the linear combination
	of the two time series and return the combination, and optionally
	vectors containing the combination functions.}
  \item{filter.number}{Gets passed to \code{lctsfn} and \code{prodcomb.fn}}
  \item{family}{Gets passed to \code{lctsfn} and \code{prodcomb.fn}}
  \item{my.maxit}{Maximum number of iterations in the optimization.
	May need to be increased to, e.g. 1000 or 2000 for longer
	time series (e.g. T=2048)}
  \item{spec.filter.number}{Wavelet filter number. This argument
	gets passed to the \code{lctsfn} and is used for the wavelet
	for all spectral smoothing.}
  \item{spec.family}{Same as \code{spec.filter.number} but for the wavelet
	family.}
  \item{optim.control}{Argument passed to the \code{optim} optimizer as
	its \code{control} argument. \code{optim} 
	performs optimization. See help page for \code{optim}.}
  \item{irng}{Random number generator used to generate coefficients
	for starting parameters for the linear combination of time
	series (actually wavelet coefficients of the combination
	functions)}
  \item{lapplyfn}{Function to use to process lists. If this argument
	is \code{mclapply} then the multicore library function 
	\code{mclapply} is used to parallel process the lists.
	If you don't have multicore then the \code{lapply} function
	can be used to process things sequentially.}
  \item{Bsims}{The number of bootstrap simulations for the (single)
	test of stationarity \code{\link{BootTOS}}.}

  \item{\dots}{Other arguments, passed to the \code{optim} call.}
}
\details{The function searches for time-varying linear combinations of
	two time series, \code{tsa} and \code{tsy}, such that the combination
	is stationary (according to the \code{\link{TOSts}} test statistic).

	Each linear combination is parametrised by a coarse scale
	Haar wavelet decomposition (controlled by \code{Ncoefs}).
	Initially, the Haar wavelet coefficients (up to a fixed finite scale,
	controlled by \code{Ncoefs}) are randomly chosen. These coefficients
	are converted to functions \eqn{\alpha_t, \beta_t}{\alpha_t, \beta_t}
	by the \code{\link{coeftofn}} function and then a linear combination
	with the time series is formed out of those and the time series,
	i.e.
	\eqn{Z_t = \alpha_t x_t + \beta_t y_t}{Z_t = \alpha_t x_t + \beta_t y_t}
	The non-stationarity of \eqn{Z_t}{Z_t} is measured using the
	\code{\link{TOSts}} test statistic and this value is minimized
	over the coarse scale Haar wavelet coefficients.

	This optimization procedure is repeated \code{Nsims} times.
	If the \code{lapplyfn} is set to \code{mclapply} then this
	function from the \code{multicore} package is used to process
	the lists in parallel. 

	This function can be called multiple times (e.g. on different
	processors in a multiprocessor environment. The result sets from
	different runs can be combined using the
	\code{\link{mergexy}} function.

	The variance \code{Ncoefs} is very important, it controls the
	complexity of the linear combinations. If it is too big the
	linear combinations themselves can be extremely oscillatory and
	stationarity is easy to obtain. Small values of \code{Ncoefs}
	results in piecewise constant functions with fewer jumps.

	The \code{Ncoefs} value must take the value of
	\eqn{2^k - 1}{2^k-1}. If this is the case the \eqn{k}{k}
	is the number of scale levels present in the Haar representation
	of the combining function \eqn{\alpha_t, \beta_t}{\alpha_t, \beta_t}
	(excluding the scaling function coefficient, just the wavelet
	coefficients from the coarsest scale).

	The functions to compute the linear combination and also the
	test statistic on that combination, and just to compute the
	combination and return also (optionally) the combination
	vectors are supplied in \code{lctsfn} and \code{prodcomb.fn}.
	By default, these are just the \code{LCTS} and \code{prodcomb}
	functions. However, it is possible to recode these to
	look at operating on combinations that operate on portfolios.
	I.e. rather than look at linear combinations of log-returns
	(which if \code{tsx} and \code{tsy} were) one can look at
	linear combinations of actual series (ie portfolios) and
	then look for stationarity of log-returns of the portfolios.
	These functions will be made available in a later package.
}
\value{An object of class \code{csFSS} which is a
	list with the following components.
  \item{startpar}{A matrix with \code{Nsims} rows and \code{2*Ncoefs}
	columns containing the initial random coefficients of the
	linear combination functions, one row for each optimization
	run. The first \code{Ncoefs} numbers on each row correspond to
	the \eqn{\alpha_t}{\alpha_t} coefficients, the second \code{Ncoefs}
	numbers correspond to the \eqn{\beta_t}{\beta_t} coefficients.}
  \item{endpar}{Same dimension as \code{startpar} except containing
	the final coefficients obtained after running the optimizer.
	If, for a particuar run, the optimizer converged and the p-value
	is less than 0.05 then one can say that this solution represents
	a valid time-varying linear combination where the combination
	is stationary (coefficient storage format as for \code{startpar}).}
  \item{convergence}{A vector of length \code{Nsims}. Reports the convergence
	code from \code{optim} for each optimization run. A value of
	0 indicates successful convergence.}
  \item{minvar}{A vector of length \code{Nsims}. Contains the minimum
	variance achieved on each run.}
  \item{pvals}{A vector of length \code{Nsims}. Contains the p-values
	achieved on each run.}
  \item{tsx}{The \code{tsx} time series that was supplied to this function}
  \item{tsy}{The \code{tsy} time series that was supplied to this function}
  \item{tsxname}{The name of the \code{tsx} object that was supplied}
  \item{tsyname}{The name of the \code{tsy} object that was supplied}
  \item{filter.number}{The filter number that was used}
  \item{family}{The wavelet family that was used}
  \item{spec.filter.number}{The filter number that was used}
  \item{spec.family}{The wavelet family that was used}
}

\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{\link{LCTS}}, \code{\link{BootTOS}},\code{\link{plotBS}}, \code{\link{prodcomb}}, \code{\link{COEFbothscale}},
\code{\link{LCTSres}}, \code{\link{print.csFSS}}, \code{\link{summary.csFSS}},
\code{\link{plot.csFSS}}}
\examples{
#
# Find some stationary solutions with \code{Ncoefs=3}.
#
# Note: this is a toy example
#
tsx1 <- rnorm(32)	# A x time series
tsy1 <- rnorm(32)	# A y time series
#
# Find costationary solutions, but only from 2 random starts
#
# Typically, the length of tsx and tsy would be bigger (eg sret, fret are
# other examples you might use). Also, Nsims would be bigger, you need
# to use many random starts to ensure good coverage of the solution
# space, e.g. Nsims=100
#
# Note: the following examples are not run so as to adhere to CRAN
# requirements for package execution timings
#
\dontrun{ans <- findstysols(Nsims=3, tsx=tsx1, tsy=tsy1)}
#
# Print out a summary of the results
#
\dontrun{ans}
#Class 'csFSS' : Stationary Solutions Object from costat:
#       ~~~~~  : List with 13 components with names
#              startpar endpar convergence minvar pvals tsx tsy tsxname tsyname
#		filter.number family spec.filter.number spec.family 
#
#
#summary(.):
#----------
#Name of X time series:  tsx1 
#Name of Y time series:  tsy1 
#Length of input series:  32 
#There are  3  sets of solutions
#Each solution vector is based on  3  coefficients
#Some solutions did not converge, check convergence component for more information.
#Zero indicates successful convergence, other values mean different things and
#you should consult the help page for `optim' to discover what they mean
#For size level:  0.05 
#	 0  solutions appear NOT to be stationary
#	 3  solutions appear to be stationary
#Range of p-values: ( 0.93 , 0.995 )
#
#Wavelet filter for combinations:  1   DaubExPhase 
#Wavelet filter for spectrum:  1   DaubExPhase 
#
#______________
#
# Ok. The printout above suggests that some solutions did not converge.
# Which ones?
#
\dontrun{ans$convergence}
# [1] 0 1 0 
#
# The second one did not converge, the others did. Good. The printout
# above also indicates that all the resultant solutions were stationary
# (this is not surprising for this example, as the inputs tsx1 and tsy1
# are stationary, and indeed iid).
#
# Let's see how the solutions compare. For example, let's plot the
# hierarchical cluster analysis of the final solutions (those that
# converged and are stationary)
#
\dontrun{plot(ans, ALLplotscale=FALSE)}
#
# My cluster shows that solution 1 and 3 are similar. Let's
# view solution 3.
#
\dontrun{oldpar <- par(mfrow=c(2,2))}
\dontrun{plot(ans, solno=3)}
\dontrun{par(oldpar)}
}
\keyword{ts}
