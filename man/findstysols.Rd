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
	filter.number=1, family="DaubExPhase", my.maxit=500)
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
\value{A list with the following components.
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
}
\references{`Costationarity and stationarity tests for stock index returns'
	by Cardinali and Nason.
}
\author{Guy Nason}
\seealso{\code{\link{LCTS}}, \code{\link{myTOS}},\code{\link{plotBS}}, \code{\link{prodcomb}}, \code{\link{COEFscale}}, \code{\link{COEFbothscale}},
\code{\link{LCTSres}}}
\examples{

#
# Find some stationary solutions with \code{Ncoefs=3}.
#
# (not run in R package installation as its compute intensive) 
#
\dontrun{tmp <- findstysols(Nsims=10, tsx=x2, tsy=y2)}
#
# Here are my results from this call
#
\dontrun{tmp}
#
#$startpar
#            [,1]        [,2]        [,3]       [,4]       [,5]       [,6]
#[1,] -2.2547452 -0.77196922 -0.52060764 -0.8593779  0.6733086  1.4768199
#[2,] -2.3211605 -0.04410615  0.15945799 -1.5154560 -1.5176482  0.1374697
#[3,] -0.7792824 -0.85653735  0.40063430  0.9371025  0.1987466 -0.9809195
#[4,] -0.3141137 -0.13877435 -0.39190827  1.4200912 -0.1809564  0.5827172
#[5,] -1.3527352  0.78055909 -0.63661402 -0.2365094 -0.3829789 -0.8137181
#[6,]  0.3049843 -0.70033280  0.01900144  0.4765084 -0.7540503 -2.0909379
#[7,]  1.0726596 -1.09382977  0.62279794  0.3470011  1.1275294  0.8646476
#[8,]  0.9281181 -0.16476371  1.01723416  0.6716444 -0.6169555  0.5984466
#[9,] -1.2719359 -0.08608032  0.50967856 -0.9645608  0.4374194  0.4465487
#[10,] -0.8540283  1.07240182  0.22499203 -0.7814163  0.5153788 -0.4387466
#
#[1,]     0.8654740
#[2,]    -4.0003431
#[3,]    -0.9556344
#[4,]     0.7849405
#[5,] -4016.6360353
#[6,]    -1.9475162
#[7,]     1.4951978
#[8,]     0.2847806
#[9,]     2.4693086
#[10,]   166.0928567
#
#$convergence
#[1]  0  0  1  1 10  0  1  1  0  1
#
#$minvar
#[1] 5.409046e-07 2.187609e-07 2.270164e-07 1.975476e-07 1.941969e-07
#[6] 1.650412e-07 2.410830e-07 2.103711e-07 2.064031e-07 2.122840e-07
#
#$pvals
#[1] 0.32 0.45 0.00 0.39 0.24 0.28 0.54 0.37 0.32 0.44
#
#
# From the $convergence list only 4 converged (solns 1, 2, 6 and 9)
#
# Of these 4, what are the associated p-values?
#
\dontrun{tmp$pvals[tmp$convergence==0]}
#[1] 0.32 0.45 0.28 0.32
#
# So, all of these are > 0.05, so apparently each of these solutions are
# stationary.
#
# We can further examine these solutions with \code{\link{COEFbothscale}}
# and optionally \code{\link{COEFscale}}.
#
# E.g.
\dontrun{COEFbothscale(tmp)}
#
# This extracts the solution vectors for those where the optimizer converged
# and where the solutions are deemed stationary. Then those are subjected
# to a hierarchical clustering and plotted. So, we can see which solutions
# are similar and which are different.
#
# The function \code{\link{LCTSres}} can produce pictures of the results
# for particular solutions. E.g. the linear combination vectors, the combined
# series and its estimated spectrum.
}
\keyword{ts}
