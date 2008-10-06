\name{costat-package}
\alias{costat-package}
\alias{costat}
\docType{package}
\title{Find costationary time series, compute time-localized autocovariance,
	time-localized cross-covariance, time-localized spectrum.
}
\description{Main purpose of package is, given two time series, \deqn{X_t}
and \deqn{Y_t}, find the time-varying linear combination
\deqn{Z_t = \alpha_t X_t + \beta_t Y_t} such that \deqn{Z_t} is
a stationary series. Essentially, the program begins using random starts
for the coefficient vector of \deqn{(\alpha_t, \beta_t)} and then optimizes
a measure of stationarity over possible combinations of the coefficient
function. 
}
\details{
\tabular{ll}{
Package: \tab costat\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2007-06-13\cr
License: \tab GPL version 2 or newer
}
For costationarity the most important function is the
\code{\link{findstysols}} which, given two time series,
\code{tsx} and \code{tsy}, and a given complexity \code{Ncoefs},
finds the set (number dependent and equal to \code{Nsims})
of solutions that solve the optimization problem ``find the coefficient
vectors that cause the time-varying linear combination of \code{tsx}
and \code{tsy} to be as most stationary as possible''.
\code{\link{findstysols}} is capable of running one optimization run,
or several. The former can be useful when running a single optimization
job as part of several on a multiprocessor machine, however the function
can run all your optimization jobs a single call (it'll just take longer).

Typically, one should start with \code{Ncoefs} small (it must be one less
than a dyadic number, e.g. 3, 7, 15, for Haar wavelets), 3 say, run
several optimizations to see how likely the linear combinations are to
be stationary. If the time series themselves are fairly stationary
(but not exactly, if they were both stationary then it is trivial
to find a stationary linear combination) then it will be easy to find
many stationary combinations. If the time series are highly non-stationary,
evolving significantly over time, then more complex time-varying combination
vectors will be required to get stationarity more often.
Note that since the algorithm is based on numerical optimization, one is
never guaranteed to find an optimal solution even if one exists. 
Our methodology adopts the time-honoured strategy of multiple random
starts to find stationary solutions. If many solutions indicate
stationarity has been reached then you have found the types and complexity
of solutions that give stationarity. If many solutions indicate that
stationarity has not been reached (look at the p-value output of
\code{link{findstysols}}) then you should increase \code{Ncoefs} to the
next dyadic number (less one) and then try again.

Once you have discovered a set of solutions which appear to contain
a number of stationary ones then the functions \code{\link{LCTSres}}
and \code{\link{COEFbothscale}} can be used to interrogate the solution
set to find clusters of solutions. Since \code{\link{findstysols}} uses
multiple numerical optimizations the solution set tends to cluster around
a number of ``true'' linear combination vectors and the interrogation
functions are useful for identifying the number of these, and also what
they represent.

The function \code{\link{mergexy}} can merge together the result sets
of multiple \code{\link{findstysols}} runs with the resultant object
being as if one run of \code{\link{findstysols}} had been used.
This again can be useful to concatenate result sets from multiple
runs on, say, a multiprocessor/core machine.

The test of stationarity is contained in the \code{\link{myTOS}}
function, which can take a time series and uses a bootstrap method
to test for stationarity. The exact test statistic is contained
in \code{\link{TOSts}} which is called by \code{\link{myTOS}}.
In principle, another test of stationarity could be substitued for
the wavelet based bootstrap test, a Fourier test, or something else
could potentially be used. The costationarity idea is independent
of the particular test statistic used. However, the judgement of 
a particular linear combination of two time series and whether it is
stationary does, of course, depend on the particular test used.


This package also contains a number of other functions of use in
analysing single and pairs of locally stationary time series. For example,
\code{\link{lacv}} computes the time-localized autocovariance of
any time series (including stationary ones!) using a wavelet method (although
the quantity itself only depends on time and lag), \code{\link{crosslacv}}
performs a similar job, but computes the cross time-localized covariance
between two series, \code{\link{ewcrossspec}} the time-localized cross
spectrum, \code{\link{localvar}} computes the time-localized variance of
the series, again computed via a wavelet method, but not, in the end,
dependent on it, just time.

The functions \code{\link{tstosscan}} and \code{\link{plottstosscan}}
take a single time series, apply a test of stationarity at multiple
locations on multiple length windows of a series, and the latter function
produces a plot that displays the results of the tests. These functions
are very useful in discovering the degree of non-stationarity of a time series.
I.e. not only whether a series, or bits of it, are non-stationary, but where
and also over what periods. E.g. a series might be stationary over a window
of 32 or 64 observations, but decidedly non-stationary over a longer
time period (plus the statistical power of detection is greater for
longer series as there is generally more information).
 
}
\author{
Guy Nason

Maintainer: Who to complain to <g.p.nason@bristol.ac.uk}
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason, 2008, University of Bristol Technical Report 08:08.}
\keyword{ package }
\examples{
#
# See comprehensive examples in the help pages for the major functions
# outlined above.
#
}
