\name{extractCS}
\alias{extractCS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extractor function for \code{csFSS} object.}

\description{Get much information from the slots of a \code{csFSS}.
	Each slot can carry information from multiple solutions
	per slot. This function permits an arbitrary selection of
	solutions for information from a slot.	
}
\usage{
extractCS(object, slot=c("startpar", "endpar", "convergence",
	"minvar", "pvals", "lcts"), coeftype=c("all", "alpha", "beta",
	"alphafunc", "betafunc"), solno, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{The \code{csFSS} object that you want to extract
	information from.}
  \item{slot}{The slot that you want to get information on. These
	are \code{startpar}: the starting parameters for
	the optimization for each solution;
	\code{endpar}: the final parameters calculated by the
	optimization for each solution;
	\code{convergence}: the status codes returned by
	the optimization for each solution;
	\code{minvar}: the minimum variance of the spectral
	estimate at the optimial solution, one for each
	solution;
	\code{pvals}: the p-values for the test of stationarity
	for the final optimal parameter set;
	\code{lcts}: the (time-varying) linear combination of
	the time series, one for each solution. These are the Z_t
	time series, the combined series which are meant to be
	stationary.

	The \code{startpar}, \code{endpar} and \code{lcts} slots return
	result in
	one vector for each solution requested, organized as a matrix.
	Each row of the matrix corresponds to one of the solutions
	requested. The remaining slots return numbers, one number for
	each solution organized as a vector.}

  \item{coeftype}{For the slots that return coefficients, these can
	be returned in various ways. Each coefficient vector (one per
	solution) actually stores two sets of coefficients: one associated
	with the alpha_t linear combination and the other with
	the beta_t linear combination. Setting \code{coeftype} to
	the following causes the following to happen:
	\code{all}: the complete vector of coefficients is returned
	(these are actually wavelet coefficients corresponding to
	the wavelet specification in the \code{csFSS} object);
	\code{alpha}: only the alpha_t coefficients are returned;
	\code{beta}: only the beta_t coefficients are returned;
	\code{alphafunc}: the alpha_t function (in the time domain)
	is returned, ie as a function in time rather than a set of
	transform coefficients;
	\code{betafunc}: as for \code{alphafunc} but for the beta_t
	function.
}
  \item{solno}{The indices of which solutions you want the information
	on}
  \item{\dots}{Other arguments to \code{\link{coeftofn}}. For example,
	by default the length of the functional representations of
	alpha_t and beta_t is 256 caused by the default \code{n=256}
	of the \code{\link{coeftofn}}} argument. 
}
	
\details{Extracts slot information from \code{csFSS} objects.
}
\value{Information from the relevant slot, as a number, vector or
	matrix depending on what it is that is requested as described
	in the various arguments above.
}
\references{
Cardinali, A. and Nason, G.P. (2012) Costationarity of Locally 
Stationary Time Series using \code{costat}.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{\link{findstysols}}, \code{\link{coeftofn}}}
\examples{
#
# Create dummy data
#
x1 <- rnorm(32)
y1 <- rnorm(32)
#
# Find stationary combinations
# Note: we don't run this example in installation/package formation as
# it takes a long time. However, this precise command IS run in
# the help to findstysols
#
\dontrun{ans <- findstysols(Nsims=100, tsx=x1, tsy=y1)}
#
# Get the optimal (endpar) alphas for the first 10 solutions
#
\dontrun{extractCS(ans, slot="endpar", coeftype="alpha", solno=1:10)}
#
# Plot the beta_t associate with the optimal solution for solution 29
#
\dontrun{ts.plot(extractCS(ans, slot="endpar", coeftype="betafunc",
	solno=29))}
#
# Get the p-value associated with solution 29
#
\dontrun{extractCS(ans, slot="pvals", solno=29)}
}
\keyword{ts}
