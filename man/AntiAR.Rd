\name{AntiAR}
\alias{AntiAR}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Undo autoreflection action for an EWS object (wd stationary)
}
\description{The \code{\link{BootTOS}} function has the ability to
deal with boundary conditions by augmenting the right-hand end of a time
series by a reflected version of that series. So, the series doubles
in length and the new vector has periodic boundary conditions. One
can then compute a local spectrum on this data which returns an EWS
in a \code{wd} object, usually with a type attribute of "station".
This function can take this \code{wd} object and properly can return
the first half of it, which corresponds to the boundary-correct spectrum
of the original series.
}
\usage{
AntiAR(S)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{A \code{wd} class object of type "station". This corresponds
	to a EWS estimate on a reflected time series.
}
}
\details{
This function arises because using spectral estimation functions,
	like \code{ewspec} from the \code{wavethresh} package doesn't
	always work that well at the boundaries. This is because
	the wavelet functions in \code{wavethresh} usually assume
	periodic boundary conditions and this is not appropriate for
	a discrete time series where time 1 and time T are usually
	very different (and cannot be assumed to be the same).

Hence, a previous function could generate a new time series by taking the
original, e.g. \code{x}, reflecting it with \code{rev(x)} and then
sticking the reflected onto the right-hand end of the original. 
Spectral estimation, (e.g. using \code{ewspec}) can then be applied
to this new reflected/augmented series and the boundaries are now
roughly correct as the start and end of the series correspond to time 1.

The spectral estimate so obtained though is double the size of the
the one that is needed, and contains the spectrum of the reflected series.
Hence, this function obtains the first half of the estimate and returns
it.

Not usually intended for the casual user
}
\value{A \code{wd} class object containing the boundary-corrected
	estimate of the spectrum for the original series.
}

\references{
Cardinali, A. and Nason, G.P. (2012) Costationarity of Locally 
Stationary Time Series using \code{costat}.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{
G. P. Nason.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{BootTOS}}
}
\examples{
#
# Generate example, temporary series
#
x <- rnorm(128)	
#
# Reflect it about its end point
#
x2 <- c(x, rev(x))
#
# Compute EWS estimate
#
x2ews <- ewspec(x2)
#
# Now get bit corresponding to x into object
#
xews <- AntiAR(x2ews$S)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}
\keyword{smooth}% __ONLY ONE__ keyword per line
