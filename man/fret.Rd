\name{fret}
\alias{fret}
\docType{data}
\title{Particular section of FTSE log-return series.}
\description{Observations 256:767 from the SP500 log-returns series
	stored in \code{\link{SP500FTSElr}} dataset.
}
\usage{data(fret)}
\format{A vector of 512 observations of the FTSE100 log-returns series
}
\details{Its just more convenient to refer to \code{fret} than to
	\code{SP500FTSElr[256:767,3]}.
}
\source{Yahoo! Finance
}
\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\examples{
\dontrun{ts.plot(fret)}
}
\keyword{datasets}
