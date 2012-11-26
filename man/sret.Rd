\name{sret}
\alias{sret}
\docType{data}
\title{Particular section of SP500 log-returns series.}
\description{Observations 256:767 from the SP500 log-return series
	stored in \code{\link{SP500FTSElr}} dataset.
}
\usage{data(sret)}
\format{A vector of 512 observations of the SP500 log-returns series.
}
\details{Its just more convenient to refer to \code{sret} than to
	\code{SP500FTSElr[256:767,2]}.
}
\source{Yahoo! Finance}
\references{
Cardinali, A. and Nason, G.P. (2012) Costationarity of Locally 
Stationary Time Series using \code{costat}.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\examples{
\dontrun{ts.plot(sret)}
}
\keyword{datasets}
