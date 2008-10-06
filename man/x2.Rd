\name{x2}
\alias{x2}
\docType{data}
\title{Particular section of SP500 log-returns series.}
\description{Observations 513:1024 from the SP500 log-return series
	stored in \code{\link{SP500FTSElr}} dataset.
}
\usage{data(x2)}
\format{A vector of 512 observations of the SP500 log-returns series.
}
\details{Its just more convenient to refer to \code{x2} than to
	\code{SP500FTSElr[513:1024,2]}.
}
\source{Yahoo! Finance}
\references{`Costationarity and stationarity tests for stock index returns'
	by Cardinali and Nason.
}
\examples{
\dontrun{ts.plot(x2)}
}
\keyword{datasets}
