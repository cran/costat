\name{y2}
\alias{y2}
\docType{data}
\title{Particular section of FTSE log-return series.}
\description{Observations 513:1024 from the SP500 log-returns series
	stored in \code{\link{SP500FTSElr}} dataset.
}
\usage{data(y2)}
\format{A vector of 512 observations of the FTSE100 log-returns series
}
\details{Its just more convenient to refer to \code{y2} than to
	\code{SP500FTSElr[513:1024,3]}.
}
\source{Yahoo! Finance
}
\references{`Costationarity and stationarity tests for stock index returns'
	by Cardinali and Nason.
}
\examples{
\dontrun{ts.plot(y2)}
}
\keyword{datasets}
