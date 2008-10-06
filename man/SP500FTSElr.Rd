\name{SP500FTSElr}
\alias{SP500FTSElr}
\docType{data}
\title{Log-returns time series of the SP500 and FTSE100 indices}
\description{Log-returns of the SP500 and FTSE indices between
	21th June 1995 until 2nd October 2002. Only trading days where
	both indices were recorded are stored. There are 2048
	observations.
}
\usage{data(SP500FTSElr)}
\format{
  A data frame with 2048 observations on the following 3 variables.
  \describe{
    \item{\code{Date}}{The trading day that the index was recorded.}
    \item{\code{SP500lr}}{The log-return for SP500}
    \item{\code{FTSElr}}{The log-return for FTSE100}
  }
}
\source{Downloaded from Yahoo! Finance
}
\references{`Costationarity and stationarity tests for stock index returns'
	by Cardinali and Nason.
}
\examples{
#
# Plot the log-returns for the SP500
#
\dontrun{ts.plot(SP500FTSElr[,2])}
}
\keyword{datasets}
