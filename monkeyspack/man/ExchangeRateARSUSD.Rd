\name{ExchangeRateARSUSD.rds}
\alias{ExchangeRateARSUSD.rds}
\docType{data}
\title{ExchangeRateARSUSD.rds
%%   ~~ data name/kind ... ~~
}
\description{Exchange rate of Argentinian peso/ United States Dollar from January 2002 to June 2018
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{readRDS("data/ExchangeRateARSUSD.rds")}
\format{
  A data frame with 4455 observations on the following 2 variables.
  \describe{
    \item{\code{dates}}{Factor w/ 4455 levels}
    \item{\code{currency_price}}{a numeric vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(ExchangeRateARSUSD)
readRDS("data/ExchangeRateARSUSD.rds")
## maybe str(ExchangeRateARSUSD) ; plot(ExchangeRateARSUSD) ...
}
\keyword{datasets}
