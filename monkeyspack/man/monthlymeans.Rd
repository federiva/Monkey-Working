\name{monthly.means}
\alias{monthly.means}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{monthly.means
%%  ~~monthly means from daily data ~~
}
\description{Using daily data, calculates monthly means for all the given years
%%  ~~ Using daily data, calculates monthly means for all the given years ~~
}
\usage{
monthly.means(DataX, ncol.interest, ncol.date = NULL, ncol.year = NULL, ncol.month = NULL, NA.PRESENCE = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DataX}{It's a data.frame with daily data for some variable
%%     ~~description~~
}
 \item{ncol.interest}{Column's index of the variable X (numeric)
%%     ~~description~~
}
 \item{ncol.date}{Column's index of the dates's column. Default is NULL
%%     ~~description~~
}
 \item{ncol.year}{Index of the column which has the year of each Xi. Default is NULL (numeric)
%%     ~~description~~
}
 \item{ncol.month}{Index of the column which has the number of the month of each Xi. Default is NULL (numeric)
%%     ~~description~~
}
 \item{NA.PRESENCE}{If TRUE, variable has NAs in it. If FALSE, it hasn't. Default is FALSE.
%%     ~~description~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns a data.frame with three columns: 
\item{"month.order"}{Column with the number of the month (from 1 to 12) corresponding to each value}
\item{"year"}{Column with the year}
\item{"mean.value"}{Column with the monthly mean of the observation}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{"month.order" }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Natalia Mariana Denkiewicz
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
