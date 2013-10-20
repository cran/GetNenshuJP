\name{GetNenshuData}
\alias{GetNenshuData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get data about Japanese company(e.g. income, employee)
}
\description{
Get dataframe from www.nenshu.jp. This is parser function with "XML" package.
}
\usage{
GetNenshuData(code)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{code}{character or numerical.Security code which is decided by Securities Identification Code Committee.}
}
\details{
Getting data have some column.
\tabular{ll}{
code: \tab Security code\cr
name: \tab company name\cr
year: \tab fiscal year\cr
period: \tab accounting period\cr
employee: \tab number of employee\cr
temporary: \tab number of temporary employee\cr
age: \tab average age\cr
seniority: \tab the length of service\cr
income: \tab average income\cr
release: \tab the day of information discovery\cr
}
}
\references{
http://www.nenshu.jp/
}
\author{
Hayakawa Atsushi
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%%\code{\link{xmlValue}}
%%}
\examples{
GetNenshuData("7203")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
