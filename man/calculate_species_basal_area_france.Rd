\name{caculate_species_basal_area_france}
\alias{caculate_species_basal_area_france}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Species basal area per plot for french NFI
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
caculate_species_basal_area_france(dbh, species, site)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dbh}{
Numeric vector of tree dbh (data base field c13).
}
\item{species}{
Character vector of species names. Same length as dbh.
}
\item{site}{
Vector of site ids (data base field idp). Same length as dbh.
}
}
\details{
}
\value{
}

\references{
%% ~put references to the literature/web site here ~
}
\author{
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
dbh <- abs(rnorm(10, 100, 60))
site <- sample.int(4,10, replace=T)
species <- LETTERS[sample.int(4,10, replace=T)]

caculate_species_basal_area_france(dbh, species, site)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
