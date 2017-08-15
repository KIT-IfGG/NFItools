\name{threshold_kappa}
\alias{threshold_kappa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Threshold calculation using Kappa statistics
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
threshold_kappa(x = 0.5, obs_values = obs, pred_values = pred, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{obs_values}{
%%     ~~Describe \code{obs_values} here~~
}
  \item{pred_values}{
%%     ~~Describe \code{pred_values} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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

\code{\link{calc_kappa}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
### Dummy data
obs <- sample(c(0,1), size=n, prob=c(0.3,0.7), replace=T)
pred <- plogis(rnorm(n, mean=obs, sd=2)

### Caculate kappa for this data  
calc_kappa(x=0.8, obs_values = obs, pred_values = pred)

### Calculate best threshold
threshold_kappa(x=0.8, obs_values = obs, pred_values = pred)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
