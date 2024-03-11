\name{MKER}
\alias{MKER}

\title{
Estimating Factor Numbers via Matrix Kendall's Tau Eigenvalue-Ratio Method
}
\description{
This function is to estimate row and column factor numbers via Matrix Kendall's Tau Eigenvalue-Ratio Method.
}
\usage{
MKER(X, kmax)
}

\arguments{
  \item{X}{
Input three-dimensional array, of dimension \eqn{T \times p \times q}.
\eqn{T} is the sample size, \eqn{p} is the row dimension of each matrix observation and \eqn{q} is the column dimension of each matrix observation.
}
  \item{kmax}{
The user-supplied maximum factor numbers.
}
}
\details{
See He at al. (2022) <arXiv:2207.09633> for details.
}
\value{
\item{khat}{The estimated row factor number.}
\item{rhat}{The estimated column factor number.}
}
\references{
He, Y., Wang, Y., Yu, L., Zhou, W., & Zhou, W. X. (2022). A new non-parametric Kendall's tau for matrix-value elliptical observations <arXiv:2207.09633>.
}
\author{
Yong He, Yalin Wang, Long Yu, Wang Zhou and Wenxin Zhou.
}

\examples{
set.seed(123456)
T=20;p=10;q=10;k=2;r=2
R=matrix(runif(p*k,min=-1,max=1),p,k)
C=matrix(runif(q*r,min=-1,max=1),q,r)
  X=Y=E=array(0,c(T,p,q))
    for(i in 1:T){
      Y[i,,]=R\%*\%matrix(rnorm(k*r),k,r)\%*\%t(C)
      E[i,,]=matrix(rnorm(p*q),p,q)
    }
    X=Y+E

fn=MKER(X,9)
fn$khat;
fn$rhat
}
