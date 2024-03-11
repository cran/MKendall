\name{MRTS}
\alias{MRTS}

\title{
Matrix Robust Two-Step Algorithm for Large-Dimensional Matrix Elliptical Factor Model
}
\description{
This function is to fit the large-dimensional matrix elliptical factor model via the Matrix Robust Two-Step (RTS) algorithm.
}
\usage{
MRTS(X, k, r)
}

\arguments{
  \item{X}{
Input three-dimensional array, of dimension \eqn{T \times p \times q}.
\eqn{T} is the sample size, \eqn{p} is the row dimension of each matrix observation and \eqn{q} is the column dimension of each matrix observation.
}
  \item{k}{
A positive integer indicating the row factor numbers.
}
  \item{r}{
A positive integer indicating the column factor numbers.
}
}
\details{
See He at al. (2022) <arXiv:2207.09633> for details.
}
\value{
The return value is a list. In this list, it contains the following:
\item{Rloading}{ The estimated row loading matrix of dimension \eqn{p \times k}}
\item{Cloading}{ The estimated column loading matrix of dimension \eqn{q \times r}}
\item{Fhat}{ The estimated factor matrices, are output in the form of a three-dimensional array with dimensions of \eqn{T \times k \times r}. \eqn{T} is the sample size, \eqn{k} and \eqn{r} are the row and column dimensions of each factor matrix, respectively.
 }
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

fit=MRTS(X,k,r)
fit$Rloading;fit$Cloading;fit$Fhat

}

