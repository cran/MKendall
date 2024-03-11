\name{MSK}
\alias{MSK}

\title{
Estimating Row and Column Sample Matrix Kendall's Tau
}
\description{
This function is to estimate row and column sample matrix Kendall's tau which are defined in He et al. (2022) <arXiv:2207.09633>
}
\usage{
MSK(X, type = "1")
}

\arguments{
  \item{X}{
Input three-dimensional array, of dimension \eqn{T \times p \times q}.
\eqn{T} is the sample size, \eqn{p} is the row dimension of each matrix observation and \eqn{q} is the column dimension of each matrix observation.
}
  \item{type}{
If type=1, calculate the row sample matrix Kendall's tau; if type=2,  calculate the column sample matrix Kendall's tau. The default is the row sample matrix Kendall's tau.
}
}
\details{
See He at al. (2022) <arXiv:2207.09633> for details.
}
\value{
If type=1, the return value is a \eqn{p \times p} matrix; if type=2, the return value is a \eqn{q \times q} matrix.
}
\references{
He, Y., Wang, Y., Yu, L., Zhou, W., & Zhou, W. X. (2022). A new non-parametric Kendall's tau for matrix-value elliptical observations <arXiv:2207.09633>.
}
\author{
Yong He, Yalin Wang, Long Yu, Wang Zhou and Wenxin Zhou.
}

\examples{
X=array(rnorm(400),c(20,5,4))
MSK(X,1)

}

