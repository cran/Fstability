\name{asm}
\alias{asm}
\title{Adjusted Stability Measure}
\usage{
asm(x,y)
}
\arguments{
\item{x}{A dataframe where each column is a subset.}

\item{y}{A vector of the original set.}
}
\description{
Calculates feature stability using asm.
}
\examples{
x<-c(1,2,3,4)
y<-c(3,4,5)
z<-c(2,3,4,5,6)
T<-lump(x,y,z)
A<-c(1,2,3,4,5,6,7)
asm(T,A)
}
\details{
This formula to calculate feature selection stability comes from:

Lustgarten, J. L., Gopalakrishnan, V., & Visweswaran, S. (2009). Measuring Stability of Feature Selection in Biomedical Datasets. AMIA Annual Symposium Proceedings, 2009, 406–410.
}
