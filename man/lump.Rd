\name{lump}
\alias{lump}
\title{Lump Function}
\usage{
lump(...)
}
\arguments{
\item{...}{The subsets, given as vectors.}

}
\description{
Lumps vectors into a data frame. Adds NAs to make all the lengths the same.
}
\examples{
x<-c(1,2,3,4)
y<-c(3,4,5)
z<-c(2,3,4,5,6)
lump(x,y,z)
}
