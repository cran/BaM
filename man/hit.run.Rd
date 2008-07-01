\name{hit.run}
\alias{hit.run}

\title{hit.run}
\description{
    Implementation of hit.run algorithm, p. 377.
}
\usage{hit.run(theta.mat,reps,I.mat)}
\arguments{
    \item{theta.mat}{theta.mat}
    \item{reps}{reps}
    \item{I.mat}{I.mat}
   
}
\examples{
\dontrun{
#code to implement graph on p. 378, see page 393.
		
num.sims <- 10000
Sig.mat <- matrix(c(1.0,0.95,0.95,1.0),2,2)
walks<-rbind(c(-3,-3),matrix(NA,nrow=(num.sims-1),ncol=2))
walks <- hit.run(walks,num.sims,Sig.mat)
z.grid <- outer(seq(-3,3,length=100),seq(-3,3,length=100),
FUN=dmultinorm,c(0,0),Sig.mat)
contour(seq(-3,3,length=100),seq(-3,3,length=100),z.grid,
levels=c(0.05,0.1,0.2))
points(walks[5001:num.sims,],pch=".")


}
}

\keyword{models}




