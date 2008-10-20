\name{pbc.vote}
\alias{pbc.vote}
\docType{data}
\title{pbc.vote}
\description{
   	Precinct level data for Palm Beach County, Florida from the 2000 U.S. Presidential election
}
\usage{pbc.vote}
\format{
  A data frame where each case (row) is one of 516 precincts.  The column variables are defined by:
	\describe{	
  \item{avgage}{Average age of precinct voters}
  \item{tech}{Voting Technology}
  \item{badballots}{combined overvotes and undervotes}
  \item{turnout}{Total ballots cast}
  \item{rep}{Republican}
 \item{npa}{No party affiliation}
 \item{whi}{White}
  \item{bla}{Black}
  \item{his}{Hispanic}
 \item{int}{Independent party}
  \item{new}{Registered to vote after June 30, 2000} 
	}  
  
}
\examples{
\dontrun{
	{
        n <- length(strikes)
        r <- 1
        s.y <- sum(strikes)

        p.posterior.1000000 <- rbeta(1000000,n*r,s.y+0.5)
        length(p.posterior.1000000[p.posterior.1000000<0.05])/1000000

        par(mar=c(3,3,3,3))
        ruler <- seq(0,1,length=1000)
        beta.vals <- dbeta(ruler,n*r,s.y+0.5)
        plot(ruler[1:200],beta.vals[1:200],yaxt="n",main="",ylab="",type="l")
        mtext(side=2,line=1,"Density")
        for (i in 1:length(ruler))
        if (ruler[i] < 0.05)
        segments(ruler[i],0,ruler[i],beta.vals[i])
        segments(0.04,3,0.02,12.2)
        text(0.02,12.8,"0.171")
}
}
}
 
 \source{	Conell, C. and Cohn, S.  (1995).  Learning from Other People's Actions: Environmental Variation and Diffusion in 
French Coal Mining Strikes, 1890-1935
               \emph{American Journal of Sociology} {\bf LI}, American Journal of Sociology.
}
\keyword{datasets}


