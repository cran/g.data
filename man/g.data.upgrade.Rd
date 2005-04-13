\name{g.data.upgrade}
\alias{g.data.upgrade}
\title{Upgrade DDP's Created Prior to R-2.1.0}
\description{
  Earlier DDP's used the \code{delay} function which is deprecated in
  R-2.1.0.  This function modifies old DDP code to use
  \code{delayedAssign} (which only exists in R-2.1.0 and higher).
}
\usage{
  g.data.upgrade(dir=searchpaths()[pos], pos=2, warn=TRUE)
}
\arguments{
  \item{dir}{DDP pathname.}
  \item{pos}{If `dir' is not given, use the DDP in this position of the
    search path.}
  \item{warn}{If TRUE, a warning is generated.}
}
\details{
  You can call this function directly to upgrade old DDP's by explicitly
  passing a pathname.  It is also automatically invoked by
  \code{g.data.attach} as necessary.  Once all your DDP's are upgraded,
  this function will no longer be needed.
}
\value{
  None, used only for its side effect.
}
\seealso{\code{\link{g.data.save}}}
\keyword{data}
