\name{g.data.upgrade}
\alias{g.data.upgrade}
\title{Upgrade a version 1 DDP to version 2}
\description{
  Convert an old-style DDP to the current format.
}
\usage{
g.data.upgrade(dir, warn=TRUE, backward=FALSE)
}
\arguments{
  \item{dir}{Directory containing the DDP.}
  \item{warn}{Logical: produce a warning.}
  \item{backward}{Logical: Make DDP compatible with old g.data, using
    symbolic links.}
}
\details{
  Version 1 DDP's were structured like a package, with a `DESCRIPTION'
  file, `R' subdirectory, and `data' subdirectory.  In version 2, all
  data files live at the top level.  Also, data filenames have `@'
  symbols preceding uppercase letters, see \code{\link{g.data.mash}}.

  With backward=TRUE, we attempt to make a DDP valid under both v1 and
  v2 g.data.  Symbolic links are used, so this won't work under Windows.
  If you take one of these backward-compatible DDP's and run
  \code{g.data.upgrade(backward=FALSE)}, the backward-compatibility is
  eliminated.
}
\value{
  None, used for its side effect of reconfiguring `dir'.
}
\seealso{\code{\link{g.data.mash}}}
\keyword{data}
