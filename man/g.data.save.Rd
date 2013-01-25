\name{g.data.save}
\alias{g.data.attach}
\alias{g.data.save}
\alias{g.data.get}
\alias{g.data.put}
\title{
  Create and Maintain Delayed-Data Packages
}
\description{
g.data.save reads the data in search position "pos", and writes them as a
delayed-data package ("DDP") to "dir".  Data objects are initially created as
promise objects, the promise being to load the data and return it the
first time the item is requested.

g.data.attach attaches such a package, in position 2 by default.
}
\usage{
 g.data.attach(dir, pos=2, warn=TRUE, readonly=FALSE)
 g.data.save(dir=attr(env, "path"), obj=ls(env, all.names=TRUE),
             pos=2, rm.obj=NULL)
 g.data.get(item, dir)
 g.data.put(item, value, dir)
}
\arguments{
 \item{dir}{Directory (full pathname) of DDP.}
 \item{pos}{Search path position.}
 \item{warn}{Logical: warn user if directory being attached doesn't exist}
 \item{readonly}{Logical: set an attribute on the package that will cause
                 \code{g.data.save} to abort.}
 \item{obj}{Object name(s).}
 \item{rm.obj}{Objects to remove, both in memory and on disk.}
 \item{item}{Item to retrieve from an unattached package.}
 \item{value}{Value for the data item being put with \code{g.data.put}.}
}
\details{
Data stored in a delayed-data package (DDP) are available on demand, but
do not take up memory until requested.  You attach a DDP with
\code{g.data.attach}, then read from it and assign to it via its
position on the search path (similar to S-Plus).  Unlike S-Plus, you
must run g.data.save() to actually commit to disk.

You can create a DDP from any position in the search path, not just one
created with \code{g.data.attach}; e.g. you can attach a list or
dataframe, and its components will become objects in the DDP.  In this
case, the call to \code{g.data.save(dir)} must specify the path where
files will be saved.  If the DDP was created with \code{g.data.attach},
then its directory is known and does not need to be passed again to \code{g.data.save}.

The filename associated with an object `obj' is `obj.RData', except that
uppercase letters are preceded by an `@' symbol.  This is required by
Windows since `x.RData' and `X.RData' are the same file under that OS.
Unexported functions \code{g.data.mash} and \code{g.data.unmash} perform the object name /
filename conversion, e.g. g.data.mash(dir, "aBcD") returns "dir/a@Bc@D.RData".

g.data.get can be used to get a single piece of data from a package,
without attaching the package.  g.data.put puts a single item into an
unattached package.
}
\value{
  g.data.get returns the requested data.
}
\seealso{\code{\link{delayedAssign}}}
\examples{
\dontrun{
ddp <- tempfile("newdir")           # Where to put the files
g.data.attach(ddp)                  # Warns that this is a new directory
assign("m1", matrix(1, 5000, 1000), 2)
assign("m2", matrix(2, 5000, 1000), 2)
g.data.save()                       # Writes the files
detach(2)

g.data.attach(ddp)                  # No warning, because directory exists
ls(2)
system.time(print(dim(m1)))         # Takes time to load up
system.time(print(dim(m1)))         # Second time is faster!
find("m1")                          # m1 still lives in pos=2, is now real
assign("m3", m1*10, 2)
g.data.save()                       # Or just g.data.save(obj="m3")
detach(2)

mym2 <- g.data.get("m2", ddp)       # Get one objects without attaching
unlink(ddp, recursive=TRUE)         # Clean up this example
}

\dontrun{
ddp <- tempfile("newdir")           # New example
y <- list(m1=1:1000, m2=2:1001)
attach(y)                           # Attach an existing list or dataframe
g.data.save(ddp)
detach(2)
unlink(ddp, recursive=TRUE)         # Clean up this example
}
}
\keyword{data}
