\name{g.data.save}
\alias{g.data.attach}
\alias{g.data.get}
\alias{g.data.load}
\alias{g.data.save}
\title{
  Create and Maintain Delayed-Data Packages
}
\description{
g.data.save reads the data in position "pos", and writes them as a
delayed-data package to "dir".  Data objects are initially created as
promise objects, the promise being to load the data and return it the
first time the item is requested.

g.data.attach attaches such a package, in position 2 by default.

g.data.get can be used to get a single piece of data from a package,
without attaching the package.

g.data.load is an internal subroutine used to create the promise objects.
}
\usage{
 g.data.attach(dir, pos=2, warn=TRUE)
 g.data.get(item, dir)
 g.data.load(i, pkg)
 g.data.save(dir=searchpaths()[pos], obj=all.obj, pos=2, rm.obj)
}
\arguments{
 \item{dir}{Directory (full pathname) of package.  Data is stored in its
   /data subdirectory, as <item>.RData .}
 \item{pos}{Search path position}
 \item{warn}{Logical: warn user if directory being attached doesn't exist}
 \item{item}{Item to retrieve from an unattached package.}
 \item{i}{Item to evaluate.}
 \item{pkg}{Package name where item is stored.}
 \item{obj}{Objects to save; defaults to all objects in position "pos".}
 \item{rm.obj}{Objects to remove, both in memory and on disk.}
}
\details{
Data stored in a delayed-data package (DDP) are available on demand, but
do not take up memory until requested.  You attach a DDP with
g.data.attach(), then read from it and assign to it in a manner similar
to S-Plus, except that you must run g.data.save() to actually commit to
disk.

You can create a DDP from any position in the search path; e.g. you can
attach a list or dataframe, and its components will become objects in
the DDP.  In this case, the call to g.data.save(dir) must specify the path
where files will be saved.  Alternately, you can attach an existing DDP
(or create a new, empty one) with g.data.attach(dir).  In this case, the
path is stored as an attribute of the environment (see
\code{\link{searchpaths}}), and you can call g.data.save() with no arguments.
}
\value{
  g.data.get returns the requested data.

  g.data.load returns data after loading it.
}
\seealso{\code{\link{delay}}, \code{\link{autoload}},
         \code{\link{searchpaths}}}
\examples{
\dontrun{
ddp <- tempfile("newdir")           # Where to put the files
g.data.attach(ddp)                  # Warns that this is a new directory
assign("x1", matrix(1, 1000, 1000), 2)
assign("x2", matrix(2, 1000, 1000), 2)
g.data.save()                       # Writes the files
detach(2)

g.data.attach(ddp)                  # No warning, because directory exists
ls(2)
system.time(print(dim(x1)))         # Takes time to load up
system.time(print(dim(x1)))         # Second time is faster!
find("x1")                          # x1 still lives in pos=2, is now real
assign("x3", x2*10, 2)
g.data.save()                       # Or just g.data.save(obj="x3")
detach(2)

myx2 <- g.data.get("x2", ddp)       # Get one objects without attaching
unlink(ddp, recursive=TRUE)         # Clean up this example
}

\dontrun{
ddp <- tempfile("newdir")           # New example
y <- list(x1=1:1000, x2=2:1001)
attach(y)                           # Attach an existing list or dataframe
g.data.save(ddp)
detach(2)
unlink(ddp, recursive=TRUE)         # Clean up this example
}
}
\keyword{data}
