## Save objects in position "pos" to a delayed-data package:
g.data.save <- function(dir=searchpaths()[pos], obj=all.obj, pos=2, rm.obj,
                        compress=FALSE) {
  if (is.character(pos)) pos <- match(pos, sub("package:","",search()))
  if (!is.null(attr(pos.to.env(pos), "readonly"))) stop("Read-Only!")
  pkg <- basename(dir)
  for (i in file.path(dir,c("","data","R"))) if (!file.exists(i)) dir.create(i)
  if (!missing(rm.obj)) {
    rm(list=rm.obj, pos=pos)
    file.remove(file.path(dir, "data", paste(rm.obj,"RData",sep=".")))
  }
  all.obj <- objects(pos, all.names=TRUE)
  for (i in obj) {
    get(i, pos)                              # Put on-shell if a promise object
    fn <- file.path(dir, "data", paste(i,"RData",sep="."))
    save(list=i, file=fn, envir=pos.to.env(pos), compress=compress)
  }
  code <- paste(all.obj, " <- delay(g.data.load(\"", all.obj, "\", \"", pkg,
                "\"))", sep="")
  if (!length(all.obj)) code <- ""
  cat(code, file=file.path(dir, "R", pkg), sep="\n")
  fn <- file.path(dir, "DESCRIPTION")
  cat(paste("Package:",pkg), "Version: 1.0", paste("Date:",date()),
      "Title: DDP", "Author: You", "Maintainer: You <u@u.com>",
      "Description: DDP", "License: GPL", sep="\n", file=fn)
}

## Routine used in data packages:  x <- delay(g.data.load("x", "newdata"))
g.data.load <- function(i, pkg) {
  pos <- match(paste("package",pkg,sep=":"), search())
  if (is.na(pos)) {if (interactive()) stop("pkg not found") else pos <- 2}
  env <- pos.to.env(pos)
  load(system.file("data", paste(i,"RData",sep="."), package=pkg), env)
  get(i, envir=env)
}

## Attach a delayed-data package:
##  kinda like: library(basename(dir), lib.loc=dirname(dir), char=TRUE)
g.data.attach <- function(dir, pos=2, warn=TRUE, readonly=FALSE) {
  pkg <- basename(dir)
  env <- attach(NULL, pos, paste("package",pkg,sep=":"))
  attr(env, "path") <- dir
  if (readonly) attr(env, "readonly") <- TRUE
  if (file.exists(dir)) {
    sys.source(file.path(dir, "R", pkg), env, keep.source=FALSE)
    if (!file.exists(fn <- file.path(dir, "DESCRIPTION")) ||
        is.na(read.dcf(fn,"Version")[1,1]))            # Backward compatibility
      cat(paste("Package:",pkg), "Version: 1.0", paste("Date:",date()),
          "Title: DDP", "Author: You", "Maintainer: You <u@u.com>",
          "Description: DDP", "License: GPL", sep="\n", file=fn)
  } else {
    if (warn) warning(paste("'g.data.save' will create:", dir, "\n"))
  }
}

## Get data from an unattached package:
g.data.get <- function(item, dir) {
  env <- new.env()
  load(file.path(dir, "data", paste(item,"RData",sep=".")), env)
  get(item, envir=env)
}

## Put data into an unattached package:
g.data.put <- function(item, value, dir) {
  g.data.attach(dir)
  assign(item, value, 2)
  g.data.save(obj=item)
  detach(2)
}
