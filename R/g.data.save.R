## Convert object name <-> filename, e.g. aBcD <-> dir/a@Bc@D.RData ("@" needed for Windows):
g.data.mash   <- function(dir, obj)
  file.path(dir, paste(gsub("([[:upper:]])", "@\\1", obj), "RData", sep="."))
g.data.unmash <- function(fn) gsub("@", "", sub("\\.RData$", "", basename(fn)))

## Attach (or virtually create) a delayed-data package ("DDP"):
g.data.attach <- function(dir, pos=2, warn=TRUE, readonly=FALSE, backward=FALSE) {
    env <- attach(NULL, pos, paste("package", basename(dir), sep=":"))   # Need for searchpaths()
    attr(env, "path")     <- dir
    attr(env, "readonly") <- readonly
    if (!file.exists(dir)) {if (warn) warning("New DDP: ", dir); return(invisible())}
    if (file.exists(file.path(dir, "data"))) g.data.upgrade(dir, backward=backward)
    for (fn in dir(dir, pattern="\\.RData$", all.files=TRUE, full.names=TRUE))
      eval(substitute(delayedAssign(OB, get(load(FN))), list(OB=g.data.unmash(fn), FN=fn)), env)
}

## Save objects in position "pos" to a delayed-data package:
g.data.save <- function(dir=attr(env, "path"), obj=ls(env, all.names=TRUE), pos=2, rm.obj=NULL) {
    if (is.character(pos)) pos <- match(pos, search())
    if (is.na(pos)) stop("pos not found")
    env <- pos.to.env(pos)
    if (isTRUE(attr(env, "readonly"))) stop("Read-Only!")
    if (!file.exists(dir)) dir.create(dir)
    if (length(rm.obj)) {rm(list=rm.obj, pos=pos); file.remove(g.data.mash(dir, rm.obj))}
    is.promise <- function(i) is.call(eval(parse(text=paste("substitute(", i, ", env)"))))
    for (i in obj) if (!is.promise(i)) save(list=i, file=g.data.mash(dir, i), envir=env)
}

## Get data from an unattached package:
g.data.get <- function(item, dir) get(load(g.data.mash(dir, item)))

## Put data into an unattached package:
g.data.put <- function(item, value, dir) {
    assign(item, value)
    save(list=item, file=g.data.mash(dir, item))
}
