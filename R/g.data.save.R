# Save objects in position "pos" to a delayed-data package:
g.data.save <- function(dir=searchpaths()[pos], obj=all.obj, pos=2, rm.obj) {
  if (is.character(pos)) pos <- match(pos, sub("package:","",search()))
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
    eval(substitute(save(list=i, file=fn)), pos.to.env(pos))
  }
  code <- paste(all.obj, " <- delay(g.data.load(\"", all.obj, "\", \"", pkg,
                "\"))", sep="")
  cat(code, file=file.path(dir, "R", pkg), sep="\n")
}

# Routine used in data packages:  x <- delay(g.data.load("x", "newdata"))
g.data.load <- function(i, pkg) {
  pos <- match(paste("package",pkg,sep=":"), search())
  if (is.na(pos)) {if (interactive()) stop("pkg not found") else pos <- 2}
  env <- pos.to.env(pos)
  load(system.file("data", paste(i,"RData",sep="."), package=pkg), env)
  get(i, envir=env)
}

# Attach a delayed-data package:
g.data.attach <- function(dir, pos=2, warn=TRUE) {
  ## like: library(basename(dir), lib.loc=dirname(dir), char=TRUE)
  env <- attach(NULL, pos, paste("package",basename(dir),sep=":"))
  attr(env, "path") <- dir
  if (file.exists(dir))
    sys.source(file.path(dir, "R", basename(dir)), env, keep.source=FALSE) else
    if (warn) warning(paste("'g.data.save' will create:", dir, "\n"))
}

# Get data from an unattached package:
g.data.get <- function(item, dir) {
  env <- new.env()
  load(file.path(dir, "data", paste(item,"RData",sep=".")), env)
  get(item, envir=env)
}
