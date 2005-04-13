g.data.upgrade <- function(dir=searchpaths()[pos], pos=2, warn=TRUE) {
  if (is.character(pos)) pos <- match(pos, sub("package:","",search()))
  fn <- file.path(dir, "R", basename(dir))
  x <- readLines(fn)
  x <- sub("(.*) <- delay\\(g.data.load\\(\"(.*)\", \"(.*)\"\\)\\)",
           "delayedAssign(\"\\1\", g.data.load(\"\\1\", \"\\3\"))", x)
  cat(x, sep="\n", file=fn)
  if (warn) warning("Upgraded DDP '", dir, "'")
}
