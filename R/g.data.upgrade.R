g.data.upgrade <- function(dir, warn=TRUE, backward=FALSE) {
    if (file.access(dir, mode=2) < 0) stop("Cannot upgrade ", dir)
    owd <- setwd(dir);  if (!is.null(owd)) on.exit(setwd(owd))
    if (!all(file.exists(c("R","data")))) stop("No upgrade needed on ", dir)
    if (file.exists("UPGRADED")) {        # This ddp was upgraded but with backward compatibility
        if (backward) {warning(dir, " already upgraded with backward=TRUE"); return()}
        warning(dir, " removing backward-compatibility")
        newfiles <- dir(".",    pattern="\\.RData$", all.files=TRUE)
        oldfiles <- dir("data", pattern="\\.RData$", all.files=TRUE)  # These should all be links
        if (!identical(sort(gsub("@","",newfiles)), sort(oldfiles))) stop("Data file mismatch!")
        if (!identical(dir("R"), basename(dir))) stop("R file mismatch!")
        unlink(c("DESCRIPTION", "UPGRADED", "data", "R"), recursive=TRUE)            # Dangerous!
        return()
    }
    for (i in dir("data", pattern="\\.RData$", all.files=TRUE)) {  # Move data files to top level
        file.rename(file.path("data", i), new <- g.data.mash(dir, sub("\\.RData$", "", i)))
        if (backward) file.symlink(new, file.path("data", i))
    }
    if (!backward && !all(file.remove(file.path("R", basename(dir)), "data", "R")))
      stop("Upgrade failed on ", dir)  # If directories not empty, this will fail (intentionally)
    if (!backward && file.exists("DESCRIPTION")) file.remove("DESCRIPTION")    # It may not exist
    if (backward) file.create("UPGRADED")
    if (warn) warning("Upgraded DDP: ", dir)
}
