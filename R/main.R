
#' Save R Object using parallel compression
#' @description save writes an external representation of R objects to the specified file.
#' The objects can be read back from the file at a later date
#' by using the function load or attach (or data in some cases).
#' @param ... the names of the objects to be saved (as symbols or character strings).
#' @param list A character vector containing the names of objects to be saved.
#' @param file the name of the file where data will be saved
#' @param envir environment to search for objects to be saved.
#' @param n.cores number of cores to use to compress
#' @param eval.promises logical: should objects which are promises be forced before saving?
#' @param precheck logical: should the existence of the objects be checked before starting to
#' save (and in particular before opening the file/connection)?
#' @export save.fast
save.fast <- function (...,
                       list = character(),
                       file = stop("'file' must be specified"),
                       envir = parent.frame(),
                       n.cores = 4,
                       eval.promises = TRUE,
                       precheck = TRUE)
{
  if (.Platform$OS.type == "unix") {
    if (system(
      'command -v pigz',
      wait = T,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    ) > 0) {
      stop('The pigz command is not available on this system!')
    }
  } else {
    stop('Platform is not a unix system!')
  }

  if (!is.numeric(n.cores))
    stop("'n.cores' mut be numeric")
  names <- as.character(substitute(list(...)))[-1L]
  if (missing(list) && !length(names))
    warning("nothing specified to be save()d")
  list <- c(list, names)
  if (precheck) {
    ok <- vapply(list, exists, NA, envir = envir)
    if (!all(ok)) {
      n <- sum(!ok)
      stop(sprintf(
        ngettext(n, "object %s not found",
                 "objects %s not found"),
        paste(sQuote(list[!ok]),
              collapse = ", ")
      ), domain = NA)
    }
  }
  on.exit(close(con))
  n.cores.arg <- paste0(' --processes ', n.cores)
  con <- pipe(paste0('pigz ', n.cores.arg, ' > ', file))
  .Internal(saveToConn(list,
                       con,
                       FALSE, # ascii,
                       2,  #version
                       envir,
                       eval.promises))
}

#' Save the current workspace
#' @description save.image.fast () is just a short-cut for ‘save my current workspace’,
#' i.e., save.fast(list = ls(all.names = TRUE), file = ".RData", envir = .GlobalEnv).
#' Files generated with this function can be loaded with core load() function
#' @param file the name of the file where data will be saved
#' @param n.cores number of cores to use to compress
#' @param safe logical. If TRUE, a temporary file is used for creating the saved workspace.
#' The temporary file is renamed to file if the save succeeds. This preserves an existing
#' workspace file if the save fails, but at the cost of using extra disk space during the save.
#' @export save.image.fast
save.image.fast <-
  function(file = ".RData",
           n.cores = 4,
           safe = TRUE) {
    ## Check arguments
    if (is.null(file)) {
      stop("file argument is required")
    }
    if (safe) {
      outfile <- paste0(file, "Tmp")
      i <- 0
      while (file.exists(outfile)) {
        i <- i + 1
        outfile <- paste0(file, "Tmp", i)
      }
    }
    else
      outfile <- file
    on.exit(file.remove(outfile))
    save.fast(
      list = names(.GlobalEnv),
      file = outfile,
      envir = .GlobalEnv,
      n.cores = n.cores,
      precheck = F
    )
    if (safe)
      if (!file.rename(outfile, file)) {
        on.exit()
        stop(gettextf("image could not be renamed and is left in %s",
                      outfile),
             domain = NA)
      }
    on.exit()
  }
