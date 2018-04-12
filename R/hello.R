save.fast <- function (...,
                       list = character(),
                       file = stop("'file' must be specified"),
                       envir = parent.frame(),
                       n.cores = 2,
                       eval.promises = TRUE,
                       precheck = TRUE)
{
  if (.Platform$OS.type == "unix") {
    if (system(
      'command -v lbzip2',
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

#' @export save.image.fast
save.image.fast <-
  function(file = ".RData",
           version = NULL,
           verbose = FALSE,
           n.cores = 2,
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
