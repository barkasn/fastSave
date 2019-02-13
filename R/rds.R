
#' Save a single R object in RDS format compressed with lbzip2
#' @param object R object to serialize
#' @param file name of the file to save
#' @param refhook a hook function for handling reference objects
#' @param n.cores number of cores to use (default = 4)
#' @export saveRDS.lbzip2
saveRDS.lbzip2 <- function(object, file = "", refhook = NULL, n.cores = 4) {
    if (inherits(file, "connection"))
        stop("'file' must be a filename")
    con <-  pipe(paste0('lbzip2 -n ',n.cores,' > ',file))
    on.exit(close(con))
    saveRDS(object = object, file = con, ascii = FALSE, version = NULL, refhook = refhook)
}

#' Read a single R object in RDS format compressed with lbzip2
#' @param file name of the file to save
#' @param refhook a hook function for handling reference objects
#' @param n.cores number of cores to use (default = 4)
#' @export readRDS.lbzip2
readRDS.lbzip2 <- function(file, refhook = NULL, n.cores = 4 ) {
    if (inherits(file, "connection"))
        stop("'file' must be a filename")
    con <- pipe(paste0('lbunzip2 -c -n ', n.cores, ' ', file))
    on.exit(close(con))
    readRDS(con, refhook = refhook)
}

#' Save a single R object in RDS format compressed with lbzip2
#' @param object R object to serialize
#' @param file name of the file to save
#' @param refhook a hook function for handling reference objects
#' @param n.cores number of cores to use (default = 4)
#' @export saveRDS.pigz
saveRDS.pigz <- function(object, file = "", refhook = NULL, n.cores = 4) {
    if (inherits(file, "connection"))
        stop("'file' must be a filename")
    con <-  pipe(paste0('pigz --processes ',n.cores,' > ',file))
    on.exit(close(con))
    saveRDS(object = object, file = con, ascii = FALSE, version = NULL, refhook = refhook)
}

#' Save a single R object in RDS format that is compressed with pigz
#' @description this function delegates to the built in readRDS and is
#' provided for naming consistency
#' @param ... parameters passed to readRDS
#' @export readRDS.pigz
readRDS.pigz <- function(...) {
    readRDS(...)
}
