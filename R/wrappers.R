#' Wrapper for save.lbzip2
#' @param ... parameters for save.lbzip2
#' @export save.fast
save.fast <- function(...) {
  save.lbzip2(...)
}

#' Wrapper for load.lbzip2
#' @param ... parameters for load.lbzip2
#' @export load.fast
load.fast <- function(...) {
  load.lbzip2(...)
}



#' Save the session in the current working directory with a file name that
#' includes the time stamp and process id
#' @param prefix the prefix to use,for the filename
#' @param compression type of file to save 'gz' or 'lbzip2', default: lbzip2
#' @param n.cores number of cores for save.image.fast or save.image.lbzip2, default = 4
#' @return the name of the file that was saved in
#' @export preserve.state
#' @examples
#' a <- 12345
#' preserve.state()
preserve.state <- function(prefix='savepoint_', compression='lbzip2', n.cores=4) {
    if(compression == 'gz') {
        file <- paste0(prefix,gsub(' ','_',Sys.time()),'_',Sys.getpid(),'.RData')
        save.image.pigz(file, n.cores=n.cores)
    } else if (compression == 'lbzip2') {
        file <- paste0(prefix,gsub(' ','_',Sys.time()),'_',Sys.getpid(),'.RDataFS')
        save.image.lbzip2(file, n.cores=n.cores)
    } else {
        stop(paste0('Unknown compression method: ', compression));
    }
    file
}
