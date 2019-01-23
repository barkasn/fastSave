library('fastSave')


check_lbzip2_save_restore <- function() {
    data_orig <- runif(100)
    data_copy <- data_orig
    filename <- 'lbzip2_test_file.RData'
    save.lbzip2(data_copy, file=filename)
    rm(data_copy)
    load.lbzip2(filename)
    unlist(filename)
    all(data_copy == data_orig)
}

    
test_that("can save and restore objects with lbzip2 functions")

