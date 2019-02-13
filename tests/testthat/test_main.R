library('fastSave')


check_lbzip2_save_restore <- function() {
    data_orig <- runif(100)
    data_copy <- data_orig
    filename <- 'lbzip2_test_file.RData'
    save.lbzip2(data_copy, file=filename)
    rm(data_copy)
    load.lbzip2(filename)
    unlink(filename)
    expect_equal(all(data_copy == data_orig),TRUE)
}

check_pigz_save_restore <- function() {
    data_orig <- runif(100)
    data_copy <- data_orig
    filename <- 'pigz_test_file.RData'
    save.pigz(data_copy, file=filename)
    save(data_copy,file=filename)
    rm(data_copy)
    load.pigz(filename)
    unlink(filename)
    expect_equal(all(data_copy == data_orig), TRUE)
}



test_that("can save and restore objects with lbzip2 functions", {check_lbzip2_save_restore()})
test_that("can save and restore object with pigz functions", {check_pigz_save_restore()})
