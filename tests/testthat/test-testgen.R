library(testr)
library(testthat)

context("Generation")

test_that('Test start_capture()', {
    testr::start_capture("stats::dpois")
    dpois(0:7, lambda = 1)
    testr::write_captured_tests("/tmp")
    expect_true(file.exists("tmp"))
    expect_true(file.info("tmp")$isdir)
    expect_true(file.exists("capture/stats___dpois"))
    expect_true(file.info("capture/stats___dpois")$isdir)
    expect_true(file.exists("capture"))
    expect_true(file.info("capture")$isdir)
    expect_true(file.exists("tmp/test.stats.dpois.tar.gz"))
    expect_false(file.info("tmp/test.stats.dpois.tar.gz")$isdir)
    expect_equal(length(list.files("tmp",recursive = TRUE)), 1)
    expect_equal(length(list.files("capture/stats__dpois",recursive = TRUE)), 1)
    unlink("tmp")
    unlink("capture")
})

test_that('Test find_packages_using_function()', {
    x <- testr::find_packages_using_function("cor")
    y <- testr::find_packages_using_function("aksagdK*@&e9dgiakegdkgjqge93yqe")
    expect_true(length(x) > 0)
    expect_true(class(x[1]) == "character")
    expect_true(length(x) == 0)
    expect_true(class(x) == "character")
})

test_that('Generate Abbreviate', {
    expect_warning(generate("abbreviate", "CaptureInfo/capture"))
    generate("abbreviate", "CaptureInfo/capture_abbreviate", verbose = FALSE)
    expect_true(file.exists("abbreviate"))
    expect_true(file.info("abbreviate")$isdir)
    expect_equal(length(list.files("abbreviate")), 2) # one is bad.args file
    sink(tempfile())
    tryCatch(testthat::test_dir(cache$output_dir), error=function(x) {
        result <<- FALSE
        x #invisible(x)
    })
    sink()
    unlink("abbreviate", recursive = T)
})

test_that('Generate Warnings/Errors', {
    expect_warning(generate("we", "CaptureInfo/capture"))
    generate("we", "CaptureInfo/capture_warn_error", verbose = FALSE)
    expect_true(file.exists("we"))
    expect_true(file.info("we")$isdir)
    expect_equal(length(list.files("we",recursive = T)), 3) # one is bad.args file
    sink(tempfile())
    tryCatch(testthat::test_dir(cache$output_dir), error=function(x) {
        result <<- FALSE
        x #invisible(x)
    })
    sink()
    unlink("we", recursive = T)
})

