

#' Runs all available source code from a package in order to generate
#' usable tests.
#'
#' @param pkg the name of the package (must be installed)
#' @param flist list of functions to instrument for capture
#' @param output.dir directory to write all harnes scripts, tests, and generated content.
run_package <- function(pkg, flist, output.dir = getwd()) {

    cat(sprintf("Package %s:\n", pkg))

    package.dir <- find.package(pkg)
    exScripts <- list.files(file.path(package.dir, "R-ex"), pattern = ".+\\.[RSrs]", full.names = TRUE)
    testScripts <- list.files(file.path(package.dir, "tests"), pattern = ".+\\.[RSrs]", full.names = TRUE)

    pkg.output.dir <- file.path(output.dir, pkg)

    if(dir.exists(pkg.output.dir)) {
        unlink(pkg.output.dir, recursive = TRUE)
    }
    dir.create(pkg.output.dir)

    for(script in c(exScripts, testScripts)) {
        run_package_source(pkg, flist, script, pkg.output.dir)
    }

    validate_tests(file.path(pkg.output.dir, "captured"))
}

#' Executes a script in an independent R session and stores the output in the
#' package working directory.
#'
#' @param pkg the name of the package (must be installed)
#' @param source.file the full path of the source file to execute
#' @param flist list of functions to intrument
#' @param output.dir the output dir to write capture tests and output
run_package_source <- function(pkg, flist, source, output.dir) {


    cat(sprintf("  Running %s... ", basename(source)))

    script <- c(
        "library(testr)",
        sprintf("library(%s)", pkg),
        sprintf("setwd('%s')", dirname(source)),
        sprintf("start_capture(%s)", deparse(flist)),
        sprintf("source('%s', echo = TRUE)", basename(source)),
        sprintf("generate('%s')", file.path(output.dir, "captured"))
    )

    harnessScript <- file.path(output.dir, basename(source))
    writeLines(script, harnessScript)

    scriptOutput = paste(harnessScript, "out", sep=".")
    errorCode <- system2(command = "Rscript", args = c(harnessScript), stdout = scriptOutput, stderr = scriptOutput)

    if(errorCode == 0) {
        cat("OK\n")
    } else {
        cat(sprintf("FAILED: Exited with %d\n", errorCode))
    }
}

#' Attempts to run all generated tests to verify that they're actually correct.
#'
#'
validate_tests <- function(capture.dir) {

    cat(sprintf("  Validating tests... "))

    test.files <- list.files(capture.dir, pattern=".+\\.R", full.names = TRUE, recursive = TRUE)

    ok <- vapply(test.files, FUN.VALUE = logical(1), function(test.file) {

        test.output <- paste0(test.file, ".out")
        exitCode <- system2("Rscript", args = test.file, stdout = test.output, stderr = test.output)
        if(exitCode != 0) {
            file.rename(test.file, paste0(test.file, ".bad"))
        }
        exitCode == 0
    })

    cat(sprintf("%d/%d OK\n", sum(ok), length(test.files)))
}



#' get_tests
#' @description get the names of all generated test cases
#' @param capt_dir location of test cases
#' @export
get_tests <- function(capt_dir)
{
    d <- list.files(capt_dir, pattern = ".R$", recursive = TRUE,
                    full.names = TRUE)
    rm <- grep("^capture", list.files(capt_dir, recursive = TRUE))
    if (length(rm))
        d <- d[-rm]
    d
}

#' generate_test_cases
#'
#' Systematically generates test cases for a given base function by running as many package
#' examples and tests as possible to capture inputs and outputs of functions.
#'
#' If the functions argument is missing, then the list of functions is read from the
#' environment variable FUNCTIONS
#'
#' @description generates test cases of base functions using package sources
#' @import devtools methods
#' @export
generate_test_cases <- function(functions)
{
    # Read from environment if not explicitly provided
    if(missing(functions)) {
        functions <- strsplit(Sys.getenv("FUNCTIONS"), split="\\s*,\\s*")[[1]]
        if(length(functions) == 0) {
            stop("No functions provided. Set the FUNCTIONS environment variable with a comma-delimited list of functions")
        }
    }

    packages <- installed.packages()[, 1]

    for(pkg in packages) {
        run_package(pkg, functions)
    }
}
