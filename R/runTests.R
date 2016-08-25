
#' @title remove_failing_tcs
#' @description removes the test cases that fail in GNU R
#' @export
remove_failing_tcs <- function()
{
    ## check if generated test cases run without failure
    # get the generated test cases
    capt_dir <- get_capt_dir()
    tc <- get_tests(capt_dir)
    # check which test cases fail?
    res <- sapply(tc, function(x) {
        oldGlobals <- c(ls(.GlobalEnv), "testEnv")
        z = try(source(x, local = .GlobalEnv))
        allGlobals <- ls(.GlobalEnv)
        allGlobals <- allGlobals[!is.element(allGlobals, oldGlobals)]
        rm(list = allGlobals, envir = .GlobalEnv)
        inherits(z, "try-error")
    })
    # remove failing test cases
    unlink(names(res[res == TRUE]))
    get_tests(capt_dir)
}

#' @title find_packages_using_function
#' @description find packages that use the function of interest
#' @param functionName name of the function
#' @param limit max number of packages to use
#' @export
find_packages_using_function <- function(functionName, limit = 100)
{
    top <- c(character())
    res <- c()
    for (path in .libPaths()) {
        call <- paste0("egrep -R -n \'\\<",functionName,"\\>\' ",path)
        res <- c(res, system(call, intern = TRUE) )
    }

    # remove line that dont start with library path
    if(length(res)) {
        for (path in .libPaths()) {
            keep <- grepl(path, res)
            res2 <- res[keep]
            # remove library path
            res2 <- sapply(res2, function(x) {
                strsplit(x[[1]], path)[[1]][2]},
                simplify = TRUE, USE.NAMES = FALSE)
            # select package name from path
            res2 <- sapply(res2, function(x) {
                strsplit(x[[1]], "/")[[1]][2]},
                simplify = TRUE, USE.NAMES = FALSE)
            # make a count table from package name occurence
            if(length(res2) > 0) {
                resTab <- as.data.frame(unclass(rle(sort(res2))))[ , 2:1]
                resTab <- resTab[with(resTab, order(-lengths)), ]
                if (nrow(resTab) < limit)
                    top <- c(top, as.character(resTab$values))
                else
                    top <- c(top, as.character(resTab$values[1:limit]))
            }
        }
    }
    top
}

#' @title runPackageTests
#' @description ectract and run example/test codes from package
#' @param pkg name of the packge
#' @param lib.loc library location
#' @param outDir output directory to store extracted code
#' @export
run_package_tests <- function (pkg, lib.loc = NULL, outDir)
{
    pkgdir <- find.package(pkg)
    owd1    <- setwd(outDir)
    # on.exit(setwd(owd1))

    .createExdotR <- function (pkg, pkgdir, silent = FALSE, use_gct = FALSE, addTiming = FALSE, ..., commentDontrun = TRUE, commentDonttest = TRUE)
    {
        Rfile <- paste0(pkg, "-Ex.R")
        db <- tools::Rd_db(basename(pkgdir), lib.loc = dirname(pkgdir))
        if (!length(db)) {
            message("no parsed files found")
            return(invisible(NULL))
        }
        files <- names(db)
        if (pkg == "grDevices")
            files <- files[!grepl("^(unix|windows)/", files)]
        filedir <- tempfile()
        dir.create(filedir)
        on.exit(unlink(filedir, recursive = TRUE))
        cnt <- 0L
        for (f in files) {
            nm <- sub("\\.[Rr]d$", "", basename(f))
            tools::Rd2ex(db[[f]], file.path(filedir, paste(nm, "R", sep = ".")),
                         defines = NULL, commentDontrun = commentDontrun,
                         commentDonttest = commentDonttest)
            cnt <- cnt + 1L
            if (!silent && cnt%%10L == 0L)
                message(".", appendLF = FALSE, domain = NA)
        }
        nof <- length(Sys.glob(file.path(filedir, "*.R")))
        if (!nof)
            return(invisible(NULL))
        massageExamples <- function (pkg, files, outFile = stdout(), ..., commentDonttest = TRUE, commentDontrun = TRUE)
        {
            if (dir.exists(files[1L])) {
                old <- Sys.setlocale("LC_COLLATE", "C")
                files <- sort(Sys.glob(file.path(files, "*.R")))
                Sys.setlocale("LC_COLLATE", old)
            }
            if (is.character(outFile)) {
                out <- file(outFile, "wt")
                on.exit(close(out))
            }
            else {
                out <- outFile
            }

            lines <- c(
                paste0("pkgname <- \"", pkg, "\""),
                "assign(\"par.postscript\", graphics::par(no.readonly = TRUE))",
                "options(contrasts = c(unordered = \"contr.treatment\", ordered = \"contr.poly\"))",
                "graphics.off()"
            )
            cat(lines, sep = "\n", file = out)
            if (pkg == "tcltk") {
                if (capabilities("tcltk"))
                    cat("require('tcltk')\n", file = out)
                else
                    cat("stop(\"tcltk not found!\")\n", file = out)
            }
            else if (pkg != "base")
                cat("library('", pkg, "')\n", sep = "", file = out)

            for (file in files) {
                nm <- sub("\\.R$", "", basename(file))
                nm <- gsub("[^- .a-zA-Z0-9_]", ".", nm, perl = TRUE, useBytes = TRUE)
                if (pkg == "grDevices" && nm == "postscript") next
                if (pkg == "graphics" && nm == "text") next
                if (!file.exists(file)) stop("file ", file, " cannot be opened", domain = NA)
                lines <- readLines(file)

                patterns <- c("^[[:space:]]*#", "^[[:space:]]*help\\(",
                              "^[[:space:]]*\\?", "^[[:space:]]*help\\.search",
                              "^[[:space:]]*nameEx.{2}help\\.search", "demo\\(",
                              "data\\(package", "data\\(\\)",
                              "^library\\(\\)", "^library\\(lib\\.loc",
                              "^file\\.show\\("
                )

                for (pattern in patterns) {
                    com <- grep(pattern, lines, perl = TRUE, useBytes = TRUE)
                    if (length(com))
                        lines <- lines[-com]
                }

                have_par <- any(grepl("[^a-zA-Z0-9.]par\\(|^par\\(", lines, perl = TRUE, useBytes = TRUE))
                have_contrasts <- any(grepl("options\\(contrasts", lines, perl = TRUE, useBytes = TRUE))
                cat("### * ", nm, "\n\n", sep = "", file = out)
                if (commentDonttest) {
                    dont_test <- FALSE
                    for (line in lines) {
                        if (any(grepl("^[[:space:]]*## No test:", line, perl = TRUE, useBytes = TRUE))) dont_test <- TRUE
                        if (!dont_test) cat(line, "\n", sep = "", file = out)
                        if (any(grepl("^[[:space:]]*## End\\(No test\\)", line, perl = TRUE, useBytes = TRUE))) dont_test <- FALSE
                    }
                }
                if (commentDontrun) {
                    dont_run <- FALSE
                    for (line in lines) {
                        if (any(grepl("^[[:space:]]*## Not run:", line, perl = TRUE, useBytes = TRUE))) dont_test <- TRUE
                        if (!dont_test) cat(line, "\n", sep = "", file = out)
                        if (any(grepl("^[[:space:]]*## End\\(Not run\\)", line, perl = TRUE, useBytes = TRUE))) dont_test <- FALSE
                    }
                }
                else for (line in lines) {
                    if (!(line = "")) {
                        cat(line, "\n", sep = "", file = out)
                    }
                }
                if (have_par) cat("graphics::par(get(\"par.postscript\"))\n", file = out)
                if (have_contrasts) cat("base::options(contrasts = c(unordered = \"contr.treatment\",", "ordered = \"contr.poly\"))\n", sep = "", file = out)
            }
            cat("###### FOOTER ######\n", file = out)
            cat("options(digits = 7L)\n", file = out)
            cat("grDevices::dev.off()\n", file = out)
        }

        massageExamples(pkg, filedir, Rfile, commentDonttest = commentDonttest, ...)
        invisible(Rfile)
    }

    message(gettextf("Testing examples for package %s", sQuote(pkg)),
            domain = NA)
    Rfile <- .createExdotR(pkg, pkgdir, silent = TRUE)
    if (length(Rfile)) {
        for (file in Rfile) {
            print(paste0("@@@@@@@@@@ START EXAMPLE: ", file, " @@@@@@@@@@"))
            oldGlobals <- c(ls(.GlobalEnv), "testEnv")
            tryCatch( source(file, echo = FALSE, local = .GlobalEnv), error = function(e) print(e) )
            allGlobals <- ls(.GlobalEnv)
            allGlobals <- allGlobals[!is.element(allGlobals, oldGlobals)]
            rm(list = allGlobals, envir = .GlobalEnv)
            print(paste0("@@@@@@@@@@ DONE WITH EXAMPLE: ", file, " @@@@@@@@@@"))
        }
    } else {
        warning(gettextf("no examples found for package %s", sQuote(pkg)), call. = FALSE, domain = NA)
    }

    d <- file.path(pkgdir, "tests")
    this <- paste(pkg, "tests", sep = "-")
    unlink(this, recursive = TRUE)
    dir.create(this)
    file.copy(Sys.glob(file.path(d, "*")), this, recursive = TRUE)
    owd2 <- setwd(".")
    setwd(this)
    on.exit(setwd(owd2))
    message(gettextf("Running specific tests for package %s", sQuote(pkg)), domain = NA)
    Rfiles <- dir(".", pattern = "\\.R$")
    if (length(Rfiles)) {
        for (file in Rfiles) {
            print(paste0("&&&&&&& START TEST: ", file, " &&&&&&&"))
            oldGlobals <- c(ls(.GlobalEnv), "testEnv")
            tryCatch( source(file, echo = FALSE, local = .GlobalEnv), error = function(e) print(e) )
            allGlobals <- ls(.GlobalEnv)
            allGlobals <- allGlobals[!is.element(allGlobals, oldGlobals)]
            rm(list = allGlobals, envir = .GlobalEnv)
            print(paste0("&&&&&&& DONE WITH TEST: ", file, " &&&&&&&"))
        }
    }
    invisible(0L)
}


#' @title run_all_tests
#' @description run all the test/example codes from all the selected packages
#' @param outDir output dir
#' @param errorsAreFatal should errors break the process
#' @param scope how to prioritize/select packages to run
#' @param srcdir source directory
#' @param pkg_limit maximum number of packages to use for test generation
#' @param custom_pkg_list custom list of packages of interest to use for test generation
#' @export
run_all_tests <-
    function (outDir = ".", errorsAreFatal = FALSE,
              scope = c("all", "base", "recommended", "top"),
              srcdir = NULL, pkg_limit = NULL,
              custom_pkg_list = NULL)
    {
        ow <- options(warn = 1); on.exit(ow); scope <- match.arg(scope);
        status <- 0L; pkgs <- character();

        known_packages <- tools:::.get_standard_package_names()
        all_avail_packages <- names(installed.packages()[ ,1])
        avail_packages <- all_avail_packages[!is.element(all_avail_packages, c(known_packages$base, known_packages$recommended))]

        pkgs <- c(character(0))

        if (scope %in% c("all", "base"))
            pkgs <- known_packages$base
        if (scope %in% c("all", "recommended"))
            pkgs <- c(pkgs, known_packages$recommended)
        if (scope %in% c("all"))
            pkgs <- c(pkgs, avail_packages)
        if (scope %in% c("top"))
            pkgs <- c(
                do.call(
                    find_packages_using_function,
                    list(functionName = testEnv$fname, limit = testEnv$pkg_limit),
                    envir = testEnv),
                pkgs)
        if (!is.na(custom_pkg_list)) {
            pkgs <- c( custom_pkg_list[ is.element(custom_pkg_list, all_avail_packages) ], pkgs )
        }
        if (pkg_limit > 0)
            pkgs <- pkgs[ 1:pkg_limit ]

        pkgs <- pkgs[ !duplicated(pkgs) ]

        # Sometimes last value is NA
        pkgs <- pkgs[!is.na(pkgs)]
        pkgs <- pkgs[!pkgs == "NA"]

        if (scope %in% c("top") && length(pkgs) < pkg_limit ) {
            pkgs <- c(known_packages$base, known_packages$recommended, avail_packages)
            pkgs <- pkgs[ !duplicated(pkgs) ]
            pkgs <- pkgs[1:pkg_limit]
        }

        if(length(pkgs)) {
            print("Selected packages:")
            print(pkgs)
            for (pkg in pkgs) {
                print(paste0("############ START PACKAGE: ", pkg, " #######"))

                tryCatch(run_package_tests(pkg, .Library, outDir), error = function(e) print(e) )

                print(paste0("############ DONE WITH PACKAGE: ", pkg, " #######"))
            }
        } else {
            print("No packages were selected for example/test code extraction")
        }
        invisible(status)
    }

#' @title get_tests
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

#' @title generate_test_cases
#' @description generates test cases based on environmental variables
#' @import devtools methods
#' @export
generate_test_cases <- function(){
    set_function_name(Sys.getenv("function_name"))
    set_pkg_name(Sys.getenv("package_name"))
    set_job(Sys.getenv("JOB_NAME"))
    set_build(Sys.getenv("BUILD_NUMBER"))
    set_pkg_limit(Sys.getenv("pkg_limit"))
    set_scope(Sys.getenv("scope"))
    set_root(getwd())
    if (as.logical(Sys.getenv("install_testr"))) {
        if(!require(devtools)){
            install.packages("devtools", dependencies = TRUE,
                             repos = "http://cloud.r-project.org/")
            library(devtools)
        }
        install_git("https://github.com/psolaimani/testr.git", branch = "master",
                    upgrade_dependencies = FALSE)
        install_git("https://github.com/bedatadriven/hamcrest.git",
                    branch = "master", upgrade_dependencies = FALSE)
    }
    set_test_out_dir(paste0(testEnv$root, "/", testEnv$job, "_", testEnv$build))
    dir.create(testEnv$testOutDir, recursive = TRUE)
    start_capture( paste(testEnv$pkg_name, "::", testEnv$fname, sep = ""),
                   verbose = TRUE )
    run_all_tests(
        outDir = testEnv$testOutDir, scope = testEnv$scope,
        pkg_limit =  as.numeric(testEnv$pkg_limit),
        custom_pkg_list = testEnv$custom_pkg_list
    )
    setwd(testEnv$root)
    stop_capture_all()
    generate("capture")
    set_capt_dir(file.path(testEnv$root,"capture"))
    set_arch_dir(file.path(testEnv$root,"tests"))
    set_test_dir(file.path(testEnv$root,"capture",paste0(testEnv$pkg_name,"___",
                                                       testEnv$fname)))
    write_captured_tests(testEnv$arch_dir)
}
