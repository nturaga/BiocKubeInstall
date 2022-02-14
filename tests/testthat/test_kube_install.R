futile.logger::flog.threshold("FATAL", name = "kube_install")
futile.logger::flog.threshold("FATAL", name = "kube_progress")

test_that("build dummy pkgs", {
    deps <- list(
        C = "A",
        D = "B",
        E = c("A", "B"),
        F = "E",
        G = NULL
    )
    
    lib_path <- tempdir()
    bin_path <- tempdir()
    logs_path<- tempdir()
    
    ## remove all files in the temp directory
    do.call(file.remove, 
            list(list.files(lib_path, full.names = TRUE, recursive = TRUE))
    )
    
    param <- BiocParallel::SnowParam(2)
    ## No error for all pkgs
    result <- with_mock(
                kube_install_single_package = function(...) {},
                kube_install(lib_path, bin_path, logs_path, deps, 
                             BPPARAM = param)
            )
    expect_identical(result, list())
    
    ## Build with single error
    result <- with_mock(
        kube_install_single_package = 
            function(pkg, ...) if (pkg %in% "A") simpleError(pkg),
        kube_install(lib_path, bin_path, logs_path, deps, 
                     BPPARAM = param)
    )
    expect_identical(sort(names(result)), c("A", "C", "E", "F"))
    
    ## Build with multiple errors
    result <- with_mock(
        kube_install_single_package = 
            function(pkg, ...) if (pkg %in% c("A", "B")) simpleError(pkg),
        kube_install(lib_path, bin_path, logs_path, deps, 
                     BPPARAM = param)
    )
    expect_identical(sort(names(result)), c("A", "B", "C", "D", "E", "F"))
})


test_that("build pkgs", {
    deps0 <- c("agilp","AMOUNTAIN", "ASAFE", "BAC")
    l <- rep(list(NULL), length(deps0))
    names(l) <- deps0
    
    lib_path <- tempdir()
    bin_path <- tempdir()
    logs_path<- tempdir()
    
    ## remove all files in the temp directory
    do.call(file.remove, 
            list(list.files(lib_path, full.names = TRUE, recursive = TRUE))
            )
    
    p <- BiocParallel::SnowParam(2)
    res <- kube_install(lib_path, 
                        bin_path,
                        logs_path,
                        l,
                        p
    )
    
    files <- list.files(lib_path, full.names = TRUE)
    expect_equal(sum(grepl(".out$", files)), length(deps0))
    ## 3 success + 1 final package 
    expect_equal(sum(grepl(".gz$", files)), length(deps0))
})

futile.logger::flog.threshold("INFO", name = "kube_install")
futile.logger::flog.threshold("INFO", name = "kube_progress")