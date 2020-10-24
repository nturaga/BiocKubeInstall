futile.logger::flog.threshold("FATAL", name = "kube_install")

test_that(".trim() works", {

    deps <- list(
        A = list(),
        B = list(),
        C = list("A"),
        D = list("B"),
        E = list("A", "B")
    )

    lib <- tempfile()
    bin <- tempfile()
    param <- BiocParallel::SerialParam()

    result <- with_mock(
        kube_install_single_package = function(...) {},
        kube_install(4L, lib, bin, deps, BPPARAM = param)
    )
    expect_equal(
        result,
        c(A = TRUE, B = TRUE, C = TRUE, D = TRUE, E = TRUE)
    )

    result <- with_mock(
        kube_install_single_package = function(x, ...) if (x %in% "A") stop(),
        kube_install(4L, lib, bin, deps, BPPARAM = param)
    )
    expect_equal(
        result,
        c(A = FALSE, B = TRUE, C = NA, D = TRUE, E = NA)
    )

})

