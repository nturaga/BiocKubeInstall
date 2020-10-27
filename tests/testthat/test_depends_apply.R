futile.logger::flog.threshold("FATAL", name = "kube_install")

test_that("'.depends_apply()' works", {

    deps <- list(
        A = list(),
        B = list(),
        C = list("A"),
        D = list("B"),
        E = list("A", "B")
    )
    param <- BiocParallel::SerialParam()

    noop <- setNames(list(), character())
    result <- .depends_apply(noop, identity, BPPARAM = param)
    expect_identical(setNames(logical(), character()), result)
    
    result <- .depends_apply(deps, identity, BPPARAM = param)
    expect_true(all(result))

    result <- .depends_apply(deps, identity, exclude = "A", BPPARAM = param)
    expect_equal(
        c(A = NA, B = TRUE, C = TRUE, D = TRUE, E = TRUE),
        result
    )

    FUN <- function(x, ...)
        if (x == "A") stop("oops")
    result <- .depends_apply(deps, FUN, BPPARAM = param)
    expect_equal(
        c(A = FALSE, B = TRUE, C = NA, D = TRUE, E = NA),
        result
    )

    result <- .depends_apply(deps, FUN, exclude = "B", BPPARAM = param)
    expect_equal(
        c(A = FALSE, B = NA, C = NA, D = TRUE, E = NA),
        result
    )

})
