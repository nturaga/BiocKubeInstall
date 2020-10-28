futile.logger::flog.threshold("FATAL", name = "kube_install")

test_that("'.depends_apply()' works", {

    deps <- list(
        A = character(),
        B = character(),
        C = "A",
        D = "B",
        E = c("A", "B")
    )
    param <- BiocParallel::SerialParam()

    noop <- setNames(list(), character())
    result <- .depends_apply(noop, identity, BPPARAM = param)
    expect_identical(setNames(logical(), character()), result)
    
    result <- .depends_apply(deps, identity, BPPARAM = param)
    expect_true(all(result))

    result <- .depends_apply(.exclude(deps, "A"), identity, BPPARAM = param)
    expect_equal(
        result,
        c(B = TRUE, C = TRUE, D = TRUE, E = TRUE)
    )

    FUN <- function(x, ...)
        if (x == "A") stop("oops")
    result <- .depends_apply(deps, FUN, BPPARAM = param)
    expect_equal(
        result,
        c(A = FALSE, B = TRUE, C = NA, D = TRUE, E = NA)
    )

    result <- .depends_apply(.exclude(deps, "B"), FUN, BPPARAM = param)
    expect_equal(
        result,
        c(A = FALSE, C = NA, D = TRUE, E = NA)
    )

})
