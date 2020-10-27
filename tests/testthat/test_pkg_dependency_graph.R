futile.logger::flog.threshold("FATAL", name = "kube_install")

test_that(".trim() works", {

    ## toy dependency graph
    deps <- list(
        A = list(),
        B = list(),
        C = list("A"),
        D = list("B"),
        E = list("A", "B")
    )

    ## no-ops
    expect_equal(.trim(deps, character(), character()), deps)
    expect_equal(.trim(deps, "F", character()), deps)
    expect_equal(.trim(deps, "F", "F"), deps)
    expect_equal(.trim(list(), character(), character()), list())

    expect_equal(
        .trim(deps, "A", character()),
        list(B = list(), C = list(), D = list("B"), E = list("B"))
    )
    expect_equal(
        .trim(deps, c("A", "B"), character()),
        list(C = list(), D = list(), E = list())
    )
    expect_equal(
        .trim(deps, "A", "A"),
        list(B = list(), D = list("B"))
    )
    expect_equal(
        .trim(deps, c("A", "B"), "A"),
        list(D = list())
    )
    expect_equal(
        .trim(deps, c("A", "B"), c("A", "B")),
        setNames(nm = list())
    )

})
