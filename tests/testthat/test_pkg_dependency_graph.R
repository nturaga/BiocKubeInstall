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

test_that(".pkg_dependencies_*() works", {

    COLNAMES <- c(
        "Package", "Version", "Depends", "Imports", "LinkingTo", "Repository"
    )
    REPOS <- contrib.url(c(
        BiocManager::repositories()[["BioCsoft"]],
        "https://storage.googleapis.com/XXX"
    ))

    .repos_tag <- function(repos) {
        if (length(repos) > 1L) {
            "SOFTWARE"
        } else if (startsWith(repos, "https://storage.googleapis.com")) {
            "BINARY"
        } else {
            stop("unknown repository:\n  ", paste(repos, collapse = "\n  "))
        }
    }

    .mock_available.packages <-
        function(contriburl, method, fileds, type, filters, repos, ...)
    {
        repos_tag <- .repos_tag(repos)
        switch(
            repos_tag,
            SOFTWARE = db_soft,
            BINARY = db_binary,
            stop("unknown tag\n  ", paste(repos, collapse = "\n  "))
        )
    }

    ## pkg_dependencies("_software")
    db_soft <- matrix(
        c(
            "A", "1", "", "", "", REPOS[1],
            "B", "1", "", "", "", REPOS[1],
            "C", "1", "A", "", "", REPOS[1],
            "D", "1", "B", "", "", REPOS[1],
            "E", "1", "A, B", "", "", REPOS[1]
        ), ncol = 6, byrow = TRUE, dimnames = list(LETTERS[1:5], COLNAMES)
    )
    expect_equal(
        with_mock(
            available.packages = .mock_available.packages,
            pkg_dependencies("_software")
        ),
        list(A=character(), B=character(), C="A", D="B", E=c("A", "B"))
    )

    ## pkg_dependencies("_update"), no updates necessar
    db_binary <- matrix(
        c(
            "A", "1", "", "", "", REPOS[2],
            "B", "1", "", "", "", REPOS[2],
            "C", "1", "A", "", "", REPOS[2],
            "D", "1", "B", "", "", REPOS[2],
            "E", "1", "A, B", "", "", REPOS[2]
        ), ncol = 6, byrow = TRUE, dimnames = list(LETTERS[1:5], COLNAMES)
    )
    # expect_equal(
    #     with_mock(
    #         available.packages = .mock_available.packages,
    #         pkg_dependencies("_update", "XXX")
    #     ),
    #     setNames(list(), character())
    # )

    ## pkg_dependencies("_update"), "A" (and so dependencies C, E) out-of-date
    db_binary <- matrix(
        c(
            "A", "0.99", "", "", "", REPOS[2],
            "B", "1", "", "", "", REPOS[2],
            "C", "1", "A", "", "", REPOS[2],
            "D", "1", "B", "", "", REPOS[2],
            "E", "1", "A, B", "", "", REPOS[2]
        ), ncol = 6, byrow = TRUE, dimnames = list(LETTERS[1:5], COLNAMES)
    )
    # expect_equal(
    #     with_mock(
    #         available.packages = .mock_available.packages,
    #         pkg_dependencies(build = "_update", "XXX")
    #     ),
    #     list(A=character(), C="A", E="A")
    # )

    ## pkg_dependencies("_update"), "A" out-of-date, C, E not present
    db_binary <- matrix(
        c(
            "A", "0.99", "", "", "", REPOS[2],
            "B", "1", "", "", "", REPOS[2],
            "D", "1", "B", "", "", REPOS[2]
        ), ncol = 6, byrow = TRUE, dimnames = list(c("A", "B", "D"), COLNAMES)
    )
    # expect_equal(
    #     with_mock(
    #         available.packages = .mock_available.packages,
    #         pkg_dependencies(build = "_update", "XXX")
    #     ),
    #     list(A = character(), C = "A", E = "A")
    # )

    ## pkg_dependencies("_update"), "A" out-of-date, D, E not present
    db_binary <- matrix(
        c(
            "A", "0.99", "", "", "", REPOS[2],
            "B", "1", "", "", "", REPOS[2],
            "C", "1", "A", "", "", REPOS[2]
        ), ncol = 6, byrow = TRUE, dimnames = list(c("A", "B", "C"), COLNAMES)
    )
    # expect_equal(
    #     with_mock(available.packages = .mock_available.packages, {
    #         pkgs <- pkg_dependencies(build = "_update", "XXX")
    #         pkgs[order(names(pkgs))]
    #     }),
    #     list(A = character(), C = "A", D = character(), E = "A")
    # )

})
