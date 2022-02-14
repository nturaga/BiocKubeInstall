futile.logger::flog.threshold("FATAL", name = "kube_install")
futile.logger::flog.threshold("FATAL", name = "kube_progress")

test_that("reverse dependences", {
    deps <- list(
        C = "A",
        D = "B",
        E = c("A", "B"),
        F = "E",
        G = NULL
    )
    rev_deps <- .reverse_deps(deps) 
    expect_identical(rev_deps, 
                     list(A = c("C", "E"), 
                          B = c("D", "E"), 
                          E = "F", 
                          C = character(0), 
                          D = character(0), 
                          F = character(0),
                          G = character(0))
                     )
})

test_that("failure_propagation", {
    deps <- list(
        C = "A",
        D = "B",
        E = c("A", "B"),
        F = "E",
        G = NULL
    )
    rev_deps <- .reverse_deps(deps) 
    
    ## Fail a package which has "heavy" reverse dependances
    failed <- new.env(parent = new.env())
    affected <- .failure_propagation("A", failed, rev_deps)
    expect_identical(affected, c("A", "C", "E", "F"))
    expect_identical(sort(names(failed)), c("C", "E", "F"))
    
    ## The other package has already been failed.
    affected <- .failure_propagation("E", failed, rev_deps)
    expect_identical(affected, "E")
    expect_identical(sort(names(failed)), c("C", "E", "F"))
    
    ## Fail a simple package
    failed <- new.env(parent = new.env())
    affected <- .failure_propagation("G", failed, rev_deps)
    expect_identical(affected, c("G"))
    expect_identical(sort(names(failed)), character(0))
})


test_that("package iterator", {
    deps <- list(
        C = "A",
        D = "B",
        E = c("A", "B"),
        F = "E",
        G = NULL
    )
    
    myfun <- function(pkg, failed_list = c()){
        if(pkg %in% failed_list){
            simpleError("I failed")
        }else{
            ## check dependences
            if (!all(deps[[pkg]] %in% success)) {
                stop("Unsatisfied error")
            }
            success <<- c(success, pkg)
            pkg
        }
    }
    
    params <- list(
        SerialParam(),
        SnowParam(2)
    )
    # p <- SerialParam()
    for(p in params){
        ## No failure
        success <- c()
        iter <- .dependency_graph_iterator_factory(
            deps,
            myfun
        )
        res <- bpiterate(
            iter$ITER, iter$FUN,
            REDUCE = iter$REDUCE,
            init = c(), ## need to keep this as initial value for reducer
            BPPARAM = SerialParam()
        )
        expect_identical(sort(names(res)), character(0))
        expect_identical(length(success), 7L)
        
        ## Single failure
        success <- c()
        iter <- .dependency_graph_iterator_factory(
            deps,
            myfun
        )
        res <- bpiterate(
            iter$ITER, iter$FUN,
            failed_list = "F",
            REDUCE = iter$REDUCE,
            init = c(), ## need to keep this as initial value for reducer
            BPPARAM = SerialParam()
        )
        expect_identical(sort(names(res)), "F")
        expect_identical(length(success), 6L)
        
        ## Multiple failure
        success <- c()
        iter <- .dependency_graph_iterator_factory(
            deps,
            myfun
        )
        res <- bpiterate(
            iter$ITER, iter$FUN,
            failed_list = c("A", "B"),
            REDUCE = iter$REDUCE,
            init = c(), ## need to keep this as initial value for reducer
            BPPARAM = SerialParam()
        )
        expect_identical(sort(names(res)), c("A", "B", "C", "D", "E", "F"))
        expect_identical(length(success), 1L)
    }
})

futile.logger::flog.threshold("INFO", name = "kube_install")
futile.logger::flog.threshold("INFO", name = "kube_progress")
