.exclude <-
    function(X, exclude)
{
    exclude <- intersect(exclude, c(names(X), unlist(X, use.names = FALSE)))
    ## remove from deps
    X <- X[!names(X) %in% exclude]
    ## remove satisfied dependencies
    X <- Map(setdiff, X, MoreArgs = list(y = exclude))

    flog.info(
        "%d packages explicitly excluded [.exclude()]",
        length(exclude),
        name = "kube_install"
    )

    X
}

#' @importFrom methods is
#'
#' @importFrom BiocParallel `bpstopOnError<-` `bptasks<-`
.depends_apply <-
    function(X, FUN, ..., exclude = .base_packages(), BPPARAM = NULL)
{
    stopifnot(
        is.list(X),
        !is.null(names(X)),
        is.function(FUN),
        .is_character(exclude),
        is(BPPARAM, "BiocParallelParam")
    )

    if (!is.null(BPPARAM))
        bpstopOnError(BPPARAM) <- FALSE

    result <- rep(NA, length(X))
    names(result) <- names(X)

    X <- .exclude(X, exclude)

    done <- failed <- character()
    n <- 0L
    iter <- 0L

    repeat {
        iter <- iter + 1L
        result[done] <- !done %in% failed
        if (length(X) == 0L || length(X) == n)
            break

        do <- names(X)[lengths(X) == 0L]
        flog.info(
            "%d packages in iteration %d [.depends_apply()]",
            length(do),
            iter,
            name = "kube_install"
        )

        if (is(BPPARAM, "RedisParam"))
            bptasks(BPPARAM) <- length(do)

        ## do the work here
        res <-  bptry(bplapply(do, FUN, ..., BPPARAM = BPPARAM))
        failed_idx <- !bpok(res)
        done <- do
        failed <- do[failed_idx]

        ## LOG ERROR
        err_messages <- vapply(res[failed_idx], conditionMessage, character(1))
        err_text <- sprintf("Package: %s; error: %s", failed, err_messages)
        flog.error(err_text, name = "kube_install")

        ## update X
        n <- length(X)
        X <- .trim(X, done, failed)
    }

    if (length(X))
        flog.error("final dependency graph is is not empty [.depends_apply()]")

    result
}        
