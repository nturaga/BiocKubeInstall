.exclude <-
    function(X, exclude)
{
    all <- unique(c(names(X), unlist(X, use.names = FALSE)))
    n0 <- length(all)

    implicit <- intersect(exclude, all)
    ## remove from deps
    X <- X[!names(X) %in% implicit]
    ## remove satisfied dependencies
    X <- Map(setdiff, X, MoreArgs = list(y = implicit))

    flog.info(
        "%d of %d packages excluded",
        length(implicit), n0,
        name = "kube_install"
    )

    X
}

#' @importFrom methods is
#'
#' @importFrom BiocParallel `bpstopOnError<-` `bptasks<-`
.depends_apply <-
    function(X, FUN, ..., BPPARAM = NULL)
{
    stopifnot(
        is.list(X),
        !is.null(names(X)),
        is.function(FUN),
        is(BPPARAM, "BiocParallelParam")
    )
    flog.info(
        "%d packages to process [.depends_apply()]",
        length(X),
        name = "kube_install"
    )

    if (!is.null(BPPARAM))
        bpstopOnError(BPPARAM) <- FALSE

    result <- rep(NA, length(X))
    names(result) <- names(X)

    done <- failed <- character()
    n <- 0L
    iter <- 0L

    repeat {
        result[done] <- !done %in% failed
        if (length(X) == 0L || length(X) == n)
            break

        do <- names(X)[lengths(X) == 0L]
        flog.info(
            "%d packages at depth %s",
            length(do),
            if (iter) paste0("n-", iter) else "n",
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
        iter <- iter + 1L
    }

    if (length(X))
        flog.error("final dependency graph is is not empty [.depends_apply()]")

    result
}
