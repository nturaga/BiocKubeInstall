.include <-
    function(X, include)
{
    all <- unique(c(names(X), unlist(X, use.names = FALSE)))
    n0 <- length(all)

    implicit <- intersect(include, all)
    ## X[i] depends on include...
    idx <- unlist(Map(
        function(x, table) any(x %in% table),
        X,
        MoreArgs = list(table = implicit)
    ))

    ## or X _is_ include
    X0 <- X[idx]
    X <- X[idx | names(X) %in% c(names(X0), unlist(X0, use.names = FALSE))]

    flog.info(
        "%d of %d packages included",
        length(implicit), n0,
        name = "kube_install"
    )

    X
}

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
#' @importFrom BiocParallel `bpstopOnError<-` `bptasks<-` bpstart bpstopOnError
#' @importFrom RedisParam bpstopall
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
    ## Start RedisParam
    bpstart(BPPARAM)
    ## Do work
    flog.info(
        "%d packages at depth %s",
        length(do),
        if (iter) paste0("n-", iter) else "n",
        name = "kube_install"
    )
    ## LOG ERROR
    err_messages <- vapply(res[failed_idx], conditionMessage, character(1))
    err_text <- sprintf("Package: %s; error: %s", failed, err_messages)
    flog.error(err_text, name = "kube_install")
    
    ## Stop RedisParam - This should stop all work on workers
    bpstopall(BPPARAM)

    if (length(X))
        flog.error("final dependency graph is is not empty [.depends_apply()]")

    result
}

fun_factory <- function(FUN, pkg) {
    function(pkg, ...) {
        if (identical(pkg, ".WAITING")) {
            Sys.sleep(1)
            list(pkg = pkg, status = "success")
        } else {
            value <- FUN(pkg, ...)
            if (is(value, "error")) {
                list(pkg = pkg, 
                     status = conditionMessage(value))
            } else {
                list(pkg = pkg, status = "success")
            }
        }
    }
}



dependency_graph_iterator_factory <-
  function(deps, FUN)
  {

    force(FUN)
      
    FUN_ <- fun_factory(FUN, pkg)
    
    ## fast and robust reverse dependencies calculation -- includes
    ## packages with zero reverse dependencies; 0.05s versus 1.85s for
    ## iteration.
    allPackages <- unique(c(unlist(deps, use.names = FALSE), names(deps)))
    packages <- rep(names(deps), lengths(deps))
    dependencies <- factor(unlist(deps, use.names = FALSE), levels = allPackages)
    reverseDependencies <- split(packages, dependencies)
    
    ## calculate the dependence number for each package including
    ## packages with 0 dependencies
    numberOfDependencies <- integer(length(allPackages))
    names(numberOfDependencies) <- allPackages
    numberOfDependencies[names(deps)] <- lengths(deps)
    
    ## queues of packages 'ready' for working, and currently in-progress
    ready <- new.env(parent = emptyenv()) # packages w/ dependencies satisfied
    working <- new.env(parent = emptyenv()) # packages assigned to workers
    
    ## return the next package with all dependencies satisfied,
    ## '.WAITING' if some packages have unmet dependencies, or NULL if
    ## all packages have been returned
    iter <- function() {
      pkg <- head(names(ready), 1L)
      if (length(pkg)) {
        ## remove from the 'ready' queue, add to working, and return
        rm(list = pkg, envir = ready)
        assign(pkg, NULL, working)
        return(pkg)
      }
      
      ## no packages in the 'ready' queue -- recharge
      pkgs <- setdiff(
        names(numberOfDependencies)[numberOfDependencies == 0L],
        names(working)
      )
      if (length(pkgs)) {
        for (pkg in pkgs[-1L]) # add to 'ready' queue
          assign(pkg, NULL, ready)
        assign(pkgs[[1]], NULL, working)
        return(pkgs[[1]])
      }
      
      if (any(numberOfDependencies > 0L)) {
        ## packages need to have dependencies satisfied, but none ready
        return(".WAITING")
      }
      
      return (NULL) # complete
    }
    
    reduce <- function(x, y) {
      pkg <- y$pkg
      status <- y$status
      if (identical(pkg, ".WAITING")) {
        ## no-op
        return(x)
      } 
      ##OBOB remove 'pkg' from 'working' queue
      rm(list = pkg, envir = working)
      ## decrement numberOfDependencies for pkg and all reverse dependencies
      i <- c(pkg, reverseDependencies[[pkg]])
      numberOfDependencies[i] <<- numberOfDependencies[i] - 1L
      ## return the status of the pkg when failed
      if (!identical(status, "success")) {
          msg <- list(status)
          names(msg) <- pkg
          c(x, msg)
      } else {
          x
      }
    }
    
    list(ITER = iter, FUN = FUN_, REDUCE = reduce, this = environment())
  }

