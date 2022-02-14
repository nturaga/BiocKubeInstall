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

## Propagate the error to the packages which depend on pkg
.failure_propagation <- function(pkg, failed, reverse_deps, source_pkg = NULL) {
    affected_pkg <- pkg
    ## If source is NULL, pkg is the source of the error
    if (is.null(source_pkg))
        source_pkg <- pkg
    ## recursively propagate the error through the reverse dependances
    if (!exists(pkg, envir = failed, inherits = FALSE)) {
        if (!identical(pkg, source_pkg))
            failed[[pkg]] <- paste0(
                "Unable to build the package due to the dependence failure: ",
                source_pkg
            )
        deps <- reverse_deps[[pkg]]
        for (i in deps) {
            affected_pkg <- append(
                affected_pkg, 
                .failure_propagation(i, failed, reverse_deps, source_pkg = source_pkg)
            )
        }
    }
    affected_pkg
}

.reverse_deps <- function(deps) {
    ## fast and robust reverse dependencies calculation -- includes
    ## packages with zero reverse dependencies; 0.05s versus 1.85s for
    ## iteration.
    all_packages <- unique(c(unlist(deps, use.names = FALSE), names(deps)))
    packages <- rep(names(deps), lengths(deps))
    dependencies <- factor(
        unlist(deps, use.names = FALSE),
        levels = all_packages
    )
    split(packages, dependencies)
}


.fun_factory <- function(FUN, pkg) {
    function(pkg, ...) {
        if (identical(pkg, ".WAITING")) {
            Sys.sleep(5)
            value <- pkg
        } else {
            value <- FUN(pkg, ...)
        }
        
        if (is(value, "condition")) {
            list(pkg = pkg,
                 status = conditionMessage(value))
        } else {
            list(pkg = pkg, status = "success")
        }
    }
}

.dependency_graph_iterator_factory <-
    function(deps, FUN)
{
    force(FUN)

    FUN_ <- .fun_factory(FUN, pkg)

    reverse_deps <- .reverse_deps(deps)

    ## calculate the dependence number for each package including
    ## packages with 0 dependencies
    all_packages <- unique(c(unlist(deps, use.names = FALSE), names(deps)))
    number_of_deps <- integer(length(all_packages))
    names(number_of_deps) <- all_packages
    number_of_deps[names(deps)] <- lengths(deps)

    ## queues of packages 'ready' for working, and currently in-progress
    ready <- new.env(parent = emptyenv()) # packages w/ dependencies satisfied
    working <- new.env(parent = emptyenv()) # packages assigned to workers
    failed <- new.env(parent = emptyenv()) # failed packages

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
            names(number_of_deps)[number_of_deps == 0L],
            names(working)
        )
        if (length(pkgs)) {
            for (pkg in pkgs) { # add to 'ready' queue
                assign(pkg, NULL, ready)
            }
            ## call iter again to obtain the next value
            return(iter())
        }

        if (any(number_of_deps > 0L)) {
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
        if (identical(status, "success")) {
            i <- c(pkg, reverse_deps[[pkg]])
            number_of_deps[i] <<- number_of_deps[i] - 1L
        } else {
            pkgs <- .failure_propagation(pkg, failed, reverse_deps)
            failed[[pkg]] <- status
            number_of_deps[pkgs] <<- - 1L
        }
        ## If the log "kube_progress" is not configured,
        ## it will just print the message on the screen
        flog.info(
            "Total: %d, working: %d, failed/finished: %d",
            length(number_of_deps),
            length(working),
            sum(number_of_deps < 0),
            name = "kube_progress"
        )
        failed
    }

    list(ITER = iter, FUN = FUN_, REDUCE = reduce, this = environment())
}
