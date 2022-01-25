.fun_factory <- function(FUN, pkg) {
    function(pkg, ...) {
        if (identical(pkg, ".WAITING")) {
            Sys.sleep(1)
            list(pkg = pkg, status = "success")
        } else {
            value <- FUN(pkg, ...)
            if (is(value, "condition")) {
                list(pkg = pkg,
                     status = conditionMessage(value))
            } else {
                list(pkg = pkg, status = "success")
            }
        }
    }
}

.dependency_graph_iterator_factory <-
    function(deps, FUN)
{
    force(FUN)

    FUN_ <- .fun_factory(FUN, pkg)

    ## fast and robust reverse dependencies calculation -- includes
    ## packages with zero reverse dependencies; 0.05s versus 1.85s for
    ## iteration.
    all_packages <- unique(c(unlist(deps, use.names = FALSE), names(deps)))
    packages <- rep(names(deps), lengths(deps))
    dependencies <- factor(
        unlist(deps, use.names = FALSE),
        levels = all_packages
    )
    reverse_deps <- split(packages, dependencies)

    ## calculate the dependence number for each package including
    ## packages with 0 dependencies
    number_of_deps <- integer(length(all_packages))
    names(number_of_deps) <- all_packages
    number_of_deps[names(deps)] <- lengths(deps)

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
            names(number_of_deps)[number_of_deps == 0L],
            names(working)
        )
        if (length(pkgs)) {
            for (pkg in pkgs[-1L]) { # add to 'ready' queue
                assign(pkg, NULL, ready)
            }
            assign(pkgs[[1]], NULL, working)
            return(pkgs[[1]])
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
        i <- c(pkg, reverse_deps[[pkg]])
        number_of_deps[i] <<- number_of_deps[i] - 1L
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
