#' Install and create binaries for R packages.
#'
#' @details The package given by `pkg` is installed in the given
#'     library path `lib_path`, and the binaries are created in the
#'     `bin_path`.
#'
#' @param pkg character() name of R or Bioconductor package.
#'
#' @param lib_path character() path where R package libraries are
#'     stored.
#'
#' @param bin_path character() path where R package binaries are
#'     stored.
#'
#' @examples
#' kube_install_single_package(
#'     pkg = 'AnVIL',
#'     lib_path = "/host/library",
#'     bin_path = "/host/binaries"
#' )
#'
#' @return `kube_install_single_package()` returns invisibly.
#'
#' @importFrom BiocManager install
#'
#' @export
kube_install_single_package <-
    function(pkg, lib_path, bin_path)
{
    .libPaths(c(lib_path, .libPaths()))
    cwd <- setwd(bin_path)
    on.exit(setwd(cwd))
    BiocManager::install(
                     pkg,
                     INSTALL_opts = "--build",
                     update=FALSE,
                     quiet=TRUE
                 )
    Sys.info()[["nodename"]]
}


#' Wait for kubernetes workers
#'
#' @details Wait for the workers to start up. More details on redis
#'     flags here https://redis.io/commands/client-list.
#'
#' @title Wait for worker pods to become active.
#'
#' @param workers integer() number of workers in the kubernetes cluster.
#'
#' @examples
#' \dontrun{
#' kube_wait(workers = 6L)
#' }
#'
#' @importFrom redux hiredis
#' @export
kube_wait <-
    function(workers = as.integer(1))
{
    stopifnot(is.integer(workers))

    redis <- redux::hiredis()
    ## Wait for workers to be ready
    repeat{
        len_workers <- length(
            grep("flags=b", strsplit(redis$CLIENT_LIST(), "\n")[[1]])
        )
        ## Break if the workers number matches.
        if (len_workers == workers)
            break
        ## Sleep till workers come up
        Sys.sleep(1)
    }
    rm(redis)
    gc()
}


#' Install and create binaries for packages parallely using a
#' kubernetes cluster.
#'
#' @description Install packages and create binaries using a
#'     kubernetes cluster for a specific bioconductor docker
#'     image. The kube_install function can be scaled to a large
#'     cluster to reduce times even further (in theory). Please note
#'     that this command will charge your google billing account,
#'     beware of the charges.
#'
#' @param workers numeric() number of workers in the kubernetes
#'     cluster. It should match the `parallelism` argument in the
#'     k8sredis yaml file.
#'
#' @param lib_path character() path where R package libraries are
#'     stored.
#'
#' @param bin_path character() path where R package binaries are
#'     stored.
#'
#' @param deps package dependecy graph as computed by
#'     `.pkg_dependecies()`.
#'
#' @param inst installed packages as computed by
#'     `installed.packages()`.
#'
#' @importFrom RedisParam RedisParam
#' @importFrom BiocParallel bplapply bptry bpok
#' @importFrom futile.logger flog.error flog.info flog.appender appender.file
#'
#' @examples
#' \donttest{
#' deps_rds <- readRDS(system.file("extdata", "pkg_dependencies.rds"))
#' kube_install(
#'     workers = 6L,
#'     lib_path = "/host/library",
#'     bin_path = "/host/binaries",
#'     deps = deps_rds,
#'     inst = installed.packages())
#'}
#'
#' @export
kube_install <-
    function(workers, lib_path, bin_path,
             deps = .pkg_dependecies(),
             inst = installed.packages())
{
    stopifnot(is.integer(workers), .is_scalar_character(lib_path),
              .is_scalar_character(bin_path))

    ## Logging
    flog.appender(appender.file('kube_install.log'), name = 'kube_install')
    
    ## drop "base" packages these on the first iteration
    do <- inst[,"Package"][inst[,"Priority"] %in% "base"]
    deps <- deps[!names(deps) %in% do]

    repeat {
        deps <- .trim(deps, do)
        do <- names(deps)[lengths(deps) == 0L]

        p <- RedisParam::RedisParam(workers = workers, jobname = "demo",
                                    is.worker = FALSE, tasks=length(do),
                                    progressbar = TRUE, stop.on.error = FALSE)

        ## do the work here
        res <-  bptry(bplapply(
            do, kube_install_single_package,
            BPPARAM = p,
            lib_path = lib_path,
            bin_path = bin_path
        ))
        ## LOG ERROR
        errs <- res[!bpok(res)]
        err_packages <- names(res)[!bpok(res)]
        for (err in seq_along(errs)) {
            flog.error(c(
                err_packages[err],
                conditionMessage(errs[err])
            ), name = "kube_install")
        }

        n_old <- length(deps)

        deps <- deps[!names(deps) %in% do]
        ## TODO : Trim out packages that depend on XPS and packages that don't install
        if (length(deps) == n_old)
            break
    }

    flog.info(paste("failed to build:", length(deps), "packages"),
              name = "kube_install")

    ## Create PACKAGES, PACKAGES.gz, PACAKGES.rds
    tools::write_PACKAGES(bin_path, addFiles=TRUE, verbose = TRUE)

    res
}
