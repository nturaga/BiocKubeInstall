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

    flog.appender(appender.file('kube_install.log'), name = 'kube_install')
    flog.info("building binary for package: %s", pkg, name = 'kube_install')
    cwd <- setwd(bin_path)
    on.exit(setwd(cwd))
    BiocManager::install(
                     pkg,
                     INSTALL_opts = "--build",
                     update=FALSE,
                     quiet=TRUE
                 )
    ## list(
    ##     "nodename" = Sys.info()[["nodename"]],
    ##     "package" = pkg
    ## )
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
#' @importFrom RedisParam RedisParam
#' @importFrom BiocParallel bplapply bptry bpok
#' @importFrom futile.logger flog.error flog.info flog.appender appender.file
#'
#' @examples
#' \dontrun{
#'
#' ## First method:
#' ## Run with a pre-existing bucket with some packages.
#' ## This will update only the new packages
#' binary_repo <- "anvil-rstudio-bioconductor/0.99/3.11/"
#' deps <- pkg_dependecies(binary_repo = binary_repo)
#' kube_install(
#'     workers = 6L,
#'     lib_path = "/host/library",
#'     bin_path = "/host/binaries",
#'     deps = deps
#' )
#'
#' ## Second method:
#' ## Create a new google CRAN style bucket and populate with binaries.
#' gcloud_create_cran_bucket("gs://my-new-binary-bucket",
#'     "1.0", "3.11", secret = "/home/mysecret.json", public = TRUE)
#'
#' deps_new <- pkg_dependencies(binary_repo = "my-new-binary-bucket/1.0/3.11")
#'
#' kube_install(
#'     workers = 6L,
#'     lib_path = "/host/library",
#'     bin_path = "/host/binaries",
#'     deps = deps_new
#' )
#' }
#'
#' @export
kube_install <-
    function(workers, lib_path, bin_path, deps)
{
    stopifnot(is.integer(workers), .is_scalar_character(lib_path),
              .is_scalar_character(bin_path))

    ## Logging
    flog.appender(appender.file('kube_install.log'), name = 'kube_install')

    ## Create library_path and binary_path
    .create_library_paths(lib_path, bin_path)

    ## Filter 1: drop "base" packages these on the first iteration
    inst <- installed.packages()
    do <- inst[,"Package"][inst[,"Priority"] %in% "base"]
    deps <- deps[!names(deps) %in% do]

    ## Filter 2: Repeated filter of failed packages
    failed_packages <- c()
    repeat {

        deps <- .trim(deps, do)
        do <- names(deps)[lengths(deps) == 0L]
        ## Filter 2: Removing failed packages
        do <- do[!do %in% failed_packages]

        ## Convert do into a named list called "to_install"
        to_install <- as.list(do)
        names(to_install) <- do

        p <- RedisParam::RedisParam(workers = workers, jobname = "demo",
                                    is.worker = FALSE, tasks=length(do),
                                    progressbar = TRUE, stop.on.error = FALSE)

        ## do the work here
        flog.info(
            "RedisParam is going install %d packages in DFS level",
            length(do), name = "kube_install"
        )
        res <-  bptry(bplapply(
            to_install, kube_install_single_package,
            BPPARAM = p,
            lib_path = lib_path,
            bin_path = bin_path
        ))
        ## LOG ERROR
        errs <- res[!bpok(res)]
        err_packages <- names(res)[!bpok(res)]
        failed_packages <- c(failed_packages, err_packages)
        for (err in seq_along(errs)) {
            flog.error("Package: %s", err_packages[err],
                       name = "kube_install")
            flog.error("Error message: %s",
                       conditionMessage(errs[[err]]),
                       name = "kube_install")
        }

        n_old <- length(deps)

        deps <- deps[!names(deps) %in% do]
        ## TODO : Trim out packages that depend on XPS
        ## and packages that don't install
        failed_packages <- c(failed_packages, err_packages)

        if (length(deps) == n_old)
            break
    }

    flog.info("failed to build %d packages", length(deps),
              name = "kube_install")

    ## Create PACKAGES, PACKAGES.gz, PACAKGES.rds
    tools::write_PACKAGES(bin_path, addFiles=TRUE, verbose = TRUE)

    res
}
