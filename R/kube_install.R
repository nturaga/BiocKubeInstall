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
#' binary_install(
#'     pkg = 'AnVIL',
#'     lib_path = "/host/library",
#'     bin_path = "/host/binaries"
#' )
#'
#' @return `binary_install()` returns invisibly.
#'
#' @importFrom BiocManager install
#'
#' @export
binary_install <-
    function(pkg, lib_path, bin_path)
{
    tryCatch({
        .libPaths(c(lib_path, .libPaths()))
        ## Step1: Install
        cwd <- setwd(bin_path)
        on.exit(setwd(cwd))
        BiocManager::install(pkg, INSTALL_opts = "--build", update=FALSE, quiet=TRUE)
        Sys.info()[["nodename"]]
    }, error = function(e) {
        conditionMessage(e)
    })
}

#' @keywords internal
#'
#' @title Wait for worker pods to become active.
#'
#' @param workers integer() number of workers in the kubernetes cluster.
#'
#' @importFrom redux hiredis
.kube_wait <-
    function(workers)
{
    redis <- redux::hiredis()
    ## Wait for workers to be ready
    repeat{
        n <- length(strsplit(redis$CLIENT_LIST(), "\n")[[1]])
        if (n == workers)
            break
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
#'
#' @examples
#' \donttest{
#' deps_rds <- readRDS(system.file("extdata", "pkg_dependencies.rds"))
#' kube_install(
#'     workers = 6L,
#'     lib_path = "/host/library",
#'     bin_path = "/host/binaries",
#'     deps = deps_rds,
#'     inst = installed.packages()
#')
#'}
#'
#' @export
kube_install <-
    function(workers, lib_path, bin_path,
             deps = .pkg_dependecies(),
             inst = installed.packages())
{
    ## drop these on the first iteration
    do <- inst[,"Package"][inst[,"Priority"] %in% "base"]
    deps <- deps[!names(deps) %in% do]

    while (length(deps)) {
        deps <- .trim(deps, do)
        do <- names(deps)[lengths(deps) == 0L]

        p <- RedisParam::RedisParam(workers = workers, jobname = "demo",
                        is.worker = FALSE, tasks=length(do),
                        progressbar = TRUE)

        ## do the work here
        res <- bplapply(
            do, binary_install, BPPARAM = p,
            lib_path = lib_path,
            bin_path = bin_path
        )
        message(length(deps), " " , length(do))
        deps <- deps[!names(deps) %in% do]
    }

    ## Create PACKAGES, PACKAGES.gz, PACAKGES.rds
    tools::write_PACKAGES(bin_path, addFiles=TRUE)

    res
}
