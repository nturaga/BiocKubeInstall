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
#' @param logs_path character() path where R package binary build logs
#'     are stored.
#'
#' @examples
#' \dontrun{
#' kube_install_single_package(
#'     pkg = 'AnVIL',
#'     lib_path = "/host/library",
#'     bin_path = "/host/binaries"
#' )
#' }
#'
#' @return `kube_install_single_package()` returns invisibly.
#'
#' @importFrom BiocManager install
#'
#' @export
kube_install_single_package <-
    function(pkg, lib_path, bin_path, logs_path)
{
    .libPaths(c(lib_path, .libPaths()))

    log_file <- file.path(logs_path, 'kube_install.log')
    flog.appender(appender.tee(log_file), name = 'kube_install')

    flog.info("building binary for package: %s", pkg, name = 'kube_install')
    cwd <- setwd(bin_path)
    on.exit(setwd(cwd))
    
    result <- pkg
    withCallingHandlers({
        suppressMessages(
            BiocManager::install(
                             pkg,
                             INSTALL_opts = "--build",
                             update = FALSE,
                             quiet = TRUE,
                             force = TRUE,
                             ## TODO: a successful install output isn't useful
                             keep_outputs = TRUE 
                         )
        )
        },
        error = function(e) {
            flog.error("Error: package %s failed", pkg, name = "kube_install")
            print(conditionMessage(e))
            result <<- e
        },
        warning = function(e) {
            result <<- e
            tryInvokeRestart("muffleWarning")
        }
    )
    result
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
    repeat {
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
#'     BiocParallelParam for a specific bioconductor docker
#'     image. The kube_install function can be scaled to a large
#'     cluster to reduce times even further (in theory). Please note
#'     that this command will charge your google billing account,
#'     beware of the charges.
#'
#' @param lib_path character() path where R package libraries are
#'     stored.
#'
#' @param bin_path character() path where R package binaries are
#'     stored.
#'
#' @param logs_path character() path where R package binary build logs
#'     are stored.
#'
#' @param deps package dependecy graph as computed by
#'     `.pkg_dependencies()`.
#'
#' @param BPPARAM A `BiocParallelParam` object specifying how each
#'     level of the dependency graph will be parallelized. Use
#'     `SerialParam()` for debugging; `RedisParam()` for use in
#'     kubernetes.
#'
#' @importFrom RedisParam RedisParam
#' @importFrom BiocParallel bpiterate
#' @importFrom futile.logger flog.error flog.info flog.appender
#'     appender.file appender.tee
#'
#' @examples
#' \dontrun{
#'
#' ## First method:
#' ## Run with a pre-existing bucket with some packages.
#' ## This will update only the new packages
#' binary_repo <- "anvil-rstudio-bioconductor/0.99/3.11/"
#' deps <- pkg_dependencies(binary_repo = binary_repo)
#' kube_install(
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
    function(lib_path, bin_path, logs_path,
             deps, BPPARAM = NULL)
{
    stopifnot(
        .is_scalar_character(lib_path),
        .is_scalar_character(bin_path),
        .is_scalar_character(logs_path)
    )

    ## Only if BPPARAM is null, use SnowParam
    if (is.null(BPPARAM)) {
        BPPARAM <- BiocParallel::SnowParam()
    }

    ## Logging
    log_file <- file.path(logs_path, 'kube_install.log')
    flog.appender(appender.tee(log_file), name = 'kube_install')
    flog.info(
        "%d packages to process ",
        length(deps),
        name = "kube_install"
    )

    ## Iterator function
    iter <- .dependency_graph_iterator_factory(
        deps,
        kube_install_single_package
    )

    result <- bpiterate(
        iter$ITER, iter$FUN,
        lib_path = lib_path,
        bin_path = bin_path,
        logs_path = logs_path,
        REDUCE = iter$REDUCE,
        init = c(), ## need to keep this as initial value for reducer
        BPPARAM = BPPARAM
    )

    ## Logging to document how many packages failed and installed
    ## TRUE is success, FALSE is fail
    ## TODO: try to log exluded packages like canceR, and ChemmineOB
    flog.info(
        "%d built, %d succeeded, %d failed",
        length(deps),
        length(deps) - length(result),
        length(result),
        name = "kube_install"
    )

    ## Create PACKAGES, PACKAGES.gz, PACAKGES.rds
    tools::write_PACKAGES(bin_path, addFiles = TRUE, verbose = TRUE)
    flog.info("PACKAGES files created", name = "kube_install")

    result
}


#' Run builder on k8s
#'
#' @description Run binary installation on k8s cluster
#'
#' @param bioc_version character(), bioconductor bioc_version number, e.g 3.12
#'     or 3.13
#'
#' @param image_name character(), name of the image for which binaries
#'     are being built
#'
#' @param volume_mount_path character(), path to volume mount
#'
#' @param exclude_pkgs character(), list of packages to exclude
#'
#' @importFrom RedisParam RedisParam bpstopall
#' @examples
#' \dontrun{
#'
#' kube_run(bioc_version = '3.14',
#'          image_name = 'bioconductor_docker',
#'          volume_mount_path = '/host/',
#'          exclude_pkgs = c('canceR'))
#' }
#'
#' @export
kube_run <-
    function(bioc_version, image_name,
             volume_mount_path = '/host/',
             exclude_pkgs = character())
{
    artifacts <- .get_artifact_paths(bioc_version, volume_mount_path)
    repos <- .repos(bioc_version,image_name, cloud_id = 'google')
    
    Sys.setenv(REDIS_HOST = Sys.getenv("REDIS_SERVICE_HOST"))
    Sys.setenv(REDIS_PORT = Sys.getenv("REDIS_SERVICE_PORT"))

    ## Secret key to access bucket on google
    ## PAIN point 1: Also not needed
    secret <- "/home/key.json"

    ## Step 0: Create a bucket if you need to
    ## PAIN POINT 2: Creation of new buckets
    ## Do it via github actions
    gcloud_create_cran_bucket(folder = image_name,
                              bioc_version = bioc_version,
                              secret = secret, public = TRUE)

    ## Step. 2 : Load deps and installed packages
    ## remove exclude packages
    deps <- pkg_dependencies(
                bioc_version, build = "_software",
                binary_repo = repos$binary,
                exclude = exclude_pkgs
    )

    ## Step 3: Run kube_install so package binaries are built
    BPPARAM <- RedisParam(
        jobname = "binarybuild", is.worker = FALSE,
        progressbar = TRUE, stop.on.error = FALSE
    )

    res <- kube_install(
                lib_path = artifacts$lib_path, 
                bin_path = artifacts$bin_path,
                logs_path = artifacts$logs_path,
                deps = deps, BPPARAM = BPPARAM
    )

    ## Stop RedisParam - This should stop all work on workers
    bpstopall(BPPARAM)

    ##  Step 4: Sync all artifacts produced, binaries, logs
    ## PAIN POINT 3: Remove from this function - all sync goes to Github actions
    BiocKubeInstall::cloud_sync_artifacts(
        secret = secret,
        artifacts = artifacts,
        repos = repos
    )

    ## ## Step 5: check if all workers were used
    check <- table(unlist(res))

    check
}
