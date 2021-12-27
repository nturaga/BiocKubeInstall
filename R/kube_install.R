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
#' @inheritParams kube_install
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
    function(pkg, dry.run, lib_path, bin_path, logs_path)
{
    .libPaths(c(lib_path, .libPaths()))

    log_file <- file.path(logs_path, 'kube_install.log')
    flog.appender(appender.tee(log_file), name = 'kube_install')

    flog.info("building binary for package: %s", pkg, name = 'kube_install')
    cwd <- setwd(bin_path)
    warn_opt <- options(warn = 2)
    on.exit({
        options(warn_opt)
        setwd(cwd)
    })
    if (dry.run) {
        file.create(paste0("test_", pkg))
    } else {
        suppressMessages(
            BiocManager::install(
                             pkg,
                             INSTALL_opts = "--build",
                             update = FALSE,
                             quiet = TRUE,
                             force = TRUE,
                             keep_outputs = TRUE
                         )
        )
    }
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
#'     kubernetes cluster for a specific bioconductor docker
#'     image. The kube_install function can be scaled to a large
#'     cluster to reduce times even further (in theory). Please note
#'     that this command will charge your google billing account,
#'     beware of the charges.
#'
#' @param workers numeric() number of workers in the kubernetes
#'     cluster. It should match the `replicas` argument in the
#'     k8sredis worker-replicaset.yaml file.
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
#'     `.pkg_dependecies()`.
#'
#' @param BPPARAM A `BiocParallelParam` object specifying how each
#'     level of the dependency graph will be parallelized. Use
#'     `SerialParam()` for debugging; `RedisParam()` for use in
#'     kubernetes.
#'
#' @importFrom RedisParam RedisParam
#' @importFrom BiocParallel bplapply bptry bpok
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
    function(workers, lib_path, bin_path, logs_path, deps, dry.run, BPPARAM = NULL)
{
    stopifnot(
        is.integer(workers),
        .is_scalar_character(lib_path),
        .is_scalar_character(bin_path),
        .is_scalar_logical(dry.run)
    )

    if (is.null(BPPARAM)) {
        BPPARAM <- RedisParam(
            workers = workers, jobname = "binarybuild",
            is.worker = FALSE,
            progressbar = TRUE, stop.on.error = FALSE
        )
    }

    ## Logging
    log_file <- file.path(logs_path, 'kube_install.log')
    flog.appender(appender.tee(log_file), name = 'kube_install')

    result <- .depends_apply(
        deps,
        kube_install_single_package,
        dry.run = dry.run,
        lib_path = lib_path,
        bin_path = bin_path,
        logs_path = logs_path,
        BPPARAM = BPPARAM
    )

    flog.info(
        "%d built, %d failed, %d excluded [kube_install()]",
        sum(result, na.rm = TRUE),
        sum(!result, na.rm = TRUE),
        sum(is.na(result)),
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
#' @param version character(), bioconductor version number, e.g 3.12
#'     or 3.13
#'
#' @param image_name character(), name of the image for which binaries
#'     are being built
#'
#' @param workers integer(), number of workers pods in the
#'     k8s cluster
#'
#' @param volume_mount_path character(), path to volume mount
#'
#' @param exclude_pkgs character(), list of packages to exclude
#'
#' @param dry.run logical(1), whether to generate a test run with artificial
#'     artifacts rather than binaries; should be used with `cloud_id = "local"`
#'
#' @md
#'
#' @examples
#' \dontrun{
#'
#' kube_run(version = '3.13',
#'          image_name = 'bioconductor_docker',
#'          workers = '10', volume_mount_path = '/host/')
#'
#' }
#'
#' @export
kube_run <-
    function(version, image_name, workers, depth0 = FALSE,
             volume_mount_path = '/host/',
             cloud_id = c("local", "google", "azure"),
             exclude_pkgs = c('canceR','flowWorkspace',
                              'gpuMagic', 'ChemmineOB'), dry.run = TRUE)
{
    workers <- as.integer(workers)
    artifacts <- .get_artifact_paths(version, volume_mount_path)
    cloud_id <- match.arg(cloud_id)
    repos <- .repos(version, image_name, cloud_id = cloud_id)

    Sys.setenv(REDIS_HOST = Sys.getenv("REDIS_SERVICE_HOST"))
    Sys.setenv(REDIS_PORT = Sys.getenv("REDIS_SERVICE_PORT"))

    ## Step 0: Create a bucket if you need to
    if (identical(cloud_id, "local")) {
        local_create_cran_repo(
            bioc_version = version, volume_mount_path = volume_mount_path
        )
    } else if (identical(cloud_id, "google")) {
        ## Secret key to access bucket on google
        secret <- "/home/rstudio/key.json"

        gcloud_create_cran_bucket(bucket = image_name,
            bioc_version = version, secret = secret, public = TRUE)
    }

    ## Step 1:  Wait till all the worker pods are up and running
    BiocKubeInstall::kube_wait(workers = workers)

    ## Step. 2 : Load deps and installed packages
    deps <- BiocKubeInstall::pkg_dependencies(version,
                                              build = "_software",
                                              binary_repo = repos$binary,
                                              exclude = exclude_pkgs)
    if (depth0)
        deps <- deps[lengths(deps) == 0L]
    ## Step 3: Run kube_install so package binaries are built
    res <- BiocKubeInstall::kube_install(workers = workers,
                                         lib_path = artifacts$lib_path,
                                         bin_path = artifacts$bin_path,
                                         logs_path = artifacts$logs_path,
                                         dry.run = dry.run,
                                         deps = deps)

    if (!identical(cloud_id, "local")) {
        BiocKubeInstall::local_sync_artifacts(
           artifacts = artifacts,
           repos = repos
        )
    } else if (identical(cloud_id, "google")) {
    ##  Step 4: Sync all artifacts produced, binaries, logs
        BiocKubeInstall::cloud_sync_artifacts(
            secret = secret,
            artifacts = artifacts,
            repos = repos
        )
    }

    ## ## Step 5: check if all workers were used
    check <- table(unlist(res))

    check
}
