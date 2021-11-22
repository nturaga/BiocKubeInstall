version <- "3.13"
image_name <- 'bioconductor_docker'
workers <- '4'
volume_mount_path <- '/host/'

Sys.setenv(REDIS_HOST = Sys.getenv("REDIS_SERVICE_HOST"))
Sys.setenv(REDIS_PORT = Sys.getenv("REDIS_SERVICE_PORT"))

workers <- as.integer(workers)
artifacts <- BiocKubeInstall:::.get_artifact_paths(version, volume_mount_path)

repos <- BiocKubeInstall:::.repos(version, image_name, cloud_id = 'google')

BiocKubeInstall::kube_wait(workers = workers)

exclude_pkgs = ""
deps <- BiocKubeInstall::pkg_dependencies(
    version,
    build = "_software",
    binary_repo = repos$binary,
    exclude = exclude_pkgs
)

deps0 <- deps[lengths(deps) == 0L]

res <- BiocKubeInstall::kube_install(
    workers = workers,
    lib_path = artifacts$lib_path,
    bin_path = artifacts$bin_path,
    logs_path = artifacts$logs_path,
    deps = deps0[1:10]
)

