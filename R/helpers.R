#' @keywords interal
.create_artifact_dir <-
    function(version, volume_mount_path, artifact)
{
    ver <- gsub(".", "_", version, fixed = TRUE)

    artifact_path <- paste0(volume_mount_path, paste0(artifact, "_", ver))

    if (!file.exists(artifact_path)) {
        dir.create(artifact_path, recursive = TRUE)
        flog.info(
            'created path: %s', artifact_path,
            name = "kube_install"
        )
    }
    return(artifact_path)
}

#' @keywords internal
.get_artifact_paths <-
    function(version, volume_mount_path)
{
    list(
        lib_path = .create_artifact_dir(version, volume_mount_path, 'library'),
        bin_path = .create_artifact_dir(version, volume_mount_path, 'binary'),
        logs_path = .create_artifact_dir(version, volume_mount_path, 'logs')
    )
}


#' @keywords internal
.repos <-
    function(version, image_name, cloud_id = c('local', 'google', 'azure'))
{

    cloud <- match.arg(cloud_id)

    if (identical(cloud_id, "local")) {
        # temporary location for testing
        opt <- Sys.getenv("BIOCONDUCTOR_BINARY_REPOSITORY",
            Sys.getenv("R_PKG_CACHE_DIR"))
        opt <- getOption("BIOCONDUCTOR_BINARY_REPOSITORY", opt)
        image_name <- opt
    }

    if (cloud == "google") {
        image_name <- paste0('gs://', image_name)
    }

    if (cloud == "azure") {
        image_name <- 'https://bioconductordocker.blob.core.windows.net'
    }

    ## 'binary_repo' is where the existing binaries are located.
    ## 'cran_bucket' is where packages are uploaded on a google bucket
    binary_repo <- paste0(image_name, "/packages/", version, "/bioc/")
    cran_repo <- paste0(binary_repo, "src/contrib/")
    logs_repo <- paste0(binary_repo, "src/package_logs/")

    list(cran = cran_repo, binary = binary_repo, logs = logs_repo)
}


.output_file_move <-
    function(artifacts)
{
    src <- list.files(artifacts$bin_path, full.names = TRUE, pattern = ".out$")
    dest <- paste0(artifacts$logs_path,'/', basename(src))
    file.rename(src, dest)
}

#' Create a CRAN style local repository
#'
#' @param repo The folder that will contain this repository. Can be set via
#'     `BIOCONDUCTOR_BINARY_REPOSITORY` environment variable or option.
#'
#' @inheritParams gcloud_create_cran_bucket
#'
#' @return `local_create_cran_repo` returns a character vector of the path to
#'     the binary repository.
#'
#' @md
#'
#' @examples
#' \dontrun{
#'     local_create_cran_repo(repo = "~/dockerhome/.cache/R-crancache")
#' }
#' @export
local_create_cran_repo <-
    function(repo, bioc_version = as.character(BiocManager::version()))
{
    if (missing(repo)) {
        repo <- Sys.getenv("BIOCONDUCTOR_BINARY_REPOSITORY",
            Sys.getenv("R_PKG_CACHE_DIR"))
        repo <- getOption("BIOCONDUCTOR_BINARY_REPOSITORY", repo)
    }
    destination <- paste(
        repo, 'packages', bioc_version, 'bioc', 'src/contrib',
    )
    if (!dir.exists(destination))
        dir.create(destination)

    destination
}

