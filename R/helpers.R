#' @keywords interal
.create_artifact_dir <-
    function(version, volume_mount_path, artifact)
{
    ver <- gsub(".", "_", version, fixed = TRUE)

    artifact_path <- paste0(volume_mount_path, paste0(artifact, "_", ver))

    if (!file.exists(artifact_path)) {
        dir.create(artifact_path, recursive = TRUE)
        flog.info(
            "created path: %s", artifact_path,
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
    function(version, image_name, cloud_id = c('google', 'azure'))
{

    cloud <- match.arg(cloud_id)

    if (cloud == "google") {
        bucket <- paste0("gs://", "bioconductor-packages/")
    }

    if (cloud == "azure") {
        bucket <- "https://bioconductordocker.blob.core.windows.net/"
    }

    ## 'binary_repo' is where the existing binaries are located.
    ## 'cran_bucket' is where packages are uploaded on a google bucket
    binary_repo <- paste0(
        bucket,
        version, "/container-binaries/", image_name
    )
    cran_repo <- paste0(binary_repo, "/src/contrib/")
    logs_repo <- paste0(binary_repo, "/src/package_logs/")

    list(cran = cran_repo, binary = binary_repo, logs = logs_repo)
}


.output_file_move <-
    function(artifacts)
{
    src <- list.files(artifacts$bin_path, full.names = TRUE, pattern = ".out$")
    dest <- paste0(artifacts$logs_path, "/", basename(src))

    file.rename(src, dest)
}
