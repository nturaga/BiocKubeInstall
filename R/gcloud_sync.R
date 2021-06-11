#' @keywords internal
#'
#' @title Authenticate with gcloud service account.
#'
#' @details The function requires a secret service account key file
#'     which is shared with the kubernetes cluster. The service
#'     account secret has minimum requirements to run functionality
#'     for `gsutil mb` and `gsutil rsync`.
#'
#' @param secret character(1) path to the location of the secret key
#'     for the service account.
#'
#' @return `.gcloud_auth()` shows a message on successful
#'     authentication.
#'
#' @importFrom AnVIL gcloud_cmd
.gcloud_service_account_auth <-
    function(secret)
{
    cmd_args <- c('auth', 'activate-service-account',
                  '--key-file', secret)
    ## use gcloud_cmd in AnVIL
    gcloud_cmd(cmd_args)
}


#' @keywords internal
#'
#' @title Validate gsutil URI.
.gsutil_is_uri <-
    function(source)
{
    .is_character(source) & grepl("gs://[^/]+", source)
}


#' @keywords internal
#'
#' @title Make a bucket on the google cloud
#'
#' @param bucket character(1) bucket name for the google storage
#'     bucket.
#'
#' @param uniform_bucket_level_access "-b" Specifies the uniform
#'     bucket-level access setting. Default is "off"
#'
#' @param storage_class "-c" standard: >99.99% in multi-regions and
#'     dual-regions
#'
#' @param location "-l" bucket is created in the location US, which is
#'     multi-region
#'
#' @return `.gcloud_make_bucket()` returns invisibly
#'
.gsutil_make_bucket <-
    function(bucket, uniform_bucket_level_access = FALSE,
             storage_class = "standard", location = "us")
{
    ## Validity checks
    stopifnot(
        .gsutil_is_uri(bucket),
        .is_scalar_logical(uniform_bucket_level_access),
        .is_scalar_character(storage_class),
        .is_scalar_character(location)
    )

    ## create bucket
    args <- c(
        "mb",
        "-b", ifelse(uniform_bucket_level_access, "on", "off"),
        "-c", storage_class,
        "-l", location,
        bucket
    )

    ## use .gsutil_do from AnVIL
    system2("gsutil", args = args)
}


#' Create a CRAN style google bucket with appropriate ACL
#'
#' @details Create a CRAN style bucket used to store the package
#'     binaries built for a specific docker image. The CRAN style
#'     bucket takes is created using the following format,
#'     "bucket_name/production_version/bioc_version/src/contrib". All
#'     buckets created with this function come with Uniform
#'     Bucket-Level Access and are made public. To create a private
#'     bucket, toggle the `public` argument to FALSE. The bucket once
#'     made public is available at the URL provided by google,
#'     `https://storage.googleapis.com/`.
#'
#'     An example location for a package would be,
#'     https://storage.googleapis.com/bioconductor_docker/packages/3.11/bioc/src/contrib/ABAData_1.19.0_R_x86_64-pc-linux-gnu.tar.gz
#'
#' @param bucket character(1) bucket name for the google storage
#'     bucket.
#'
#' @param bioc_version character(1) version number for Bioconductor,
#'     defaults to `BiocManager::version()`.
#'
#' @param secret character(1) path to the location of the secret key
#'     for the service account.
#'
#' @param public logical(1) if the bucket should be publicly
#'     accessible. Defauly is TRUE, if FALSE the bucket is made
#'     private.
#'
#' @return `gcloud_create_cran_bucket()` returns a character vector of
#'     the path of the workspace bucket.
#'
#' @examples
#' \dontrun{
#' gcloud_create_cran_bucket(
#'     bucket = "bioconductor_docker",
#'     bioc_version = "3.11",
#'     secret = "/home/mysecret.json",
#'     public = TRUE
#' )
#' }
#'
#' @importFrom AnVIL gsutil_cp gsutil_exists
#'
#' @export
gcloud_create_cran_bucket <-
    function(bucket,
             bioc_version = as.character(BiocManager::version()),
             secret, public = TRUE)
{
    if(!grepl("^gs://", bucket)) {
        bucket <- paste0("gs://", bucket)
    }

    ## Validity checks
    stopifnot(.gsutil_is_uri(bucket), .is_scalar_logical(public),
              .is_scalar_character(secret), file.exists(secret))

    ## Authenticate
    .gcloud_service_account_auth(secret)

    ## Create a bucket on gcloud
    if (!gsutil_exists(bucket)) {
        .gsutil_make_bucket(bucket, TRUE, "standard", "us")
    }

    ## create CRAN style directory structure
    res <- file.create("PACKAGES")
    if (res) {
        source <- "PACKAGES"
        ## destination is CRAN style
        destination <- paste(
            bucket,'packages', bioc_version, 'bioc',
            "src/contrib/PACKAGES",
            sep = "/"
        )

        ## Copy PACKAGES folder into CRAN style repo
        gsutil_cp(
            source = source, destination = destination,
            recursive = FALSE, parallel = FALSE
        )
    }

    ## Make bucket public
    if (public) {
        cmd_iam <- c("iam", "-r", "ch", "allUsers:objectViewer", bucket)
        system2('gsutil', args = cmd_iam)
    }

    ## Return TRUE if bucket exists on the cloud
    return(gsutil_exists(bucket))
}



#' Sync binaries with Google bucket.
#'
#' @details Sync binaries created by the `kube_install()` function
#'     into the Google bucket which is provided. The google bucket
#'     should be appropriately created in a CRAN style manner with the
#'     correct public permissions.
#'
#' @seealso `BiocKuberInstall::gcloud_create_cran_bucket()`
#'
#' @param src_path character(1) path to the directory where source
#'     information are stored.
#'
#' @param bucket character(1) bucket name for the google storage
#'     bucket, it must include the FULL path of the CRAN style repo.
#'
#' @return `gcloud_sync_to_bucket()` return invisibly, but a message is
#'     shown on screen on successfully tranferred files.
#'
#' @examples
#' \dontrun{
#'
#' gcloud_sync_to_bucket(
#'     src_path = "/host/binaries",
#'     bucket = "bioconductor_docker/packages/3.12/bioc/src/contrib/"
#' )
#'
#' gcloud_sync_to_bucket(
#'     src_path = "/host/logs",
#'     bucket = "bioconductor_docker/packages/3.13/bioc/src/package_logs/"
#' )
#' }
#'
#' @importFrom AnVIL gsutil_rsync gsutil_exists
#'
#' @export
gcloud_sync_to_bucket <-
    function(src_path, bucket)
{
    if(!grepl("^gs://", bucket)) {
        bucket <- paste0("gs://", bucket)
    }

    ## Validity checks
    stopifnot(
        .is_scalar_character(src_path), .gsutil_is_uri(bucket),
        .is_scalar_character(secret), file.exists(secret),
        gsutil_exists(bucket)
    )
    ## Transfer to gcloud
    gsutil_rsync(source = src_path, destination = bucket, dry = FALSE)
}


#'
#' @export
sync_artifacts <-
    function(secret, artifacts, repos)
{
    ## authenticate with secret
    .gcloud_service_account_auth(secret = secret)

    ## Sync binaries from /host/binary_3_13 to /src/contrib/
    gcloud_sync_to_bucket(src_path = artifacts$bin_path, bucket = repos$cran)

    ## Sync logs from /host/logs_3_13 to /src/package_logs
    ## Sync outputs from /host/binary_3_13/*.out to /src/package_logs
    gcloud_sync_to_bucket(src_path = artifacts$logs_path, bucket = repos$logs)
    AnVIL:::gsutil_rm(paste0('gs://',repos$cran, '*.out'))
    AnVIL:::gsutil_cp(paste0(artifacts$bin_path, '/', '*.out'), paste0('gs://', repos$logs))
}
