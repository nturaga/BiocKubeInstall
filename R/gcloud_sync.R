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
    function(folder,
             bioc_version,
             secret, public = TRUE,
             bucket = 'bioconductor-packages')
{
    if (!grepl("^gs://", bucket)) {
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
    place_holder <- ".cran_dir"
    res <- file.create(place_holder)

    if (res) {
        ## destination is CRAN style
        destination <- paste(
            bucket, bioc_version, "container-binaries", folder,
            "src/contrib", place_holder,
            sep = "/"
        )

        ## Copy placeholder file folder into CRAN style repo
        gsutil_cp(
            source = place_holder, destination = destination,
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

#' Sync all artifacts to cloud
#'
#' @details Sync packages, logs to cloud storage based on which cloud
#'     is used.
#'
#' @importFrom AnVIL gsutil_rsync gsutil_exists
#'
#' @importFrom futile.logger flog.info flog.appender appender.tee
#'
#' @param secret character() path where secret, i.e a service key for
#'     access to an object store on google or azure.
#'
#' @param artifacts list()
#'
#' @param repos list()
#'
#' @export
cloud_sync_artifacts <-
    function(secret, artifacts, repos)
{
    log_file <- file.path(artifacts$logs_path, 'kube_install.log')
    flog.appender(appender.tee(log_file), name = 'kube_install')

    ## authenticate with secret
    .gcloud_service_account_auth(secret = secret)
    flog.info('Authenticated with object storage', name = 'kube_install')

    ## Move .out files from bin_path to logs_path
    ## This avoids duplicate copy of PACKAGES* files to contrib(cran path)
    ## and to package_logs
    .output_file_move(artifacts)
    flog.info('Moved .out files to %s: ', artifacts$logs_path,
              name = 'kube_install')

    ## Sync binaries from /host/binary_3_13 to /src/contrib/
    ## rsync with .gz (gsutil_rsync exclude option)
    AnVIL::gsutil_rsync(
               source = artifacts$bin_path,
               destination = repos$cran,
               dry = FALSE,
               exclude = ".*out$"
           )
    flog.info('Finished moving binaries to cloud storage: %s',
              artifacts$bin_path, name = 'kube_install')

    ## Sync logs from /host/logs_3_13 to /src/package_logs
    AnVIL::gsutil_rsync(
               source = artifacts$logs_path,
               destination = repos$logs,
               dry = FALSE,
               exclude = ".*tar.gz$"
           )
    flog.info('Finished moving logs to cloud storage: %s',
              artifacts$logs_path, name = 'kube_install')
}
