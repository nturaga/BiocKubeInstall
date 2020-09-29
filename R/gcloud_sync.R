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
.gcloud_auth <-
    function(secret)
{
    cmd_args <- c('auth', 'activate-service-account',
                  '--key-file', secret)
    system2('gcloud', args = cmd_args)
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
#'     https://storage.googleapis.com/bucket-name/0.99/3.11/src/contrib/ABAData_1.19.0_R_x86_64-pc-linux-gnu.tar.gz
#'
#' @param bucket character(1) bucket name for the google storage
#'     bucket.
#'
#' @param production_version characater(1) version number for the
#'     bucket with the binaries.
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
#' \donttest{
#' gcloud_create_cran_bucket(
#'     bucket = "bioconductor-docker-test",
#'     production_version = "0.99",
#'     bioc_version = "3.11",
#'     secret = "/home/mysecret.json",
#'     public = TRUE
#' )
#' }
#' @export
gcloud_create_cran_bucket <-
    function(bucket,
             production_version,
             bioc_version = as.character(BiocManager::version()),
             secret,
             public = TRUE)
{
    return(NA)
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
#' @param bin_path character(1) path to the directory where binaries
#'     are stored.
#'
#' @param bucket character(1) bucket name for the google storage
#'     bucket.
#'
#' @param secret character(1) path to the location of the secret key
#'     for the service account.
#'
#' @return `gcloud_binary_sync()` return invisibly, but a message is
#'     shown on screen on successfully tranferred files.
#'
#' @examples
#' \donttest{
#' gcloud_binary_sync(
#'     bin_path = "/host/binaries",
#'     bucket = "gs://bucket-name/0.99/3.11/src/contrib/"
#'     secret = "/home/rstudio/key.json"
#' )
#' }
#' @importFrom AnVIL gsutil_rsync
#'
#' @export
gcloud_binary_sync <-
    function(bin_path, bucket, secret = "/home/rstudio/key.json")
{
    ## authenticate with secret
    .gcloud_auth(secret = secret)

    ## Transfer to gcloud
    AnVIL::gsutil_rsync(
               source = bin_path,
               destination = bucket,
               dry=FALSE
           )
}
