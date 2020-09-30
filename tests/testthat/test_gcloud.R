check_gcloud_install <- function() {
    code <- suppressWarning(
        system("gcloud --version", ignore.stderr = TRUE, ignore.stdout = TRUE)
    )

    if (code != 0) {
        skip("'gcloud' SDK is not installed on this node.") 
    }
}


check_gcloud_install <- function() {
    code <- suppressWarning(
        system("gsutil --version", ignore.stderr = TRUE, ignore.stdout = TRUE)
    )

    if (code != 0) {
        skip("'gsutil' is not installed on this node.") 
    }
}

