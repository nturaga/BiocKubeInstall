#' @importFrom AnVIL gsutil_ls
#' @importFrom utils available.packages
initialize_paths <-
    function(bioc_version = '3.14', cloud_provider = c('google', 'azure'))
{
    cloud_provider <- match.arg(cloud_provider)
    
    if (cloud_provider == 'google') {
        storage_api <- 'https://storage.googleapis.com/'
    }

    path <- paste0("bioconductor_docker/packages/", bioc_version, "/bioc")
    base <- contrib.url(path)
    contriburl <- paste0(storage_api, base)
    gs <- paste0("gs://", base)
    
    db = utils::available.packages(contriburl)
    ls = AnVIL::gsutil_ls(gs)
    
    pkgs0 <- basename(ls)
    pkgs1 <- strsplit(pkgs0, "_")
    
    ## Are PACKAGES* the same?
    
    PACKAGES <- ls[lengths(pkgs1) == 1L]
    gsutil_stat(PACKAGES)
    
    PACKAGES <- setNames(
        sub("gs://", "https://storage.googleapis.com/", PACKAGES),
        basename(PACKAGES)
    )

    p1 <- read.dcf(url(PACKAGES[["PACKAGES"]]))
    p2 <- read.dcf(gzcon(url(PACKAGES[["PACKAGES.gz"]])))
    p3 <- local({
        download.file(PACKAGES[["PACKAGES.rds"]], fl <- tempfile())
        rds <- readRDS(fl)
        rownames(rds) <- NULL
        rds
    })    

    c(p1, p2, p3)
}


test_identical_packages <-
    function(p1, p2, p3)
{

    identical(p1, p2)
    identical(p1, p3[,colnames(p1)])
    identical(p2, p3[,colnames(p2)])

    return(FALSE)
}




