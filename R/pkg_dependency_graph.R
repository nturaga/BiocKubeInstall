#' @keywords internal
#'
#' @title Calculate the package dependency graph
#'
#' @importFrom tools package_dependencies
.pkg_dependencies <-
    function(deps_rds = "pkg_dependencies.rds")
{
    db <- available.packages(repos = BiocManager::repositories())
    soft <- available.packages(
        repos = BiocManager::repositories()["BioCsoft"]
    )
    deps0 <- package_dependencies(rownames(soft), db, recursive=TRUE)
    ## return deps
    deps <- package_dependencies(
        union(names(deps0), unlist(deps0, use.names = FALSE)),
        db, recursive=FALSE
    )

    ## save deps_rds for fast reload
    if (!file.exists(deps_rds)) {
        saveRDS(deps, deps_rds)
    }

    deps
}


#' @keywords internal
#'
#' @title Trim dependency graph
.trim <- function(deps, drop) {
    lvls <- names(deps)
    df <- data.frame(
        pkg = factor(rep(names(deps), lengths(deps)), levels = lvls),
        dep = unlist(deps, use.names = FALSE)
    )
    df <- df[!df$dep %in% drop,, drop = FALSE]
    split(df$dep, df$pkg)
}


#' @keywords internal
#'
#' @title Create host directories if they don't exist already
.create_library_paths <-
    function(library_path, binary_path)
{
    if (!file.exists(library_path))
        dir.create(library_path, recursive = TRUE)

    if (!file.exists(binary_path))
        dir.create(binary_path, recursive = TRUE)
}


#' @keywords internal
#'
#' @title Read a PACKAGES file from url
#'
#' @return `.read_PACAKGES(path)` returns the packages and version
#'     number seperated by an '_'.
#'
.read_PACKAGES <-
    function(path)
{
    con <- url(path)
    pkgs <- read.dcf(file = con, all = TRUE)

    ## Return package and version
    paste(pkgs[,1], pkgs[,2], sep = "_")
}


#' @keywords internal
#'
#' @title Compare remote PACKAGES file to current Bioconductor
#'     PACKAGES file to return list to be updated.
#'
#' @return `.diff_PACAKGES(bucket_url)` returns character vector of
#'     packages to be updated.
#'
.diff_PACKAGES <-
    function(bucket_url)
{
    ## Read bioc and bucket PACKAGES
    bioc_pkgs <- .read_PACKAGES(
        file.path(contrib.url(BiocManager::repositories()), "PACKAGES")[[1]]
    )
    bucket_pkgs <- .read_PACKAGES(bucket_url)

    ## Compare package and version
    to_update <- bioc_pkgs[! bioc_pkgs %in% bucket_pkgs]

    ## Return packages to be updated
    gsub("_.*", "", to_update)
}
