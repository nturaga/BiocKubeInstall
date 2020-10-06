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
#' @title Compare binary PACKAGES file to current Bioconductor
#'     PACKAGES file to return list to be updated.
#'
#' @param binary_repo character() vector pointing to binary
#'     repository which has PACKAGES file.
#'
#' @examples
#' \dontrun{
#' repo <- "https://storage.googleapis.com/anvil-rstudio-bioconductor-test/0.99/3.11/"
#' .packages_to_update(binary_repo = repo)
#' }
#'
#' @return `.packages_to_update()` returns character vector of
#'     packages to be updated.
#'
.packages_to_update <-
    function(binary_repo = character())
{
    ## Read bioc and bucket PACKAGES
    bioc_pkgs <- as.data.frame(available.packages(
        repos = BiocManager::repositories()['BioCsoft']
    )[,c('Package', 'Version')])

    binary_pkgs <- as.data.frame(available.packages(
        repos = binary_repo
    )[,c('Package', 'Version')])

    bioc <- paste(bioc_pkgs$Package, bioc_pkgs$Version, sep = "_")
    binary <- paste(binary_pkgs$Package, binary_pkgs$Version, sep = "_")

    ## Compare package and version
    pkg_w_version <- setdiff(bioc, binary)
    ## Return packages to be updated
    gsub("_.*", "", pkg_w_version)
}
