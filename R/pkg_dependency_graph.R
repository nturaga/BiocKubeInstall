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
        db, recursive=TRUE
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
