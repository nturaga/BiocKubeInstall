#' Create a dependency graph for all Bioconductor packages.
#'
#' @rdname pkg_dependencies
#'
#' @name pkg_dependencies
#'
#' @description The function takes in a 'binary_repo' which is a CRAN
#'     style google bucket. It creates a package dependency graph in
#'     the form of a 'list()' while excluding R 'base' packages. The
#'     'binary_repo' needs to be a public google bucket. If you need
#'     to create a new google bucket in a CRAN style structure, see
#'     'gcloud_create_cran_bucket()'. If a newly created bucket is
#'     passed into the function, it will create a full package
#'     dependency structure for all Biconductor packages.
#'
#' @seealso 'gcloud_create_cran_bucket'
#'
#' @importFrom utils available.packages
#'
#' @importFrom tools package_dependencies
#'
#' @importFrom utils contrib.url
NULL

.pkg_dependencies_software <-
    function(version, db, exclude_pkgs)
{
    ## software package dependencies
    contrib_url <- contrib.url(.worker_repositories(version)[["BioCsoft"]])
    idx <- db[, "Repository"] == contrib_url
    software_pkgs <- rownames(db)[idx]
    flog.info(
        '%d software packages available',
        length(software_pkgs),
        name = "kube_install"
    )

    ## The following exluded packages don't build on
    ## bioconductor_docker set of images
    names(exclude_pkgs) <- exclude_pkgs
    if (length(exclude_pkgs)) {
        flog.info(
            '%s software packages manually excluded',
            paste(exclude_pkgs, collapse = ", ")
        )
    }

    ## all software packages
    deps0 <- package_dependencies(software_pkgs, db, recursive = TRUE)

    ## FULL dependency graph of non-software dependencies
    other <- setdiff(unlist(deps0, use.names = FALSE), names(deps0))
    deps1 <- package_dependencies(other, db, recursive = TRUE)

    deps <- c(deps0, deps1)
    ## exclude base
    exclude_base <- .exclude(deps, .base_packages())

    ## exclude manually from the argument 'exclude_pkgs'
    .exclude(exclude_base, exclude_pkgs)
}

.pkg_dependencies_update <-
    function(version, db, binary_repo_url)
{
    stopifnot(
        .is_scalar_character(binary_repo_url)
    )

    contrib_url <- contrib.url(.worker_repositories(version)[["BioCsoft"]])
    idx <- db[, "Repository"] == contrib_url
    db_soft <- db[idx, , drop = FALSE]

    db_binary <- available.packages(repos = binary_repo_url)

    flog.info(
        "%d software and %d binary packages",
        nrow(db_soft), nrow(db_binary),
        name = "kube_install"
    )

    ## new or updateable packages; package names cannot contain '_'
    pkgs_binary <- paste(db_binary[,"Package"], db_binary[, "Version"], sep = "_")
    pkgs_soft <- paste(db_soft[,"Package"], db_soft[, "Version"], sep = "_")
    pkgs0 <- sub("_.*", "", setdiff(pkgs_soft, pkgs_binary))

    ## reverse dependencies of any package already in db_binary need rebuilding
    revdep_pkgs <- intersect(pkgs0, rownames(db_binary))
    revdep <- package_dependencies(
        revdep_pkgs, db, reverse = TRUE, recursive = TRUE
    )
    pkgs1 <- setdiff(
        intersect(unlist(revdep, use.names = FALSE), rownames(db_binary)),
        revdep
    )

    pkgs <- c(pkgs0, pkgs1)
    flog.info(
        "%d new or updated, %d reverse depends packages",
        length(pkgs0), length(pkgs1),
        name = "kube_install"
    )

    ## packages and their dependencies
    deps0 <- package_dependencies(pkgs, db, recursive = TRUE)

    ## FULL dependency graph of old dependencies
    other <- setdiff(unlist(deps0, use.names = FALSE), names(deps0))
    deps1 <- package_dependencies(other, db, recursive = TRUE)

    deps <- c(deps0, deps1)

    ## need only 'pkgs', and dependencies not in the binary repository
    pkgs_all <- union(names(deps), unlist(deps, use.names = FALSE))
    need <- union(pkgs, setdiff(pkgs_all, rownames(db_binary)))
    exclude <- setdiff(
        c(names(deps), unlist(deps, use.names = FALSE)),
        need
    )

    .exclude(deps, c(.base_packages(), exclude))
    }

.pkg_dependencies_timings <- function(version, db, exclude_pkgs) {
    contrib_url <- contrib.url(.worker_repositories(version)[["BioCsoft"]])
    idx <- db[, "Repository"] == contrib_url
    software_pkgs <- rownames(db)[idx]
    flog.info(
        '%d software packages available',
        length(software_pkgs),
        name = "kube_install"
    )

    ## The following exluded packages don't build on
    ## bioconductor_docker set of images
    names(exclude_pkgs) <- exclude_pkgs
    if (length(exclude_pkgs)) {
        flog.info(
            '%s software packages manually excluded',
            paste(exclude_pkgs, collapse = ", ")
        )
    }

    ## all software packages
    deps0 <- package_dependencies(software_pkgs, db, recursive = TRUE)

    ## FULL dependency graph of non-software dependencies
    other <- setdiff(unlist(deps0, use.names = FALSE), names(deps0))
    deps1 <- package_dependencies(other, db, recursive = TRUE)

    deps <- c(deps0, deps1)
    ## exclude base
    exclude_base <- .exclude(deps, .base_packages())

    ## exclude manually from the argument 'exclude_pkgs'
    deps <- .exclude(exclude_base, exclude_pkgs)
# Install times from BBS --------------------------------------------------
    dev <- BiocPkgTools::biocBuildReport(
        version = version, stage.timings = TRUE
    )
    
    dev[["elapsed"]] <- as.numeric(
        vapply(strsplit(dev[["EllapsedTime"]], " "), `[`, character(1L), 1L)
    )
    devdf <- with(dev, dev[stage == "install" & node == "merida1", ])
    times <- devdf[["elapsed"]]
    names(times) <- devdf[["pkg"]]
    timesdf <- stack(times)
    names(timesdf) <- c("install_time_sec", "Package")

# No. of Reverse Dependencies ---------------------------------------------
    # db <- available.packages(repos = BiocManager::repositories())
    ldeps <- tools::package_dependencies(
        names(times), db, reverse=TRUE, recursive = TRUE
    )
    revdeps <- lengths(ldeps)
    rdepsdf <- stack(revdeps)
    names(rdepsdf) <- c("n_rev_deps", "Package")
    
    revdep_times <- merge(timesdf, rdepsdf)
    revdep_times <- revdep_times[with(revdep_times, order(-n_rev_deps, install_time_sec)), ]
    ## NA values get dropped 
    ndeps <- deps[match(revdep_times[["Package"]], names(deps))]
    crans <- deps[setdiff(names(deps), names(ndeps))]
    crans <- crans[names(sort(lengths(crans)))]
    c(ndeps, crans)
}

.pkg_dependencies <-
    function(db, binary_repo_url, pkgs, build)
{
    ## This is surprisingly difficult to do -- the package and its
    ## entire connected component (this is more than just the package
    ## dependencies and reverse dependencies) needs to be considered
    ## for update.
    stop("not yet implemented")
}

#' @rdname pkg_dependencies
#'
#' @param version character() Bioconductor version number.
#'
#' @param build character() One of '_software' (rebuild all packages
#'     in the 'BioCsoft' repository) or '_update' (existing binary
#'     packages in `binary_repo` for which newer versions are
#'     available in 'BioCsoft', and packages in 'BioCsoft' that are
#'     not available in `binary_repo`).
#'
#' @param binary_repo character() vector of the binary repository in
#'     the form eg. "anvil-rstudio-bioconductor/0.99/3.11"
#'
#' @param exclude character() vector of packages to exclude
#'
#' @return 'pkg_dependencies()' returns a list of Bioconductor
#'     packages with the dependencies of the package. If the
#'     'binary_repo' given has a pre-populated set of packages then
#'     only the packages that need to updated are in the list.
#'
#' @examples
#' \dontrun{
#' ## First way, give it a pre-existing binary repository
#' ## hosted as a google bucket.
#' deps <- pkg_dependencies(
#'     binary_repo = "anvil-rstudio-bioconductor/0.99/3.11"
#' )
#'
#' ## Second way, create a new bucket with no packages in it.
#' gcloud_create_cran_bucket(
#'     "gs://my-new-binary-bucket", "1.0",
#'     "3.11", secret = "/home/mysecret.json",
#'      public = TRUE
#' )
#' deps_new <- pkg_dependencies(
#'     "_software",
#'     binary_repo = "my-new-binary-bucket/1.0/3.11"
#' )
#' }
#'
#' @export
pkg_dependencies <-
    function(version, build = c("_software", "_update", "_timings"),
        binary_repo = character(), exclude = character(),
        cloud_id = c("local", "google", "azure"))
{
    build <- match.arg(build)
    stopifnot(
        .is_character(binary_repo)
    )
    ## TODO: make sure function is usable for other clouds
    ## pass argument 'cloud = "gcp"'
    cloud_id <- match.arg(cloud_id)
    cloud <- switch(cloud_id,
        google = "https://storage.googleapis.com",
        local = "file:///host/",
        azure = "https://bioconductordocker.blob.core.windows.net"
    )

    ## use `sprintf()` to produce a zero-length vector if binary_repo
    ## == character()
    binary_repo_url <- sprintf("%s/%s", cloud, binary_repo)

    repos <- .worker_repositories(version)
    db <- available.packages(repos = repos)

    flog.info(
        "%d packages, %d repositories [pkg_dependencies()]",
        nrow(db), length(repos),
        name = "kube_install"
    )

    if (identical(build, "_software")) {
        deps <- .pkg_dependencies_software(version, db, exclude)
    } else if (identical(build, "_update")) {
        deps <- .pkg_dependencies_update(version, db, binary_repo_url)
    } else if (identical(build, "_timings")) {
        deps <- .pkg_dependencies_timings(version, db, exclude) 
    } else {
        ## FIXME: support building arbitrary vector of packages?
        deps <- .pkg_dependencies(version, db, binary_repo_url, build)
    }

    flog.info(
        "%d packages in dependency graph",
        length(deps),
        name = "kube_install"
    )

    deps
}

#' @importFrom utils installed.packages
.base_packages <- function() {
    inst <- installed.packages()
    inst[inst[, "Priority"] %in% "base", "Package"]
}

#' @keywords internal
#'
#' @title Trim dependency graph
.trim <- function(deps, drop, fail) {

    ## remove 'drop' (implicitly, and 'failed') from deps
    deps <- deps[!names(deps) %in% drop]

    ## remove packages with failed dependencies
    n0 <- length(deps)
    deps <- Filter(function(pkg_dep) {
        !any(pkg_dep %in% fail)
    }, deps)
    n_fail_deps <- n0 - length(deps)

    ## remove satisfied dependencies
    deps <- Map(setdiff, deps, MoreArgs = list(y = drop))

    if (length(fail))
        flog.info(
            "%d failed; %d reverse dependencies excluded [.trim()]",
            length(fail), n_fail_deps,
            name = "kube_install"
        )

    deps
}


.worker_repositories <- function(version) {
    repos <- BiocManager::repositories()
    sub("/[[:digit:]\\.]+/", paste0("/",version,"/"), repos)
}
