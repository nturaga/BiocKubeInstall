#' Create a dependency graph for all Bioconductor packages.
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
#' @param binary_repo character() vector of the binary repository in
#'     the form eg. "anvil-rstudio-bioconductor/0.99/3.11"
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
#'     binary_repo = "my-new-binary-bucket/1.0/3.11"
#' )
#' }
#'
#' @importFrom tools package_dependencies
#'
#' @export
pkg_dependencies <-
    function(binary_repo = character())
{
    stopifnot(.is_character(binary_repo))
    ## TODO: make sure function is usable for other clouds
    ## pass argument 'cloud = "gcp"'
    cloud <- "https://storage.googleapis.com/"

    ## use `sprintf()` to produce a zero-length vector if binary_repo
    ## == character()
    binary_repo_url <- sprintf("%s/%s", cloud, binary_repo)

    repos <- c(binary_repo_url, BiocManager::repositories())
    db <- available.packages(repos = repos)
    flog.info(
        "%d packages available from %d repositories",
        nrow(db), length(repos),
        name = "kube_install"
    )

    contrib_url <-
        if (length(binary_repo_url)) {
            contrib.url(binary_repo_url)
        } else {
            character()
        }
    idx <- db[, "Repository"] == contrib_url
    binary_pkgs <- db[idx, , drop = FALSE]

    ## if: Create full set of binaries
    if (nrow(binary_pkgs) == 0) {

        ## software package dependencies
        contrib_url <- contrib.url(BiocManager::repositories()[["BioCsoft"]])
        idx <- db[, "Repository"] == contrib_url
        soft <- rownames(db)[idx]
        flog.info(
            'building %d Bioconductor software packages.',
            length(soft),
            name = "kube_install"
        )
        deps0 <- package_dependencies(soft, db, recursive=TRUE)

        ## FULL dependency graph of non-software dependencies
        other <- setdiff(unlist(deps0, use.names = FALSE), names(deps0))
        deps1 <- package_dependencies(other, db, recursive = TRUE)

        deps <- c(deps0, deps1)
    ## else: Create deps set to be updated
    } else {
        to_update <- .packages_to_update(binary_repo = binary_repo_url)
        deps <- package_dependencies(to_update, db, recursive=FALSE)
        ## Remove dependencies that do not need to be built
        pkgs <- unique(unlist(deps, use.names=FALSE))
        done <- pkgs[!pkgs %in% names(deps)]
        deps <- .trim(deps, done, character())
        flog.info('some Bioconductor packages need to be built.',
                  name = "kube_install")
    }
    flog.info('dependency graph resulted in %d packages to build.',
              length(deps), name = "kube_install")
    deps
}

.base_packages <- function() {
    inst <- installed.packages()
    inst[inst[,"Priority"] %in% "base", "Package"]
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


#' @keywords internal
#'
#' @title Create host directories if they don't exist already
.create_library_paths <-
    function(library_path, binary_path)
{
    flog.appender(appender.file('kube_install.log'), name = 'kube_install')

    if (!file.exists(library_path)) {
        dir.create(library_path, recursive = TRUE)
        flog.info(
            'created library_path: %s', library_path,
            name = "kube_install"
        )
    }

    if (!file.exists(binary_path)) {
        dir.create(binary_path, recursive = TRUE)
        flog.info(
            'created binary_path: %s', binary_path,
            name = "kube_install"
        )
    }
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
#' binary_repo <- "anvil-rstudio-bioconductor-test/0.99/3.11/"
#' .packages_to_update(binary_repo = binary_repo)
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
