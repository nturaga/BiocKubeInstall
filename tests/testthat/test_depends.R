create_test_package <- function(
    pkgpath, description=list(), extraActions=function(path=NULL){}
) {
    canned <- list(Author="Test Author",
        Maintainer="Test Maintainer <test@test.com>", "Authors@R"=NULL)
    for (name in names(description))
    {
        canned[[name]] <- description[[name]]
    }
    path <- file.path(tempdir(), pkgpath)
    unlink(path, recursive=TRUE)
    capture.output({
        suppressMessages(
            usethis::create_package(path, canned, rstudio=FALSE, open = FALSE)
        )
    })

    cat("#", file=file.path(path, "NAMESPACE"))
    extraActions(path)
    path
}

## Dependency Graph
##       A
##      / \
##     B   C
##    / \ /|\
##   D   E F G

pkgA <- create_test_package("packageA",
    description = list(Version = "0.99.0"))
pkgB <- create_test_package("packageB",
    description = list(Version = "0.99.0", Imports = "packageA"))
pkgC <- create_test_package("packageC",
    description = list(Version = "0.99.0", Imports = "packageA"))
pkgD <- create_test_package("packageD",
    description = list(Version = "0.99.0",
    Imports = paste("packageA", "packageB", sep = ", "))
)
pkgE <- create_test_package("packageE",
    description = list(Version = "0.99.0",
    Imports = paste("packageA", "packageB", "packageC", sep = ", "))
)
pkgF <- create_test_package("packageF",
    description = list(Version = "0.99.0",
    Imports = paste("packageA", "packageC", sep = ", "))
)
pkgG <- create_test_package("packageG",
    description = list(Version = "0.99.0",
    Imports = paste("packageA", "packageC", sep = ", "))
)


pkglist <- list(pkgA, pkgB, pkgC, pkgD, pkgE, pkgF, pkgG)
lapply(pkglist, function(x) {
    cmd <- sprintf('"%s"/bin/R CMD build %s', R.home(), x)
    system(cmd, intern=TRUE)
})

current <- list.files(getwd(), pattern = ".tar.gz", full.names = TRUE)
newlocation <- file.path(tempdir(), basename(current))
file.rename(current, newlocation)

lapply(newlocation,  function(x) {
    install.packages(
        x,
        repos = NULL,
        INSTALL_opts = "--build",
        update = FALSE,
        quiet = TRUE,
        force = TRUE,
        keep_outputs = TRUE
    )
})

binaries <- list.files(pattern = "gnu.tar.gz", full.names = TRUE)
bin_path <- tempfile()
dir.create(bin_path)
bin_locs <- file.path(bin_path, basename(binaries))
file.rename(binaries, bin_locs)

bin_path
tools::write_PACKAGES(bin_path, addFiles = TRUE, verbose = TRUE)
## check binaries
dir(bin_path)

read.dcf(file.path(bin_path, "PACKAGES"))

contrib_url <- file.path("file://", bin_path)
db_avail <- available.packages(contriburl = contrib_url)
## hack the available db to report "new" packages
db_avail[, "Version"] <- c(rep("0.99.0", 2), "0.99.1", rep("0.99.0", 2), "0.99.1", "0.99.0")
inst_pkgs <- installed.packages()[grep("package[A-Z]", rownames(installed.packages())), ]

## check for unbuilt packages
old.packages(
    repos = NULL,
    contriburl = contrib_url,
    available = db_avail,
    instPkgs = inst_pkgs,
    checkBuilt = TRUE
)

## build dep graph and see which packages will be rebuilt

