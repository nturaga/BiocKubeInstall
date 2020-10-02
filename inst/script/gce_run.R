## Step 0: Define important variables
library(BiocKubeInstall)

Sys.setenv(REDIS_HOST = Sys.getenv("REDIS_SERVICE_HOST"))
Sys.setenv(REDIS_PORT = Sys.getenv("REDIS_SERVICE_PORT"))

workers <- 6L
lib_path <- "/host/library"
bin_path <- "/host/binaries"
deps_rds <- "pkg_dependencies.rds"
bucket_PACAKGES <- "https://storage.googleapis.com/anvil-rstudio-bioconductor-test/0.99/3.11/src/contrib/PACKAGES"

## Step 0.1:  Wait till all the worker pods are up and running
BiocKubeInstall::kube_wait(workers)

## Step. 1 : Create lib_path and bin_path
BiocKubeInstall:::.create_library_paths(
                      library_path = lib_path,
                      binary_path = bin_path
                  )

## Step. 2 : Load deps and installed packages
## if FULL_INSTALL
if (!file.exists(deps_rds)) {
    deps <- BiocKubeInstall:::.pkg_dependencies()
} else {
    deps <- readRDS(deps_rds)
}
## ELSE find deps for packages which need to be updated


inst <- installed.packages()

res <- BiocKubeInstall::kube_install(
                            workers = workers,
                            lib_path = lib_path,
                            bin_path = bin_path,
                            deps = deps,
                            inst = inst
                        )
## Print jobs each work performed
table(unlist(res))

BiocKubeInstall::gcloud_binary_sync(
                     secret = "/home/rstudio/key.json",
                     bin_path = bin_path,
                     bucket = "gs://anvil-rstudio-bioconductor-test/0.99/3.11/src/contrib/"
                 )
