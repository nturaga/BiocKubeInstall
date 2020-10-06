## Startup: Define important variables
library(BiocKubeInstall)


##########
## GLOBALS
##########

## Ports needed for Redis to work
Sys.setenv(REDIS_HOST = Sys.getenv("REDIS_SERVICE_HOST"))
Sys.setenv(REDIS_PORT = Sys.getenv("REDIS_SERVICE_PORT"))

## the parameter parallelism has to match what is on the
## worker-jobs.yaml file.
parallelism <- 6L
lib_path <- "/host/library"
bin_path <- "/host/binaries"
deps_rds <- "pkg_dependencies.rds"
## the 'binary_repository' is where the existing binaries are located.
binary_repo_path <- "anvil-rstudio-bioconductor-test/0.99/3.11/"


##########
## RUN
##########

## Step 1:  Wait till all the worker pods are up and running
BiocKubeInstall::kube_wait(workers = parallelism)

## Step 2 : Create lib_path and bin_path
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
binary_repo_url <- paste0("https://storage.googleapis.com/", binary_repo_path)
to_update <- .packages_to_update(binary_repo = binary_repo_url)
updated_deps <- tools::package_dependencies(to_update, db, recursive=FALSE)


## Step 3: Run kube_install so package binaries are built
inst <- installed.packages(lib.loc = lib_path)
res <- BiocKubeInstall::kube_install(
                            workers = workers,
                            lib_path = lib_path,
                            bin_path = bin_path,
                            deps = deps,
                            inst = inst
                        )
table(unlist(res))


## Step 4: Run sync to google bucket
BiocKubeInstall::gcloud_binary_sync(
                     secret = "/home/rstudio/key.json",
                     bin_path = bin_path,
                     bucket = paste0("gs://", binary_repo)
                 )
