.file_move <- function(source, dest, pattern) {
    bins <- list.files(source, pattern = pattern, full.names = TRUE)
    destfiles <- file.path(dest, basename(bins))
    file.rename(bins, destfiles)
}

#' @inheritParams cloud_sync_artifacts
#'
#' @export
local_sync_artifacts <-  function(artifacts, repos) {
    log_file <- file.path(artifacts$logs_path, 'kube_install.log')
    flog.appender(appender.tee(log_file), name = 'kube_install')

    ## Move .out files from bin_path to logs_path
    ## This avoids duplicate copy of PACKAGES* files to contrib(cran path)
    ## and to package_logs
    .output_file_move(artifacts)
    flog.info('Moved .out files to %s: ', artifacts$logs_path,
              name = 'kube_install')

    ## Sync binaries from /host/binary_3_13 to /src/contrib/
    .file_move(artifacts$bin_path, repos$binary, "\\.tar\\.gz$")
    flog.info('Finished moving binaries to local storage: %s',
        artifacts$bin_path, name = 'kube_install')

    ## Sync logs from /host/logs_3_13 to /src/package_logs
    .file_move(artifacts$logs_path, repos$logs, "\\.log$")
    flog.info('Finished moving logs to local storage: %s',
        artifacts$logs_path, name = 'kube_install')
    
    ## Obtain files from kubernetes instance to local
    flog.info("Use 'kubectl cp worker:host/folder ./local/folder' to get files",
        name = 'kube_install')
}