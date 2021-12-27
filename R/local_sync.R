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

    ## TODO: We need a way to sync /host/binary_3_13 to local machine
    ## Sync binaries from /host/binary_3_13 to /src/contrib/
    ## rsync with .gz (gsutil_rsync exclude option)
    ## AnVIL::gsutil_rsync(
    ##            source = artifacts$bin_path,
    ##            destination = repos$cran,
    ##            dry = FALSE,
    ##            exclude = ".*out$"
    ##        )
    # flog.info('Finished moving binaries to cloud storage: %s',
    #          artifacts$bin_path, name = 'kube_install')

    ## Sync logs from /host/logs_3_13 to /src/package_logs
    ## AnVIL::gsutil_rsync(
    ##            source = artifacts$logs_path,
    ##            destination = repos$logs,
    ##            dry = FALSE,
    ##            exclude = ".*tar.gz$"
    ##        )
    ## flog.info('Finished moving logs to cloud storage: %s',
    ##           artifacts$logs_path, name = 'kube_install')
}