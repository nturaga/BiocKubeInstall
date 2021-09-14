library(RedisParam)
hostname <- Sys.getenv("REDIS_SERVICE_HOST")
port <- as.integer(Sys.getenv("REDIS_SERVICE_PORT"))
Sys.unsetenv("REDIS_PORT")
job_name <- Sys.getenv('BIOC_REDIS_JOB_NAME')

p <- RedisParam(
    jobname = job_name, is.worker = TRUE,
    redis.hostname = hostname, redis.port = port
)

bpstart(p)
