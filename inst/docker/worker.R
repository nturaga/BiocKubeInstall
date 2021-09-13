library(RedisParam)
hostname = Sys.getenv("REDIS_SERVICE_HOST")
port = as.integer(Sys.getenv("REDIS_SERVICE_PORT"))
Sys.unsetenv("REDIS_PORT")

p <- RedisParam(
    jobname = "biocredis", is.worker = TRUE,
    redis.hostname = hostname, redis.port = port
)

bpstart(p)
