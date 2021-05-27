### Asynchronous services for the MODE shiny application
****

rworker.R defines functions that perform visualization (or other tasks) that can be triggered asynchronously by a shiny application (in this case MODE) using Celery as a task queue and Redis as a message broker.
****

#### Docker container

To build the container, you need to put the following tar.gz binaries ./local_packages next to the Dockerfile:

- trelliscopejs 0.2.2 (trelliscopejs_0.2.2.tar.gz)

The Dockerfile will copy the binary to the appropriate folder inside the container.  Then just run the standard build command:

docker build -t image:tag .

To run the container, we mount two config files, one for minio and one for redis.  Examples can be found in the cfg folder of this repo.

Run command looks like:

docker run -p 5601:5601 -v /some/path/minio_config_svc.yml:/minio_config.yml -v /some/path/redis_config.yml:/redis_config.yml

