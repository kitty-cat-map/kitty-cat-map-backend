# kitty-cat-map

This is the repository for the backend of Kitty Cat Map (ねこねこマップ). This
README lists the instructions for building and running locally.

## Running Locally

There are two different ways to run the backend of Kitty Cat Map locally.

The first way is to use Docker (and Docker Compose) to compile and run the
backend. The benefit of this is that you don't need any Haskell tools installed
locally. Everything is built in Docker. As long as you have `docker` installed,
you will be able to easily run the backend. The downside is that Docker builds
generally take a long time. If all you want to is development on the frontend
Android or iOS app, this is the recommended approach.

The second way is to use `stack` to build the backend on your actual machine.
You then run the backend on your actual machine (not in Docker). The benefit of
this is that recompiles are much faster than using `docker`. The downside is
that you need `stack` installed on your machine locally.

### Building with `docker`

This section describes how to use `docker` to build and run the backend.

#### Setup

Install [Docker](https://www.docker.com/)
and [Docker Compose](https://docs.docker.com/compose/). Make sure you can run
commands like `docker version`.

#### Build

The following command will use `docker-compose` to build two Docker containers.
One Docker container is for the PostgreSQL server, and the other container is
the backend itself.

```sh
$ make docker-build
```

If the Haskell code for the backend changes, this command must be rerun to
create a new docker container that has the changes.

#### Run

The following command will use `docker-compose` to run the two Docker
containers:

```sh
$ make docker-up
```

### Building with `stack` locally

This section describes how to use `stack` to build the backend locally.

#### Setup

1.  Install [Docker](https://www.docker.com/)
    and [Docker Compose](https://docs.docker.com/compose/). Make sure you can
    run commands like `docker version`.

2.  Install [`stack`](https://docs.haskellstack.org/en/stable/README/).

3.  Install development libraries for PostgreSQL on your system. On Linux
    systems, these development libraries are usually in a package called
    something like `libpq-dev` or `postgres-dev`. On other systems they may be
    called something else.

#### Build

The following command will use `stack` to build the backend:

```sh
make build
```

#### Run

First, you must run the PostgreSQL database. The following command uses
`docker-compose` to run the database:

```sh
$ make docker-up-pg
```

With the database running, you can run the backend with the following command:

```sh
$ make run
```

## Accessing the Backend

If the backend is running, it can be accessed on the local machine on port 8090.

Here are some examples of using `curl` to access the backend:

-   *upload a new image*

    This endpoint will allow you to upload a new cat image.

    ```sh
    $ curl --verbose \
        --form 'file=@temp-cat-image.jpg' \
        --form 'date=2017-09-10 08:23 Z' \
        --form 'lat=-33' \
        --form 'lon=-100' \
        'http://localhost:8090/v0/image'
    ```

    In order to run this, you need a file in current directory called
    `temp-cat-image.jpg`.

-   *search for images in a given lat/lon*

    This endpoint allows you to search for images within a given min/max
    latitude and logitude.

    ```sh
    $ curl 'http://localhost:8090/v0/search/image/-40/0/-120/0/0'
    ```

-   *get an image*

    This endpoint will allow you to download an existing image.

    ```sh
    $ curl --output 'cat-image.jpg' \
        'http://localhost:8090/v0/image/60475399b11663a107b06a188a795a1e02387535933bd9f5318fa01a1593a6d1.jpg'
    ```

## API Documentation

There are two ways of generating the API documentation, depending on whether you
are using `docker` or not. The following commands should output the API
documenation in markdown format.

Method 1 (with `docker`):

```sh
$ make docker-doc
```

Method 2 (locally with `stack`):

```sh
$ make doc
```

## Other Makefile Targets

There are other targets in the `Makefile` to assist with developement. They
should all be documented, so please read through the `Makefile` to find out what
they do.
