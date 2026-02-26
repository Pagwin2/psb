FROM haskell:9.10-slim-bookworm AS build

RUN cabal update

COPY ./psb.cabal /mnt
WORKDIR /mnt
RUN cabal build --only-dependencies

COPY . /mnt

RUN cabal build

WORKDIR /github/workspace

FROM debian:12

RUN mkdir -p /github/workspace

RUN apt update

RUN apt upgrade -y

RUN apt install locales curl -y

# unfortunately debian is eternally outdated so we need to get esbuild from npm
RUN curl -fsSL https://registry.npmjs.org/@esbuild/linux-x64/-/linux-x64-0.27.3.tgz \
    | tar -xz --strip-components=2 -C /usr/local/bin package/bin/esbuild

# If I don't do this then some bit in Haskell throws a fit for BS Locale reasons
RUN locale-gen en_US.UTF-8
ENV LANG=C.UTF-8

COPY --from=build /mnt /mnt

RUN export folder=$(ls /mnt/dist-newstyle/build/x86_64-linux) && mv /mnt/dist-newstyle/build/x86_64-linux/"$folder"/psb-0.2.0.0/x/psb/build/psb/psb /mnt/psb

WORKDIR /github/workspace

ENTRYPOINT ["/mnt/psb", "build"]
