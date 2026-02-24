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

RUN apt install esbuild -y

COPY --from=build /mnt /mnt

RUN export folder=$(ls /mnt/dist-newstyle/build/x86_64-linux) && mv /mnt/dist-newstyle/build/x86_64-linux/"$folder"/psb-0.2.0.0/x/psb/build/psb/psb /mnt/psb

ENTRYPOINT ["/mnt/psb", "build"]
