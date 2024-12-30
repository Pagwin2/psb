FROM haskell

RUN mkdir -p /github/workspace

RUN cabal update

COPY ./psb.cabal /mnt
WORKDIR /mnt
RUN cabal build --only-dependencies

COPY . /mnt

RUN cabal build

WORKDIR /github/workspace

ENTRYPOINT ["/mnt/dist-newstyle/build/x86_64-linux/ghc-9.4.8/psb-0.1.0.0/x/psb/build/psb/psb", "build", "-p2"]
