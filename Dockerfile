FROM debian:jessie

RUN apt update && apt install -y curl unzip build-essential file libc6-dev libclang-dev

ENV LMDBINSTALLDATE=20170409
WORKDIR /tmp/lmdb
RUN curl -LO https://github.com/LMDB/lmdb/archive/mdb.master.zip
RUN unzip mdb.master.zip
WORKDIR /tmp/lmdb/lmdb-mdb.master/libraries/liblmdb/
RUN make install
ENV LD_LIBRARY_PATH=/usr/local/lib

ENV RUSTINSTALLDATE=20170409
WORKDIR /opt/rust
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH=$PATH:/root/.cargo/bin
RUN rustup install nightly
RUN rustup default nightly

WORKDIR /opt/rlmdb
RUN USER=docker cargo init
COPY ./Cargo.toml ./build.rs wrapper.h ./
RUN cargo build
RUN mkdir -p /tmp/mdb
COPY . ./
RUN cargo build
