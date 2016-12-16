FROM debian:jessie

RUN apt update && apt install -y curl unzip build-essential file libc6-dev

COPY ./deb/libclang.list /etc/apt/sources.list.d/
RUN curl http://apt.llvm.org/llvm-snapshot.gpg.key|apt-key add -
RUN apt update && apt install -y libclang-3.9-dev clang-3.9

WORKDIR /tmp/lmdb
RUN curl -LO https://github.com/LMDB/lmdb/archive/mdb.master.zip
RUN unzip mdb.master.zip
WORKDIR /tmp/lmdb/lmdb-mdb.master/libraries/liblmdb/
RUN make install

WORKDIR /opt/rust
RUN curl https://static.rust-lang.org/dist/rust-1.13.0-x86_64-unknown-linux-gnu.tar.gz | tar -xzf - --strip=1
RUN make install
RUN ./install

WORKDIR /opt/rlmdb
RUN USER=docker cargo init
COPY ./Cargo.toml ./build.rs wrapper.h ./
RUN cargo build
RUN mkdir -p /tmp/mdb
COPY . ./
RUN cargo build