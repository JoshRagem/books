FROM ubuntu

RUN apt-get update
RUN apt-get -y install git curl build-essential cmake autoconf libtool zlib1g-dev libssl-dev gdb

WORKDIR /libuv

RUN git clone https://github.com/libuv/libuv.git .
RUN sh autogen.sh
RUN ./configure
RUN make
RUN make install

WORKDIR /h2o

RUN git clone https://github.com/h2o/h2o.git .
RUN cmake .
RUN make libh2o
RUN mv libh2o.a /usr/local/lib
RUN mv include/* /usr/local/include/

WORKDIR /lmdb
RUN git clone https://github.com/lmdb/lmdb.git .
WORKDIR /lmdb/libraries/liblmdb
RUN make
RUN make install

ENV LD_LIBRARY_PATH=/usr/local/lib

RUN apt-get -y install valgrind
WORKDIR /books

COPY . ./
RUN make


EXPOSE 7890
CMD ./books
