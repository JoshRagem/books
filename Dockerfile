FROM ubuntu

RUN apt-get update
RUN apt-get -y install git curl build-essential cmake autoconf libtool zlib1g-dev libssl-dev

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

WORKDIR /books

COPY . ./
