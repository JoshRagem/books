FROM ubuntu

RUN apt-get update && apt-get -y install git unzip curl build-essential

WORKDIR /lmdb

RUN curl -OL https://github.com/LMDB/lmdb/archive/mdb.master.zip
RUN unzip mdb.master.zip
RUN rm mdb.master.zip

WORKDIR /lmdb/lmdb-mdb.master/libraries/liblmdb
RUN make
RUN make install

WORKDIR /books

COPY . ./

RUN make
