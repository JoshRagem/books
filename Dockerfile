FROM ubuntu

RUN apt-get update && apt-get -y install git unzip curl build-essential

WORKDIR /erlang

RUN curl -O https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
RUN dpkg -i erlang-solutions_1.0_all.deb
RUN apt-get update && apt-get -y install erlang erlang-base-hipe

WORKDIR /rebar3

RUN curl -O https://s3.amazonaws.com/rebar3/rebar3
RUN chmod a+x ./rebar3
ENV PATH=/rebar3:$PATH

WORKDIR /lmdb

RUN curl -OL https://github.com/LMDB/lmdb/archive/mdb.master.zip
RUN unzip mdb.master.zip
RUN rm mdb.master.zip

WORKDIR /lmdb/lmdb-mdb.master/libraries/liblmdb
RUN make
RUN make install

WORKDIR /books
COPY ./rebar.config ./rebar.lock ./
RUN rebar3 compile

COPY . ./

RUN rebar3 release
RUN ln -s _build/default/rel/books/bin/books
