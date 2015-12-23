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

WORKDIR /erlang

RUN curl -O https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
RUN dpkg -i erlang-solutions_1.0_all.deb
RUN apt-get update && apt-get -y install erlang erlang-base-hipe

WORKDIR /rebar3

RUN curl -O https://s3.amazonaws.com/rebar3/rebar3
RUN chmod a+x ./rebar3
ENV PATH=/rebar3:$PATH

RUN adduser -u 999 --disabled-password --gecos '' me
RUN adduser me sudo
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

RUN mkdir /var/log/hdb
RUN chown -R me:me /var/log/hdb
VOLUME /var/log/hdb

WORKDIR /hdb
COPY ./rebar.config ./rebar.lock ./
RUN rebar3 compile

COPY . ./
RUN chown -R me:me ./

USER me

RUN rebar3 release
RUN ln -s _build/default/rel/hdb/bin/hdb
