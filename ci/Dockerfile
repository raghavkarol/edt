FROM ubuntu:18.04

# Install dependencies
RUN apt update
RUN apt install -y \
        git        \
        curl       \
        libssl-dev \
        make       \
        automake   \
        autoconf   \
        gcc        \
        libncurses5-dev

# Install kerl to build erlang
RUN curl -o /bin/kerl https://raw.githubusercontent.com/kerl/kerl/master/kerl && chmod +x /bin/kerl
RUN kerl update releases
RUN kerl build 22.0.7
RUN kerl install 22.0.7 /erlang/22.0.7

# Install rebar3
RUN curl -o /bin/rebar3 https://s3.amazonaws.com/rebar3/rebar3 && chmod +x /bin/rebar3

# Update the environment
ENV PATH=/erlang/22.0.7/bin:$PATH

# Install application dependencies
RUN apt install -y inotify-tools
