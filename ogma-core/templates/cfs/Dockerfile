FROM ubuntu:22.04

RUN apt-get update
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get install --yes \
      alex \
      cabal-install \
      cmake \
      gcc-multilib \
      ghc \
      git \
      happy \
      libz-dev \
      pkg-config

ENV PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin":$PATH

WORKDIR /root

# Obtain cFS
RUN git clone https://github.com/nasa/cfs.git cfs
WORKDIR /root/cfs
RUN git checkout v6.7.0a
RUN git submodule update --init --recursive

# Copy Copilot app with other cFS apps
ADD copilot /root/cfs/apps/copilot

# Copy default setup, list of CPUs and applications
RUN cp cfe/cmake/Makefile.sample Makefile
RUN cp -r cfe/cmake/sample_defs .

# Enable Ogma-generated cFS app in cFS
RUN sed -i -e 's/\(SET(TGT1_APPLIST.*\))/\1 copilot)/g' sample_defs/targets.cmake
RUN sed -i -e '0,/^!/s//CFE_APP, \/cf\/copilot_cfs.so, COPILOT_AppMain, COPILOT_APP, 50, 16384, 0x0, 0;\n&/' sample_defs/cpu1_cfe_es_startup.scr

RUN make SIMULATION=native prep
RUN make install
