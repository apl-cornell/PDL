FROM --platform=linux/amd64 ubuntu:20.04

# Set noninteractive mode to avoid prompts during package install
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential pkg-config git curl gnupg \
    openjdk-8-jdk \
    zlib1g-dev iverilog autoconf gperf flex bison tcl-dev \
    ghc libghc-regex-compat-dev libghc-syb-dev libghc-old-time-dev libghc-split-dev

# Install sbt and scala
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" > /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" > /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | tee /etc/apt/trusted.gpg.d/sbt.asc && \
    apt-get update && \
    apt-get install -y sbt

# Clone and install Bluespec
WORKDIR /opt
RUN git clone --recursive https://github.com/B-Lang-org/bsc.git && \
    cd bsc && \
    make install-src

# Set environment variables for PDL
ENV BLUESPECDIR=/opt/bsc
ENV BSCPATH=$BLUESPECDIR/inst/bin
ENV PDLDIR=/opt/pdl
ENV PDLPATH=$PDLDIR/bin
ENV PATH=$BSCPATH:$PDLPATH:$PATH

# Clone and build PDL
RUN git clone --recursive https://github.com/apl-cornell/PDL.git /opt/pdl && \
    cd /opt/pdl && \
    git checkout exn && \
    make

# Set default workdir
WORKDIR /opt/pdl

CMD ["bash"]

