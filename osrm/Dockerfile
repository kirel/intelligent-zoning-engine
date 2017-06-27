FROM kirel/osrm-backend-docker:ize

RUN apt-get update && apt-get install -y software-properties-common && add-apt-repository ppa:ubuntu-toolchain-r/test
RUN curl -sL https://deb.nodesource.com/setup_4.x | sudo -E bash -
RUN apt-get update && apt-get install -y wget nodejs g++-4.9
RUN apt-get update && apt-get remove -y lua5.1 liblua5.1-0-dev libluajit-5.1-dev \
  && apt-get autoremove -y \
  && apt-get install -y wget lua5.2 liblua5.2-dev libluabind-dev
RUN rm /usr/bin/g++ /usr/bin/gcc && ln -s /usr/bin/gcc-4.9 /usr/bin/gcc && ln -s /usr/bin/g++-4.9 /usr/bin/g++

RUN git clone --depth 1 https://github.com/kirel/osrm-backend.git -b v5.2.7-psi /osrm-src \
  && rm -rf ./* \
  && cmake /osrm-src -DCMAKE_BUILD_TYPE=Release\
  && cmake --build . \
  && cmake --build . --target install \
  && mv /osrm-src/profiles/foot.lua profile.lua \
  && mv /osrm-src/profiles/lib/ lib \
  && echo "disk=/tmp/stxxl,25000,syscall" > .stxxl \
  && rm -rf /osrm-src

RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/*
