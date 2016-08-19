FROM cartography/osrm-backend-docker

RUN apt-get update && apt-get install -y software-properties-common && add-apt-repository ppa:ubuntu-toolchain-r/test
RUN curl -sL https://deb.nodesource.com/setup_4.x | sudo -E bash -
RUN apt-get update && apt-get install -y wget nodejs g++-4.8

RUN curl --silent -L https://github.com/Project-OSRM/osrm-backend/archive/v5.3.2.tar.gz -o v5.3.2.tar.gz \
  && tar xzf v5.3.2.tar.gz \
  && mv osrm-backend-5.3.2 /osrm-src \
  && cmake /osrm-src -DCMAKE_BUILD_TYPE=Release\
  && cmake --build . \
  && cmake --build . --target install \
  && mv /osrm-src/profiles/foot.lua profile.lua \
  && mv /osrm-src/profiles/lib/ lib \
  && echo "disk=/tmp/stxxl,25000,syscall" > .stxxl \
  && rm -rf /osrm-src

RUN npm install --unsafe-perm osrm # in osrm-build

COPY osrm.js .

RUN apt-get clean \
 && rm -rf /var/lib/apt/lists/*
