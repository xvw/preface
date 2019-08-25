#!/bin/bash
sudo chown -R opam ${HOME}
sudo chown -R opam ${GITHUB_WORKSPACE}
sudo apt-get -y install m4
opam init --disable-sandboxing
opam update -uy
eval `opam config env`
