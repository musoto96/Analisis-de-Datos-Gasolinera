#!/bin/sh
apt-get update
apt-get -y install libcurl4-openssl-dev libssl-dev libxml2-dev
Rscript packages.R