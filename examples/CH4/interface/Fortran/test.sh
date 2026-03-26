#!/bin/bash

FC=ifort cmake -B build
cmake --build build

./build/CH4-PIP-NN ../../rawdata/CH4.xyz > energy.txt
