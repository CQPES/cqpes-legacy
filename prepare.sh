#!/bin/bash

python3 -u src/prepare/prepare.py -c config/prepare.json | tee prepare.log
