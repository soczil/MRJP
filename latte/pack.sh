#!/bin/bash

find . -name ".DS_Store" -print -delete

tar -zcvf ks394468.tar.gz src Makefile README.md
