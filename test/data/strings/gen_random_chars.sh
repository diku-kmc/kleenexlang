#!/bin/bash

size_bytes=$1

cat /dev/urandom | env LC_CTYPE=C tr -dc '\n0-9a-zA-Z!@#$%^^&**()}|"\t ' | head -c $size_bytes
