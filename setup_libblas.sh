#!/bin/sh

LIBBLAS='/usr/lib/x86_64-linux-gnu/libblas.so'

if [ -f "$LIBBLAS" ]; then
  ln -s "$LIBBLAS" priv/lib/libblas.so
fi