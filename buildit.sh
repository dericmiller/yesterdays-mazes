#!/usr/bin/env sh
set -e
ca65 CrazeMazing.s -g -o CrazeMazing.o
ld65 -o CrazeMazing.nes -C CrazeMazing.cfg CrazeMazing.o -m CrazeMazing.map.txt -Ln CrazeMazing.labels.txt --dbgfile CrazeMazing.nes.dbg
