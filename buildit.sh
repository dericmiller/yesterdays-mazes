#!/usr/bin/env sh
set -e
ca65 YesterdaysMazes.s -g -o YesterdaysMazes.o
ld65 -o YesterdaysMazes.nes -C YesterdaysMazes.cfg YesterdaysMazes.o -m YesterdaysMazes.map.txt -Ln YesterdaysMazes.labels.txt --dbgfile YesterdaysMazes.nes.dbg
