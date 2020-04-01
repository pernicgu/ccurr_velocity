#!/bin/bash
cat ./texbegin.tex > merged_graphics.tex
( for i in ../ts_figs/*.tex ; do cat $i ; echo '\newline' ; done ) >> merged_graphics.tex
( for i in ../ts_tables/*.tex ; do cat $i ; echo '\newline' ; done ) >> merged_graphics.tex
cat ./texend.tex >> merged_graphics.tex
