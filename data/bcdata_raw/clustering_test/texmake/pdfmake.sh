#!/bin/bash
TODAY=$(date +"%Y%m%d_%H%M")
latexmk -pdf -quiet -jobname=./merged_graphics_${TODAY} merged_graphics.tex 
