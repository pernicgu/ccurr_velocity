#!/usr/bin/env bash

COMMAND_NAME="$1"
SVG_FILE="$2"

if ! command -v svg2tikz >/dev/null 2>&1; then
    >&2 echo "This script requires svg2tikz to work."
    exit 2
fi

if [ ! -f "$SVG_FILE" ]; then
    >&2 echo "File $SVG_FILE not found."
    exit 1
fi

if [ "$COMMAND_NAME" == "" ]; then
    >&2 echo "Usage: $0 <command-name> <svg-file>"
    exit 1
fi

echo "\newcommand\\$COMMAND_NAME[2][1em]{%"
echo "\\begin{tikzpicture}[x=#1/100,y=-#1/100,inner sep=0pt,outer sep=0pt," \
     "fill=#2, every path/.style={fill}]"
svg2tikz --codeoutput=codeonly $SVG_FILE
echo "\\end{tikzpicture}}"