#!/usr/bin/env bash

set -eu

declare _R         # Return register

declare -i _F=0    # Current frame

declare -a _S=()   # Stack frame data
declare -i _SP=0   # Beginning of current stack frame
declare -i _SQ=0   # End of current stack frame
declare -a _PSP=() # Beginnings of previous stack frames

declare -a _A=()   # Array frame data
declare -i _AP=0   # Beginning of current array frame
