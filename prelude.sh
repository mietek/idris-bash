#!/usr/bin/env bash

set -eu

declare _R # Return register

declare -a _S=()   # Stack frame data
declare -i _SP=0   # Beginning of current stack frame
declare -i _SQ=0   # End of current stack frame
declare -i _SR=0   # Current stack frame reference
declare -a _PSP=() # Beginnings of previous stack frames

declare -a _A=() # Array frame data
declare -i _AP=0 # Beginning of current array frame


idris_pushFrame () {
	_PSP[${_SR}]=${_SP}
	_SP=${_SQ}
	_SR=$(( _SR + 1 ))
	for arg in "$@"; do
		_S[${_SQ}]=${arg}
		_SQ=$(( _SQ + 1 ))
	done
}


idris_popFrame () {
	_SQ=${_SP}
	_SR=$(( _SR - 1 ))
	_SP=${_PSP[${_SR}]}
}


idris_makeArray () {
	_R=${_AP}
	# local log="${_AP}"
	for arg in "$@"; do
		_A[${_AP}]=${arg}
		_AP=$(( _AP + 1 ))
		# log="${log} ${arg}"
	done
	# echo "${log}"
}


idris_error () {
	echo "$1" >&2
	exit 1
}


idris_writeStr () {
	echo "$1"
}
