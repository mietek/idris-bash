#!/usr/bin/env bash

set -eu

declare _R         # Return register

declare -i _F=0    # Current frame

declare -a _S=()   # Stack frame data
declare -i _SP=0   # Beginning of current stack frame
declare -i _SQ=0   # End of current stack frame
declare -a _PSP=() # Beginnings of previous stack frames

declare -a _A      # Decoded array


IRTS_decodeArray () {
	_A=( "${1%% *}" )
	local -i _ec=1
	local -i _pc=0
	local -i _i
	local _e=
	for (( _i=0; _i < ${#1}; _i++ )); do
		local _c=${1:${_i}:1}
		case ${_c} in
		'(')
			if (( _pc++ )); then
				_e="${_e}("
			fi
			;;
		')')
			if (( --_pc )); then
				_e="${_e})"
			else
				_A[_ec++]=${_e}
				_e=
			fi
			;;
		*)
			if (( _pc )); then
				_e=${_e}${_c}
			fi
		esac
	done
}
