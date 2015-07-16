#!/usr/bin/env bash


set -eu


_R=


_ARRCTR=0
_FRMCTR=0
_THSFRM=( bottom )


idris_writeStr () {
	echo "$2"
	eval "$1="
}


idris_readStr () {
	read -r "$1"
}


idris_error () {
	echo "$@" >&2
	exit 1
}


idris_makeArray () {
	eval "_ARR${_ARRCTR}=( "\$@" )"
	_R=_ARR${_ARRCTR}
	echo "_ARR${_ARRCTR}=( $* )"
	_ARRCTR=$(( _ARRCTR + 1 ))
}


idris_indexArray () {
	local arr="$1[$2]"
	_R=${!arr}
}


idris_pushFrame () {
	eval "_FRM${_FRMCTR}=( "\${_THSFRM[@]}" )"
	_FRMCTR=$(( _FRMCTR + 1 ))
	_THSFRM=( "$@" )
}


idris_popFrame () {
	_FRMCTR=$(( _FRMCTR - 1 ))
	eval "_THSFRM=( "\${_FRM${_FRMCTR}[@]}" )"
}
