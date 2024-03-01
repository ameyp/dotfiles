#!/bin/bash

AMEYZING_OSTYPE=$(uname -s)

function islinux () {
    [[ $AMEYZING_OSTYPE == "Linux" && -x $(command -v apt-get) ]]
}

function isdarwin () {
    [[ $AMEYZING_OSTYPE == "Darwin" ]]
}
