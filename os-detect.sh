#!/bin/bash

GRML_OSTYPE=$(uname -s)

function islinux () {
    [[ $GRML_OSTYPE == "Linux" && -x $(command -v apt-get) ]]
}

function isdarwin () {
    [[ $GRML_OSTYPE == "Darwin" ]]
}

if isdarwin; then
  INSTALL='brew install'
  GUI_INSTALL='brew cask install'
fi

if islinux; then
  INSTALL='sudo apt-get -y install'
  GUI_INSTALL=$INSTALL
fi
