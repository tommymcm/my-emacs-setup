#!/bin/bash

GIT_ROOT=`git rev-parse --show-toplevel` ;

cp ${GIT_ROOT}/.emacs ~/.emacs ;

THEMES_DIR=~/.emacs.d/themes ;
if [ ! -d ${THEMES_DIR} ] ; then
    mkdir -p ${THEMES_DIR} ;
fi

find ${GIT_ROOT}/themes -name "*.el" -exec cp {} ${THEMES_DIR} \; ;
