#!/usr/bin/env python

import os
import subprocess as sp

OPENCV_INCLUDE_PATH = '/usr/local/include'
CABAL_BIN           = 'arm-apple-darwin10-cabal'

def cabal(cmd, args=None):
    cmd = [CABAL_BIN, cmd]
    if args is not None:
        cmd += args
    sp.check_call(cmd)

def setup_include_paths():
    os.environ['C_INCLUDE_PATH']      = OPENCV_INCLUDE_PATH
    os.environ['CPLUS_INCLUDE_PATH']  = OPENCV_INCLUDE_PATH

def build():
    setup_include_paths()
    cabal('install', args=['--dependencies-only'])
    cabal('configure')
    cabal('build')
    print('Done.')

if __name__=='__main__':
    build()
