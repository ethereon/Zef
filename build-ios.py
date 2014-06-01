#!/usr/bin/env python

import os
import subprocess as sp

OPENCV_INCLUDE_PATH = '/usr/local/include'

def cabal(cmd, args=None):
    cmd = ['cabal-ios', cmd]
    if args is not None:
        cmd += args
    sp.check_call(cmd)

def setup_include_paths():
    os.environ['C_INCLUDE_PATH']      = OPENCV_INCLUDE_PATH
    os.environ['CPLUS_INCLUDE_PATH']  = OPENCV_INCLUDE_PATH

def build():
    setup_include_paths()
    cabal('clean')
    cabal('configure')
    cabal('build', args=['zef'])
    print('Done.')

if __name__=='__main__':
    build()
