#!/bin/bash

script_dir=`dirname $0`
source $script_dir/config.conf

export CUSTOM_PATH=$TOOLCHAIN_PATH/bin:$SDK_PATH/platform-tools/
export PATH=$CUSTOM_PATH:$PATH

if [ $# -eq 0 ]; then
  echo "No argument supplied"
else
  source_dir=`dirname $1`
  out_path=$source_dir/out
  arm-linux-androideabi-g++ -o $out_path "$1"
  adb push $out_path /data/local/tmp
  rm -r $out_path
fi
