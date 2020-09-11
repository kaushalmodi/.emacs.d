#!/usr/bin/env bash

run_test=0

for var in "$@"
do
    if [[ "${var}" == '-t' ]]
    then
        run_test=1
    fi
done

cd ./src/ || exit

make clean
make e/verilog-mode.elc
if [[ "${run_test}" -eq 1 ]]
then
    make -j 8 test
fi

cd ..
cp -f ./src/e/verilog-mode.el* .
