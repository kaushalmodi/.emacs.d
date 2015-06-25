#!/bin/env tcsh

set run_test = 0
while ($#argv)
    switch ($argv[1])
        case -t :
            shift
            set run_test = 1
            breaksw
            default :
                shift
                echo "Illegal option"
    endsw
end

cd src
make clean
make
if ( ${run_test} ) then
    make test
endif
cd ..
\cp -f src/e/verilog-mode.el* .
