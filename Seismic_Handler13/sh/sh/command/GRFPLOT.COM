$ if  p1 .eqs. ""  then  $ inquire p1 "directory (including colon)"
$ if  p2 .eqs. ""  then  $ inquire p2 "start time"
$ if  p3 .eqs. ""  then  $ inquire p3 "station list"
$ if  p4 .eqs. ""  then  $ inquire p4 "seconds per plot"
$ if  p5 .eqs. ""  then  $ inquire p5 "number of plots"
$
$ open/write cmdfil sys$login:make_grf_plots.com
$ write cmdfil "$ set verify"
$ write cmdfil "$ shc"
$ write cmdfil "copen;;"
$ write cmdfil "switch/global sherrstop on"
$ write cmdfil "grfplot ''p1' ''p2' ''p3' ''p4' ''p5'"
$ write cmdfil "quit y"
$ close cmdfil
$ purge/nolog sys$login:make_grf_plots.com
$ submit/notify sys$login:make_grf_plots
$
$ exit
