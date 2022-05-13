$
$! file shsetup.com
$!      ===========
$!
$! version 1, 30-Jul-92
$!
$! VMS startup file for SeismicHandler
$! K. Stammler, 30-Jul-92
$!
$! As you can see at the date of last change the VMS part of SH is not
$! supported any more.  So don't expect this to work.
$
$
$! run-time logicals
$ define/nolog srcdsk u21:
$! define/nolog/trans=concealed shc_root bgr_seismo_:[sh.]
$! define/nolog/trans=concealed shc_root t21:[b311stamm.shs.]
$ define/nolog shc_command bgr_seismo_:[sh.command]
$ define/nolog shc_bmpcommand bgr_seismo_:[sh.command.bmp]
$ define/nolog shc_filter bgr_seismo_:[sh.filter]
$ define/nolog shc_globals bgr_seismo_:[sh.globals]
$ define/nolog shc_help bgr_seismo_:[sh.help]
$ define/nolog shc_errors bgr_seismo_:[sh.errors]
$ define/nolog shc_inputs bgr_seismo_:[sh.inputs]
$ define/nolog shc_util bgr_seismo_:[sh.util]
$ define/nolog shc_pdedir u21:[b311detec.prog.davis.locations]
$ define/nolog shc_wdv u21:[b311stamm.wdv]
$ define/nolog cm u21:[b311stamm.sh.shcupd]
$
$! logicals for compilation
$ define/nolog shc_main srcdsk:[b311stamm.sh.source]
$ define/nolog shc_graphics srcdsk:[b311stamm.sh.source.graphics]
$ define/nolog shc_newgraph srcdsk:[b311stamm.sh.source.newgraph]
$ define/nolog shc_qprog srcdsk:[b311stamm.qprog]
$ define/nolog shc_local srcdsk:[b311stamm.sh.local]
$ define/nolog shc_qlist srcdsk:[b311stamm.sh.qlist]
$ define/nolog shc_lib u31:[b311stamm.kslib]
$ define/nolog shc_foreign srcdsk:[b311stamm.sh.source.frgn]
$ define/nolog shc_objects srcdsk:[b311stamm.sh.obj]
$ define/nolog shc_utilsrc srcdsk:[b311stamm.sh.util]
$ define/nolog shc_doc srcdsk:[b311stamm.sh.doc]
$
$ ! logicals for GRN reading (currently the files are on my directory, K.S.)
$ define/nolog ks_util u21:[b311stamm.prog]
$ define/nolog grn_glsdir ks_util:,sys$login:
$ grn2q == "$ks_util:grn2q"
$ make_grndir == "@ks_util:make_grndir"
$
$! commands
$ shc == "$bgr_seismo_:[sh]seismhan_world"
$ shc_n == "$srcdsk:[b311stamm.sh]seismhan"
$ butfreq == "$shc_util:butfreq"
$ butrec == "$shc_util:butrec"
$ restfreq == "$shc_util:restfreq"
$ restrec == "$shc_util:restrec"
$ simfreq == "$shc_util:simfreq"
$ simrec == "$shc_util:simrec"
$ simrec2 == "$shc_util:simrec2"
$ fconcat == "$shc_util:fconcat"
$ findev*ent == "$shc_util:findevnt"
$ locdiff == "$shc_util:locdiff"
$ locadd == "$shc_util:locadd"
$ fereg == "$shc_util:fereg"
$ statloc == "search shc_inputs:statloc.dat"
$ pde_select == "run shc_pdedir:pde_select"
$ pdesort_baz == "@shc_pdedir:sort_baz"
$ pdesort_dist == "@shc_pdedir:sort_dist"
$ pdesort_depth == "@shc_pdedir:sort_depth"
$ pdesort_mag == "@shc_pdedir:sort_mag"
$ pdesort_date == "@shc_pdedir:sort_date"
$ o2q == "$shc_util:o_to_q"
