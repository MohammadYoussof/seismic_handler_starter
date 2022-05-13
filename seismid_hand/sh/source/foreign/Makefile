# file Makefile
#      ========
#
# version 8, 16-Feb-2000
#
# Makefile of directory $SH_SOURCE/foreign
# K. Stammler, 7-Mar-95

CFLAGS      = $(SH_CCOPTIONS) -I.. -DBASECNST=\"basecnst.h\" -Dsparc=1
CC          = $(SH_COMPILER)
LD          = $(SH_COMPILER)
RANLIB		= $(SH_RANLIB)
LDFLAGS     = -L/usr/local/X11/lib
LIBS        = -lm -lX

LIB_FRGN    = ${SH_LIB}libfrgn.a
LIB_FRGN_SRCS	= f2c.c	gsedata.c lineparse.c sh_ah.c sh_gse.c sh_gse2.c \
	sh_len.c sh_neic.c shfrgn.c

#LIB_FRGN_OBJS	= $(LIB_FRGN_SRCS:%.c=%.o)
LIB_FRGN_OBJS	= $(LIB_FRGN_SRCS:.c=.o)


all:
	@echo $(LIB_FRGN_OBJS); make $(LIB_FRGN)


$(LIB_FRGN): $(LIB_FRGN_OBJS)
	ar r $@ $(LIB_FRGN_OBJS); $(RANLIB) $@


clean:
	@rm $(LIB_FRGN_OBJS) *%


depend:
	makedepend -s "# DO NOT DELETE" -- $(CFLAGS) -- $(LIB_FRGN_SRCS)



# DO NOT DELETE

f2c.o: f2c.h
gsedata.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
gsedata.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
gsedata.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
gsedata.o: /usr/include/stdio_impl.h /usr/include/string.h
gsedata.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
gsedata.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h
gsedata.o: lineparse.h gsedata.h f2c.h
lineparse.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
lineparse.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
lineparse.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
lineparse.o: /usr/include/stdio_impl.h /usr/include/string.h
lineparse.o: /usr/include/iso/string_iso.h ../basecnst.h ../sysbase.h
lineparse.o: ../fousrdef.h lineparse.h
sh_ah.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sh_ah.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sh_ah.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sh_ah.o: /usr/include/stdio_impl.h /usr/include/string.h
sh_ah.o: /usr/include/iso/string_iso.h /usr/include/ctype.h
sh_ah.o: /usr/include/iso/ctype_iso.h /usr/include/math.h
sh_ah.o: /usr/include/iso/math_iso.h ../basecnst.h ../sysbase.h ../fousrdef.h
sh_ah.o: ../tcusrdef.h sh_ah.h /usr/include/rpc/rpc.h
sh_ah.o: /usr/include/rpc/types.h /usr/include/sys/types.h
sh_ah.o: /usr/include/sys/machtypes.h /usr/include/sys/int_types.h
sh_ah.o: /usr/include/sys/select.h /usr/include/sys/time.h
sh_ah.o: /usr/include/sys/time.h /usr/include/tiuser.h
sh_ah.o: /usr/include/sys/tiuser.h /usr/include/sys/tpicommon.h
sh_ah.o: /usr/include/fcntl.h /usr/include/sys/fcntl.h /usr/include/memory.h
sh_ah.o: /usr/include/rpc/xdr.h /usr/include/sys/byteorder.h
sh_ah.o: /usr/include/rpc/auth.h /usr/include/rpc/clnt_stat.h
sh_ah.o: /usr/include/sys/cred.h /usr/include/rpc/clnt.h
sh_ah.o: /usr/include/rpc/rpc_com.h /usr/include/sys/netconfig.h
sh_ah.o: /usr/include/rpc/rpc_msg.h /usr/include/rpc/auth_sys.h
sh_ah.o: /usr/include/rpc/auth_des.h /usr/include/sys/socket.h
sh_ah.o: /usr/include/sys/uio.h /usr/include/netinet/in.h
sh_ah.o: /usr/include/sys/stream.h /usr/include/sys/vnode.h
sh_ah.o: /usr/include/sys/t_lock.h /usr/include/sys/machlock.h
sh_ah.o: /usr/include/sys/param.h /usr/include/sys/unistd.h
sh_ah.o: /usr/include/sys/mutex.h /usr/include/sys/rwlock.h
sh_ah.o: /usr/include/sys/semaphore.h /usr/include/sys/condvar.h
sh_ah.o: /usr/include/sys/time_impl.h /usr/include/sys/resource.h
sh_ah.o: /usr/include/vm/seg_enum.h /usr/include/sys/poll.h
sh_ah.o: /usr/include/sys/strmdep.h /usr/include/sys/model.h
sh_ah.o: /usr/include/sys/strft.h /usr/include/sys/un.h
sh_ah.o: /usr/include/net/if_dl.h /usr/include/rpc/rpcsec_gss.h
sh_ah.o: /usr/include/gssapi/gssapi.h /usr/include/rpc/svc.h
sh_ah.o: /usr/include/sys/tihdr.h /usr/include/rpc/svc_auth.h
sh_ah.o: /usr/include/rpc/rpcb_clnt.h /usr/include/rpc/rpcb_prot.h
sh_ah.o: /usr/include/rpc/svc_mt.h /usr/include/synch.h
sh_ah.o: /usr/include/sys/synch.h ahhead.h
sh_gse.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sh_gse.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sh_gse.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sh_gse.o: /usr/include/stdio_impl.h /usr/include/string.h
sh_gse.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
sh_gse.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h
sh_gse.o: ../tcusrdef.h sh_gse.h
sh_gse2.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sh_gse2.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sh_gse2.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sh_gse2.o: /usr/include/stdio_impl.h /usr/include/string.h
sh_gse2.o: /usr/include/iso/string_iso.h ../basecnst.h /usr/include/stdlib.h
sh_gse2.o: /usr/include/iso/stdlib_iso.h ../sysbase.h ../fousrdef.h
sh_gse2.o: ../tcusrdef.h gsedata.h sh_gse2.h
sh_len.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sh_len.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sh_len.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sh_len.o: /usr/include/stdio_impl.h /usr/include/string.h
sh_len.o: /usr/include/iso/string_iso.h /usr/include/stdlib.h
sh_len.o: /usr/include/iso/stdlib_iso.h /usr/include/ctype.h
sh_len.o: /usr/include/iso/ctype_iso.h ../basecnst.h ../sysbase.h
sh_len.o: ../fousrdef.h ../tcusrdef.h ../utusrdef.h sh_len.h
sh_neic.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
sh_neic.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
sh_neic.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
sh_neic.o: /usr/include/stdio_impl.h /usr/include/string.h
sh_neic.o: /usr/include/iso/string_iso.h /usr/include/math.h
sh_neic.o: /usr/include/iso/math_iso.h ../basecnst.h ../sysbase.h
sh_neic.o: ../fousrdef.h ../tcusrdef.h ../qfusrdef.h ../dfqentry.h sh_neic.h
shfrgn.o: /usr/include/stdio.h /usr/include/iso/stdio_iso.h
shfrgn.o: /usr/include/sys/feature_tests.h /usr/include/sys/isa_defs.h
shfrgn.o: /usr/include/sys/va_list.h /usr/include/stdio_tag.h
shfrgn.o: /usr/include/stdio_impl.h ../basecnst.h ../sysbase.h ../fousrdef.h
shfrgn.o: ../shconst.h shfrgn.h sh_neic.h sh_gse.h sh_ah.h sh_len.h sh_gse2.h
