      subroutine clixtr (cline,flag,maxarg, narg,alist,lenal,icl1,icl2)
c
c Extract argument list following a given flag from command line.
c
c Input
c
c   cline  : character*(*) string representing command line.
c   flag   : character*(*) string representing flag to be searched
c            for on command line; must begin with minus sign.
c   maxarg : largest number of arguments permitted to follow flag
c            on command line.
c
c Output
c
c   narg   : number of arguments found following flag;
c            -1 if flag does not appear on command line.
c   alist  : character*(*) string giving list of arguments.
c   lenal  : non-blank length of alist; 
c            1 if alist is returned blank.
c   icl1   : pointer to character in cline containing first character of flag.
c   icl2   : pointer to character in cline containing last character of
c            last argument to flag.
c            (cline(icl1:icl2) is the substring in which the requested flag
c             and its arguments were found.)
c
c Note: If alist is too short to hold the argument list of the rquested flag
c       it will be truncated in a useless manner.  To avoid problems,
c       declare alist the same length as cline.
c
c Rules about flags and their arguments
c
c   1. A flag must begin with a minus sign.  The leading minus sign must
c      be followed by a non-null string, the first character of which may
c      not be a number or period.  The flag must not contain a second minus
c      sign or embedded blanks.
c   2. The argument list following a flag is terminated in one of three ways:
c      a. The occurence of a new flag (' -' followed by a character
c         other than a number or period).
c      b. End-of-line (hitting the end of cline)
c      c. The number of arguments found equals maxarg; i.e., no more
c         than maxarg tokens following a flag will be recognized as
c         arguments of that flag.
c   3. If the requested flag occurs more than once in the command line,
c      only the arguments from the first occurrence are returned.
c   4. White-space (blanks) between a flag and its first argument is
c      optional.  However, if maxarg=0 and no such space exists,
c      then the flag may not be recognized.  For example, if clixtr is
c      called with maxarg=0, then '-astring' will be recognized only as
c      the flag 'astring' with no arguments.  If called with maxarg>0, however,
c      then '-astring' may be interpreted as flag '-a' followed by argument
c      'string', or as flag '-as' with argument 'tring', etc.
c   5. An argument may begin with '-' only if its second character is
c      a number (0-9) or a period (.).
c
c Example
c
c   Let      cline = '-e sharp file -b -cdog -libzip -d x1 x2 x3 -o george'
c
c   Then     call clixtr (cline,'-c',0,10, narg,alist,icl1,icl2)
c   returns  narg = 1 ; alist = 'dog' ; icl1 = 18 ; icl2 = 22.
c
c   Then     call clixtr (cline,'-e',0,10, narg,alist,icl1,icl2)
c   returns  narg = 2 ; alist = 'sharp file' ; icl1 = 1 ; icl2 = 13.
c
c   Then     call clixtr (cline,'-e',0,0, narg,alist,icl1,icl2)
c   returns  narg = 0 ; alist = ' ' ; icl1 = 1 ; icl2 = 2.
c
c   Then     call clixtr (cline,'-b',0,10, narg,alist,icl1,icl2)
c   returns  narg = 0 ; alist = ' ' ; icl1 = 15 ; icl2 = 16.
c
c   Then     call clixtr (cline,'-lib',0,10, narg,alist,icl1,icl2)
c   returns  narg = 1 ; alist = 'zip' ; icl1 = 24 ; icl2 = 30.
c
c   Then     call clixtr (cline,'-l',0,10, narg,alist,icl1,icl2)
c   returns  narg = 1 ; alist = 'ibzip' ; icl1 = 24 ; icl2 = 30.
c
c   Then     call clixtr (cline,'-d',0,10, narg,alist,icl1,icl2)
c   returns  narg = 3 ; alist = 'x1 x2 x3' ; icl1 = 32 ; icl2 = 42.
c
      character*(*) cline,flag,alist
      character*11 numb
	common/sccsclixtr/ sccsid
	character*80 sccsid
      parameter (numb = '1234567890.')
	data sccsid /'@(#)clixtr.f	41.1	12/21/90'/
c
c Defaults
c
      narg = -1
      alist = ' '
      lenal = 1
      icl1 = 1
      icl2 = 1
c
c Find non-blank portion of flag: flag(jf:kf).
c Return default output if flag is invalid; i.e.
c   1. Does not begin with '-'.
c   2. Has a second '-'.
c   3. Has embedded space(s).
c   4. Has no non-blank string after leading '-'.
c   5. Has number or prtiod for as second character.
c
      lenf = len(flag)
      jf = index(flag,'-')
      if (jf.eq.0) return
      if (jf.eq.lenf) return
      if (index(flag(jf+1:),'-').gt.0) return
      if (index(numb,flag(jf+1:jf+1)).gt.0) return
      if (jf.gt.1) then
	if (flag(1:jf-1).ne.' ') return
      end if
      kf = jf + index(flag(jf+1:),' ') - 1
      if (kf.lt.jf) then
	kf = lenf
      else
        if (kf.eq.jf) return
	if (flag(kf+1:).ne.' ') return
      end if
c
c Find first occurrence of flag in command list.
c Return if no occurrence.
c
      lenc = len(cline)
      kc = 0
10    jc = kc + index(cline(kc+1:),flag(jf:kf))
      if (jc.eq.kc) return
      kc = jc + kf -jf
      if (jc.gt.1) then
	if (cline(jc-1:jc-1).ne.' ') go to 10
      end if
      if (kc.lt.lenc) then
        if (cline(kc+1:kc+1).ne.' ' .and. maxarg.le.0) go to 10
        if (cline(kc+1:kc+1).eq.'-') then
	  if (kc+1.lt.lenc) then
	    if (index(numb,cline(kc+2:kc+2)).eq.0) go to 10
	  else
	    go to 10
	  end if
	end if
      end if
c
c Extract and count arguments.
c
      lena = len(alist)
      icl1 = jc
      icl2 = kc
      narg = 0
20    if (narg.ge.maxarg) return
c
      jc = icl2
30    jc = jc + 1
      if (jc.gt.lenc) then
	return
      else if (cline(jc:jc).eq.' ') then
	go to 30
      else if (cline(jc:jc).eq.'-') then
	if (jc.eq.lenc) return
	if (index(numb,cline(jc+1:jc+1)).eq.0) return
      end if
c
      if (jc.eq.lenc) then
	kc = jc
      else
	kc = jc + index(cline(jc+1:),' ') - 1
	if (kc.lt.jc) kc = lenc
      end if
      ka = lenal
      if (narg.eq.0) ka = -1
      ja = ka + 2
      ka = ja + kc - jc
      if (ka.gt.lena) return
      alist(ja:ka) = cline(jc:kc)
      lenal = ka
      narg = narg + 1
      icl2 = kc
      go to 20
c
      end
