      subroutine clitok (list,itok, tok,ilist)
c
c TOK = ITOK'th token in LIST.
c ILIST = position in LIST of the last charater in TOK.
c
c If there are fewer than ITOK tokens in LIST, then the values
c  returned are TOK = ' ' and ILIST = 0.
c If necessary, the ITOK'th token in LIST is truncated to fit in TOK.
c
      character*(*) list,tok
	common/sccsclitok/ sccsid
	character*80 sccsid
	data sccsid /'@(#)clitok.f	41.1	12/21/90'/
c
c Initialize.
c
      lenl = len(list)
      it = 0
      jl = 0
c
c Find start of next token (skip over blanks).
c Return if there is none, otherwise count it.
c
10    jl = jl + 1
      if (jl.gt.lenl) then
	tok = ' '
	ilist = 0
	return
      else if (list(jl:jl).eq.' ') then
	go to 10
      end if
      it = it + 1
c
c Find end of token (search for blank).
c
      i = index(list(jl:),' ')
      if (i.eq.0) then
	kl = lenl
      else
	kl = jl + i - 1
      end if
c
c Okay, it'th token of list is in list(jl:kl).
c Either grab it or go back for another.
c
      if (it.eq.itok) then
	tok = list(jl:kl)
	ilist = kl
	return
      else
	jl = kl
	go to 10
      end if
c
      end
