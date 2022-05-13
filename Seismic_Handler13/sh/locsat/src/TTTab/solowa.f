      subroutine solowa (prompt,dfl, ans,iquit)
c
c Solicit one-word answer from user, usinq string PROMPT
c  as a prompt.
c
c If DFL = ' ', the user is pestered until he responds with a
c  non-blank, one-word response (no embedded blanks).
c If DFL != ' ', the user is shown DFL as a default response, which
c  he may invoke with <rtn> (preceded by any number of blanks).
c  However, a multi-word response will still be rejected.
c
c The string ANS is returned as the user's first valid response.
c  If ANS is 'quit' or any abbreviation thereof, IQUIT is returned as 1.
c  Otherwise, IQUIT is 0.
c
c PROMPT is truncated to 60 characters.
c DFL is truncated to 80 characters.
c The user's input is truncated to 80 characters, including any leading blanks.
c
c Calls CIPTYP, CIPSCN, RANGE
c
      character*(*) prompt, dfl, ans
      character prm*144, buf*80
	common /sccssolowa/ sccsid
	character*80 sccsid
	data sccsid /'@(#)solowa.f	41.1	12/21/90'/
c
      call range (prompt,jp,kp)
      kp = min(kp,60)
      prm = prompt(1:kp)
      call range (dfl,jd,kd)
      if (dfl(jd:jd).eq.' ') then
	idfl = 0
      else
	kd = min(kd,jd+79)
	prm(kp+2:) = '('
	prm(kp+3:) = dfl(jd:kd)
	kp = kp + kd - jd + 4
	prm(kp:) = ')'
	idfl = 1
      end if
      kp = kp + 1
c
10    call ciptyp (prm(1:kp),buf)
      if (buf.eq.'eot') buf = 'q'
      if (buf.eq.' ') then
	if (idfl.eq.0) go to 10
	buf = dfl(jd:kd)
      end if
c
      call range (buf,jb,kb)
      if (kb-jb.ge.len(ans)) then
	print '(a)', '? Answer too long.'
	go to 10
      end if
c
      i = index(buf(jb:kb),' ')
      if (i.gt.0) then
	print '(a)', '? One-word answer, please.'
	go to 10
      end if
c
      ans = buf(jb:kb)
      call cipscn (ans,'quit',1,iquit)
c
      return
      end
