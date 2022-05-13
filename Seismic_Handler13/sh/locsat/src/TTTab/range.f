      subroutine range (string,ifirst,ilast)
c
c input     
c
c   string = character string
c
c output
c
c   ifirst = index of first non-blank in STRING
c   ilast =  index of last  non-blank in STRING
c            (note: if STRING=' ' then IFIRST=ILAST=1)
c
      character*(*) string
	common/sccsrange/ sccsid
	character*80 sccsid
	data sccsid /'@(#)range.f	41.1	12/21/90'/
c
      if (string.eq.' ') then
        ifirst = 1
        ilast = 1
        return
      end if
      leng = len(string)
      do 10 ifirst = 1,leng
10    if (string(ifirst:ifirst).ne.' ') go to 20
      ifirst = leng + 1
20    do 30 ilast = leng,ifirst,-1
30    if (string(ilast:ilast).ne.' ' .and.
     &    string(ilast:ilast).ne.'\0') return
      ilast = 0
      return
      end
