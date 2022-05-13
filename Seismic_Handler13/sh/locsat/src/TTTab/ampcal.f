c
      subroutine ampcal (deltin, indxphaz, idepth,qfact)
      dimension iqfpkpbc (12) 
      dimension iqfpkpab (12) 
      dimension iqf (180)
      dimension ppq(20,2)
      dimension  skpdfq(9,2), pkkpdfq(11,2),pkppkpq(10,2)

	common /sccsampcal/ sccsid
	character*80 sccsid
	data sccsid /'@(#)ampcal.f	31.1	8/26/88'/

c              indxphaz- index of the desired phase
c
c     1 PG        2 ScP       3 PcP  
c     4 pP        5 sP        6 PP
c     7 P         8 PPP       9 SP
c     10 SPP      11 SKPdf    12 SKPab
c     13 PKPdf    14 PKPab    15 PKPbc
c     16 PKPcd    17 PKKPdf   18 PKKPbc
c     19 PKKPab   20 PKPPKP   21 (none)
c     22 PcS      23 ScS      24 SKSac
c     25 SKSdf    26 S        27 pS 
c     28 sS       29 PS       30 PPS
c     31 SS       32 SSS      33 (none)
c     34 (none)   35 (none)   36 LR-Rayleigh
c     37 LQ-Love  38 Pn       39 Pg
c     40 Sn       41 Lg       42 Rg
c
c The last five regional phases are computed with a separate
c  velocity stucture (in file velocity.dat)
c  that may be changed by the user.
c
c Also note that no amplitudes are computed for regional phases
c  at this time.
c
c
      delta=deltin
      depth=idepth
      qfact = 0.0
c
      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17
     1      ,18,19,20,21,22,23,24,25,26) indxphaz
c
8     return
9     return
10    return
16    return
21    return
22    return
23    return
24    return
25    return
c
1     continue
      if(delta.gt.10.0) return
      delta=amax1(delta,0.5)
      qfact=1.6+3.0*alog10(delta/5.0)
      return
c
2     continue
      qfact = 4.4
      if(idepth.gt.70) qfact=3.9
      return

3     continue
      if(delta .gt. 80.) return
      qfact=4.0
      return

4     continue
      if(delta.lt.5.) return
      qfact = float(iqf(nint(delta)))/100.
      if(idepth.gt.70) qfact=qfact+0.5
      if(idepth.lt.70) qfact=qfact+0.3
      go to 777

5     continue
      if(delta.lt.5.) return
      qfact = float(iqf(nint(delta)))/100.
      if(idepth.gt.70) qfact=qfact+0.5
      if(idepth.lt.70) qfact=qfact+0.3
      go to 777

6     continue
      if(delta.lt.10.) return

      data ppq /
     1   10.,11.,12.,13.,14.,15.,20.,25.,30.,35.,
     1          40.,45.,50.,55.,60.,65.,70.,75.,80.,180.,
c    1 --------------------------------------------------
     1          3.0,3.1,3.2,3.3,3.4,3.5,3.7,3.9,4.0,3.7,
     1          3.6,3.6,3.7,3.8,3.9,4.0,4.1,4.1,4.2,4.2/

      do 606 j=1,19
      if(delta.le.ppq(j+1,1)  .and.
     1     delta.gt.ppq(j,1))         then
                      qfact=ppq(j+1,2)
                        go to 6606
                                            endif
606   continue
6606  return

7     continue
      data iqf/
     * 089, 207, 220, 255, 276, 290, 302, 310, 315, 319, 323, 
     * 325, 326, 326, 325, 321, 310, 298, 279, 277, 280, 285, 
     * 294, 304, 315, 325, 335, 342, 344, 342, 338, 336, 336,
     * 335, 334, 334, 334, 333, 333, 332, 332, 332, 333, 333, 
     * 334, 334, 335, 336, 336, 337, 337, 338, 339, 339, 340,
     * 340, 341, 342, 342, 343, 344, 344, 345, 345, 346, 346, 
     * 347, 348, 348, 349, 350, 350, 351, 351, 352, 353, 353,
     * 354, 354, 355, 356, 357, 358, 359, 361, 364, 366, 368,
     * 372, 376, 380, 385, 391, 398, 406, 414, 422, 430, 438, 
     * 446, 
     *  459, 464, 470, 478, 486, 498, 513,
     * 523, 520, 503, 482, 475, 472, 568, 465, 463, 460, 457, 453,
     * 449, 446, 423, 439, 439, 435, 432, 428, 425, 423, 422, 423,
     * 425, 429, 435, 439, 441, 441, 440, 439, 438, 437, 435, 410,
     * 400, 400, 400, 400, 400, 400, 400, 400, 400, 398, 401, 404,
     * 406, 408, 410, 414, 417, 418, 419, 419, 419, 418, 418, 418,
     * 418, 417, 416, 416, 416, 8*416/
      if(delta.lt.1.0) delta=1.0
      qfact = float(iqf(nint(delta)))/100.


c VEITH AND CLAWSON MAGNITUDE CORRECTIONS...LINEAR APPROXIMATION FOR DEPTH
777   continue
      if(idepth.le.40) then
         corr=.55*depth/100.
      else if(idepth.le.300) then
          corr=.22+(depth-40.)*.17/100.
      else
          corr=.66+(depth-300.)*.11/100.
      endif

      qfact=qfact-corr
      qfact=qfact+0.3
      return

11    continue
      if(delta .lt. 105.) return

      data skpdfq /
     1   105.,110.,113.,117.,120.,125.,130.,140.,180.,
     1   5.4, 5.4, 5.2, 5.0, 4.8, 4.7, 4.6, 4.4, 4.4/

      do 611 j=1,8
      if(delta.le.skpdfq(j+1,1)  .and.
     1     delta.gt.skpdfq(j,1))         then
                      qfact=skpdfq(j+1,2)
                   if(idepth.gt.70)qfact=qfact-0.9
                        go to 6611
                                        endif
611   continue
6611  return

12    continue
      qfact = 3.9
      if(idepth.gt.70) qfact=3.7
      return

13    continue
      go to 7

14    continue
      data iqfpkpab /
     *   358, 358, 363, 368, 372, 380, 386, 392, 400, 407, 415, 415/
      indx=nint(delta)
      if(indx.lt.151)then
         qfact=3.58
        return
      else if(indx.gt.162) then
        qfact=4.15
        return
      else
        qfact = float(iqfpkpab(indx-150))/100.
        return
      endif


15    continue
      data iqfpkpbc /
     *   425, 425, 401, 365, 351, 349, 348, 348, 350, 352, 355, 400/
      indx=nint(delta)
      if(indx.lt.141)then
         qfact=4.25
        return
      else if(indx.gt.152) then
        qfact=4.00
        return
      else
        qfact = float(iqfpkpbc(indx-140))/100.
        return
      endif

17    continue
      if(delta.lt.20.) return

      data pkkpdfq/
     1   20.,30.,40.,50.,60.,130.,140.,150.,160.,170.,180.,
     1          5.5,5.4,5.2,5.0,4.7,4.7, 4.8, 5.0, 5.2, 5.4, 5.5/

      do 617 j=1,10
      if(delta.le.pkkpdfq(j+1,1)  .and.
     1     delta.gt.pkkpdfq(j,1))         then
                      qfact=pkkpdfq(j+1,2)
                        go to 6617
                                            endif
617   continue
6617  return


18    continue
      if(delta .gt. 130.  .or.delta .lt. 90.) return
      qfact = 4.6
      return

19    continue
      if (delta .lt. 105.  .or. delta. gt. 125.) return
      qfact = 4.6
      return

20    continue
      data pkppkpq /
     1  30.,40.,50.,55.,95.,100.,110.,120.,130.,140.,
     1 5.4,5.3,5.0,4.6,4.6,5.0,5.3,5.4,5.4,5.4/
      if(delta.lt. 30. .or. delta. gt. 140.) return

      do 620 j=1,9
      if(delta.le.pkppkpq(j+1,1)  .and.
     1     delta.gt.pkppkpq(j,1))         then
                      qfact=pkppkpq(j+1,2)
                        go to 6620
                                            endif
620   continue
6620  return

26    continue
      if(delta.gt.6.) return
      if(delta .lt. 1.0) delta=1.0
      qfact=   -0.3 + float(iqf(nint(delta)))/100.
      go to 777

      end
