      function func(i,x,z)
c**********************************************************************
c
c
c        parameters
c
c           input
c
c              i - i*4 - index to desired operation
c
c              x - r*4 - length of phase segment in degrees
c
c              z - r*4 - depth of phase segment in kilometers
c
c**********************************************************************

	common /sccsfunc/ sccsid
	character*80 sccsid
	data sccsid /'@(#)func.f	31.1	8/26/88'/

      func=0.0
      goto(10,20,30,40,50,60,70,80,90),i
      return
   10 func=1.0
      goto100
   20 func=x
      goto100
   30 func=z
      goto100
   40 func=x*x
      goto100
   50 func=z*x
      goto100
   60 func=z*z
      goto100
   70 func=x*x*x
      goto100
   80 func=x*x*z
      goto100
   90 func=x*z*z
  100 return
      end
