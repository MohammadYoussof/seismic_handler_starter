      block data constants
      common/numcon/dpd2r ,dpr2d ,chisq ,dg2km ,dg2rd ,km2dg ,pi    ,
     *              radeth,rd2dg ,szmax ,szmin ,twopi ,znorm
      real*8    dpd2r ,dpr2d
      real*4    chisq ,dg2km ,dg2rd ,km2dg ,pi    ,radeth,rd2dg ,
     *          szmax ,szmin ,twopi ,znorm

	common /sccsconstants/ sccsid
	character*80 sccsid
	data sccsid /'@(#)constants.f	31.1	8/26/88'/

c
      data dpd2r,dpr2d  /0.1745329252d-01 , 57.29577951d+00 /
      data dg2km,dg2rd  /111.1954041      , 0.01745329252  /
      data km2dg,pi     /0.8993174881e-02 , 3.141592026     /
      data radeth,rd2dg /6371.027344      , 57.29577637     /
      data szmax ,szmin /110.000000       , 105.000000      /
      data twopi        /6.283185005      /
      end
