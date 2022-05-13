c
      subroutine rgsrtm (delta,z,h,v,nlayers, t,ierr)
c
c  This subroutine calculates a single travel time (t) 
c    at a given distance for a given plane-layer
c    velocity model.  In other words, it is useful for
c    computing travel-times for regional and surface waves.
c
c  If delta given is less than critical distance for the layer
c    model, program will return T-T for next more shallow layer.
c
c  Inputs:
c    delta : epicenter-station distance (degrees)
c    z : source depth 
c    h(n), n=1,nlayer : array of layer thickness (km)
c    v(n), n=1,nlayer+1 : array of velocities (km/s)
c    nlayers : number of layers (=0 for Pg, Lg)
c
c  Outputs:
c    t : travel time
c    ierr : = 0 if OK, = 1 if distance less than critical distance
c           for layer model.
c
c

      parameter (maxlayer = 100)

      dimension v(maxlayer+1),h(maxlayer),d(maxlayer)

	common /sccsrgsrtm/ sccsid
	character*80 sccsid
	data sccsid /'@(#)rgsrtm.f	31.1	8/26/88'/

c
      dgrdkm = 111.195
      x = delta*dgrdkm
      nlayer = nlayers
c
1     continue
c
      if (nlayer.eq.0) then
        t = sqrt(z**2 + x**2)/v(1)
        return
      endif
c
c Compute depth to bottom of each layer
c
      do 20  n = 1,nlayer
        d(n) = 0.0
        do 15  n2 = 1,n
          d(n) = d(n) + h(n2)
15      continue
20    continue
c
c If source in the half space, send back garbage
c 
      if (z.gt.d(nlayer)) then
        t = -1.0
	ierr = 1
        return
      endif
c                
c Determine source layer
c
      if (z.gt.d(1)) then
         do 100 n = 1,nlayer-1
            if (z.gt.d(n) .and. z.le.d(n+1)) layer = n + 1
 100     continue
      else
         layer = 1
      endif
c
c Calculate where to start iterating for the downward wave
c
      hsave = h(layer)
      h(layer) = d(layer) - z
c
c Calculate downward and upward travel times
c
      sumdown = 0.0
      sumdownc = 0.0
c
      do 150 n = layer,nlayer
        save = sqrt(v(nlayer+1)**2-v(n)**2)/v(n)
        sumdown = sumdown + h(n)*save/v(nlayer+1) 
	sumdownc = sumdownc + h(n)/save
  150 continue
c
      h(layer) = hsave
      sumup = 0.0
      sumupc = 0.0
      do 200 n = 1,nlayer
        save = sqrt(v(nlayer+1)**2-v(n)**2)/v(n)
        sumup = sumup + h(n)*save/v(nlayer+1) 
	sumupc = sumupc + h(n)/save
 200  continue
c
c If the station-event distance is less than the critical distance
c  return to top of subroutine and assume wave travels in next most
c  shallow layer.
c
	 xcrit = sumdownc + sumupc
	 if (x.ge.xcrit) then
           t0 = x/v(nlayer+1)
           t = t0 + sumup + sumdown
         else
	   nlayer = nlayer - 1
	   ierr = 1
	   goto 1
         endif
 999  return
      end
