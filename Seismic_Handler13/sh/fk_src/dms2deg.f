c	subroutine dms2deg(deg,min,sec,degree)
c
c CREATION DATE: 	9. January 1992
c
c
c		C H A N G E  L O G
c 
c      Date     |       Name     |         Description
c---------------|----------------|-------------------------------------
c 09-01-92      |     Henger	 |  Version 1.0
c---------------|----------------|-------------------------------------
c

	subroutine dms2deg(deg,min,sec,degree)

c	subroutine to convert coordinate values given in
c	deg(rees),min(utes) and sec(onds) into degree(s) and
c	fractions of degrees
c
	real deg,min,sec,degree        

	degree = deg + min/60. + sec/3600.

	return
	end
