c*    subroutine deepeq
c*
c*    checks if the epicenter lies in a zone
c*    where deep earthquaques are common
c*
c*    input:
c*
c*      alat,alon   epicenter of the event
c*                  geocentrical coordinates
c*
c*    output:
c*
c*       ndeep      = 1/4 if the epic is outside/inside a deep zone
c*
c*    no other subroutine called
c*
c**********************************************************

        subroutine deepeq (alat,alon, ndeep)

        real alat,alon

        integer ndeep

        integer i,j,ii
        real dlat(30),dlon(30)
        character*80 sccsid
        save sccsid
        data sccsid /'@(#)deepevent.f	44.2  11/6/91'/
        data dlat/110.,90.,90.,70.,70.,50.,50.,30.,136.,110.,136.,
     &  85.,85.,64.,45.,20.,130.,96.,46.,34.,156.,140.,70.,55.,60.,
     &  46.,60.,45.,45.,39./
        data dlon/90.,180.,85.,150.,114.,150.,134.,166.,165.,180.,
     &  -84.,-60.,-110.,-56.,-180.,-145.,-180.,-174.,166.,180.,
     &  -35.,-20.,87.,102.,65.,81.,9.,40.,20.,33./

        ndeep = 1
        i = -1
        do 100 ii = 1,15
          i = i+2
          j = i+1
          if (alat.lt.dlat(i) .and. alat.gt.dlat(j) .and. 
     &    alon.gt.dlon(i) .and. alon.lt.dlon(j)) then
            ndeep = 4
            return
          end if
100     continue

        return
        end
