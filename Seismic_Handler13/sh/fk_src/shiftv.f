       SUBROUTINE SHIFTV(R,GAMMA,N,VS,THETA,TV,DT,ITV)
C
C   SHIFTV COMPUTES THE DELAYS FOR SIGNALS WITH A GIVEN AZIMUTH THETA
C   AND APP. SIGNAL VELOCITY VS
C
C      INPUT:
C      R(K) = DISTANCE OF SEISMOMETER STATION K FROM REFERENCE STATION [KM]
C      GAMMA = AZIMUTH OF SEISMOMETER STATION K WITH RESPECT TO REFERENCE STATION
C      N = TOTAL NUMBER OF SEISMOMETER STATIONS
C      VS = SIGNAL VELOCITY 
C      THETA = SIGNAL AZIMUTH
C      DT = SAMPLING DISTANCE in SECs (at 20 Hz --> 0.05)
C
C      OUTPUT:  TV = delay in milliseconds
C               ITV= delay in samples
C
C
       DIMENSION R(*),GAMMA(*),TV(*),ITV(*)
       PI = 3.141592654

CC     VS=(6370.*PI)/(SL*180.)		     ! if slowness is used instead of velocity

       ITVM=-9999
       DO 1 K=1,N
          DIFW = THETA - GAMMA(K)
          DIFRAD = PI/180.0*DIFW
          TV(K) = -R(K)/VS*COS(DIFRAD)
          ITV(K) = IFIX((TV(K) + 0.5*DT)/DT)
          TV(k) = TV(k)*1000.				            ! Convert to milliseconds
          IF(ITV(K).LT.0) ITV(K)=ITV(K)-1
          IF(IABS(ITV(K)).LT.IABS(ITVM)) ITVM=ITV(K)  	! maximum delay
 1     CONTINUE
       IF(ITVM.NE.0) THEN
          DO 2 I=1,N
2         ITV(I)=ITV(I)-ITVM                   	    	! negative delays
       ENDIF
999    RETURN
       END
