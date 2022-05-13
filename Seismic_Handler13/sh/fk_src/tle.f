c Unterprogramm zur Bestimmumg der Anzahl der Charakter
c eines Strings (von rechts)
c
      integer function tle(t)
      character t*(*)
           do 1 tle=len(t),1,-1
              if (t(tle:tle).ne.' ') return
1          continue
      tle=1
      end
