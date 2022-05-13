#! /bin/csh
#
# file create_beamloc.csh
#      ==================
#
# version 1, 12-Mar-2005
#
# Creates file with beam locations
# K. Stammler, 12-Mar-2005

set lat=33
while  ($lat <= 60)
	set lon=-4
	while  ($lon <= 26)
		printf "%5.1f %5.1f\n" $lat $lon
		@ lon ++
	end
	@ lat ++
end
