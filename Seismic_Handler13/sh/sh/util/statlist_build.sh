#!/bin/sh

####################################################################
# STATLIST_BUILD Ver. 1.0   Karl Koch, BGR-B3.11 Date: Jan 30,1997 #
# This command file reads an SFD file and build from the stations  #
# included there a STATLIST_SFD.STX file that can be used in SHM   #
# There will be one Array button called SFDFILE                    #
#                                                                  #
# =======>  Seismological Data Analysis Center  <===============   #
#                                                                  #
# For command syntax type: statlist_build -h                       #
#                                                                  #
# Last Updated: Jan 30, 1997                                       #
####################################################################

####################################################################
# Step 1: Get command line parameters 
####################################################################

if [ "$#" -gt "0" ]
then
	case $1 in
		-h) echo "usage: statlist_build [-h] [sfdfile1 sfdfile2 .... sfdfileN]";
      	echo " ";
      	echo "Option: -h...... this commmand syntax help";
      	echo "  ";
      	echo "   other command line paramters are assumed";
      	echo "   to be sfdfiles; if no parameters are given";
      	echo "   then ./sfdfile.sfd  is assumed";
      	exit 0;;
		*) FILES="$*";;
	esac
else
		FILES="./sfdfile.sfd"
fi

####################################################################
# Step 2: Do the job ; store the result in STATLIST_SFD.STX
####################################################################

umask 002             # Remove write permission for 'others'

echo "! file STATLIST_SFD.STX"   >  STATLIST_SFD.STX
echo "! !      ================" >> STATLIST_SFD.STX
echo "! !"                       >> STATLIST_SFD.STX
echo "! ! version 1, 18-Aug-94"  >> STATLIST_SFD.STX
echo "! !"                       >> STATLIST_SFD.STX
echo "! ! Setup for read dialog box from SFD file" >> STATLIST_SFD.STX
echo "! ! K. Koch, 30-Jan-94 [`date '+%d-%h-%y'`]" >> STATLIST_SFD.STX
echo ""                          >> STATLIST_SFD.STX
echo "set1: 0 29 SFDFILE"        >> STATLIST_SFD.STX
echo ""                          >> STATLIST_SFD.STX

# The head -30 is used in order to avoid messy SHM errors
cat $FILES | dd conv=ucase 2> /dev/null | grep 'S>' | \
	cut -d' ' -f1 | cut -d'>' -f2 | cut -d'-' -f1 | sort -u | \
	awk '{print "* "$1}' | head -30 >> STATLIST_SFD.STX
