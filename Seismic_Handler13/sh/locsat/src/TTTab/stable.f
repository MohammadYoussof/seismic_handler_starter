      block data stable
c
c        block data definition of the seismic s phase and other later
c         phases
c
c         programmer - p. kovacs
c
c          date    - 20 aug 75 (last update june 1978)
c
c     table     definition
c
c        tsth     s phase travel times in seconds for the depths as
c                  shown
c     ibfac        b factors stored times 10 for the depths as shown
c        phase name table
c
c     index and name of waves
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
c The last seven phases are computed with a separate
c  velocity stucture (in file velocity.dat)
c  that may be changed by the user.
c
c Also note that no amplitudes are computed for 
c  surface or regional phases
c  at this time.
c
c
c**********************************************************************
c
      include '../../include/phtbls.h'
c**********************************************************************
c
c Core phase travel time coefficients
c

	common /sccsstable/ sccsid
	character*80 sccsid
	data sccsid /'@(#)stable.f	31.1	8/26/88'/

      data phtc(1, 1),phtc(2, 1)/  7.254667426e 02, -2.082544817e-01/
      data phtc(3, 1),phtc(4, 1)/ -2.440611328e-01,  7.608578025e-02/
      data phtc(5, 1),phtc(6, 1)/  2.459712427e-04,  5.436490678e-05/
      data phtc(7, 1)           / -4.179948544e-04/
      data phtc(1, 2),phtc(2, 2)/  5.142206068e 02, -9.766975994e-02/
      data phtc(3, 2),phtc(4, 2)/ -1.369676559e-01,  5.494509728e-02/
      data phtc(5, 2),phtc(6, 2)/  1.271554068e-04,  3.227063238e-05/
      data phtc(7, 2)           / -2.318873039e-04/
      data phtc(1, 3),phtc(2, 3)/  1.402018852e 03, -5.402862879e 00/
      data phtc(3, 3),phtc(4, 3)/ -2.371832023e-01,  6.241608967e-02/
      data phtc(5, 3),phtc(6, 3)/ -1.611618919e-05,  5.126166133e-05/
      data phtc(7, 3)           / -1.765727112e-04/
      data phtc(1, 4),phtc(2, 4)/  1.273836037e 03, -5.072669501e 00/
      data phtc(3, 4),phtc(4, 4)/ -2.405364233e-01,  6.007655541e-02/
      data phtc(5, 4),phtc(6, 4)/  2.528757708e-05,  5.345022548e-05/
      data phtc(7, 4)           / -1.247956774e-04/
      data phtc(1, 5),phtc(2, 5)/  2.187460900e 03, -2.571896400e 01/
      data phtc(3, 5),phtc(4, 5)/ -1.388234800e-01,  1.830537900e-01/
      data phtc(5, 5),phtc(6, 5)/  6.956784600e-05,  3.125239200e-05/
      data phtc(7, 5)           / -3.708582400e-04/
      data phtc(1, 6),phtc(2, 6)/  1.909183331e 03,  9.180929113e-02/
      data phtc(3, 6),phtc(4, 6)/ -1.350494517e-01, -1.446038080e-02/
      data phtc(5, 6),phtc(6, 6)/  1.647321354e-05,  3.023835715e-05/
      data phtc(7, 6)           /  3.396644165e-05/
      data phtc(1, 7),phtc(2, 7)/  2.232263061e 03, -9.600871515e 00/
      data phtc(3, 7),phtc(4, 7)/ -1.325299524e-01,  8.865464470e-02/
      data phtc(5, 7),phtc(6, 7)/  7.524314827e-07,  3.033763313e-05/
      data phtc(7, 7)           / -3.557996733e-04/
      data phtc(1, 8),phtc(2, 8)/  2.281850197e 03, -3.997163819e 00/
      data phtc(3, 8),phtc(4, 8)/ -1.380180246e-01, -9.579527337e-03/
      data phtc(5, 8),phtc(6, 8)/  3.169570063e-05,  2.997138851e-05/
      data phtc(7, 8)           /  4.630752124e-05/
      data phtc(1, 9),phtc(2, 9)/  2.423680140e 03,  6.082231564e-02/
      data phtc(3, 9),phtc(4, 9)/ -1.350965633e-01, -1.804953947e-02/
      data phtc(5, 9),phtc(6, 9)/  1.526871915e-05,  3.020428805e-05/
      data phtc(7, 9)           /  5.402784165e-05/
      data phtc(1,10),phtc(2,10)/  7.251888088e 02, -1.677314681e-01/
      data phtc(3,10),phtc(4,10)/ -1.369773008e-01,  7.500988855e-02/
      data phtc(5,10),phtc(6,10)/  1.359912734e-04,  3.200089168e-05/
      data phtc(7,10)           / -4.091787989e-04/
      data phtc(1,11),phtc(2,11)/  9.358827037e 02, -1.353275397e-01/
      data phtc(3,11),phtc(4,11)/ -2.442724766e-01,  9.958057992e-02/
      data phtc(5,11),phtc(6,11)/  2.374554161e-04,  5.491078948e-05/
      data phtc(7,11)           / -4.079733919e-04/
      data phtc(1,12),phtc(2,12)/  3.580828656e 02,  1.871407732e 01/
      data phtc(3,12),phtc(4,12)/ -2.149156413e-01, -9.198716421e-02/
      data phtc(5,12),phtc(6,12)/ -2.024097057e-04,  5.329453918e-05/
      data phtc(7,12)           /  1.563046054e-04/
      data phtc(1,13),phtc(2,13)/  1.544465239e 03, -3.713039423e 00/
      data phtc(3,13),phtc(4,13)/ -2.310455994e-01,  4.992760588e-02/
      data phtc(5,13),phtc(6,13)/ -5.803007959e-05,  5.210043834e-05/
      data phtc(7,13)           / -1.475934031e-04/
      data phtc(1,14),phtc(2,14)/ -1.149004400e 03,  3.428346300e 01/
      data phtc(3,14),phtc(4,14)/ -1.063704600e-01, -1.631386900e-01/
      data phtc(5,14),phtc(6,14)/ -1.599832800e-04,  2.821349100e-05/
      data phtc(7,14)           /  2.574722700e-04/
      data phtc(1,15),phtc(2,15)/  1.051468300e 03, -1.377200100e 00/
      data phtc(3,15),phtc(4,15)/ -1.323626900e-01,  2.357844600e-02/
      data phtc(5,15),phtc(6,15)/  7.203753700e-06,  2.794980000e-05/
      data phtc(7,15)           / -5.366589200e-05/
c
c        surface-reflected phase distance coefficients for travel times
c
      data phdc(1, 1),phdc(2, 1)/ -3.731134930e-02, -3.877315448e-02/
      data phdc(3, 1),phdc(4, 1)/  9.442820378e-03,  4.575160257e-04/
      data phdc(5, 1)           / -8.752249696e-05/
      data phdc(1, 2),phdc(2, 2)/ -6.164515021e-01,  5.133082853e-01/
      data phdc(3, 2),phdc(4, 2)/ -7.788187919e-03, -7.242635723e-05/
      data phdc(5, 2)           /  3.824943797e-05/
      data phdc(1, 3),phdc(2, 3)/ -2.718837707e 00,  3.956497495e-01/
      data phdc(3, 3),phdc(4, 3)/ -1.504233400e-02, -2.730736392e-04/
      data phdc(5, 3)           /  6.085509950e-05/
      data phdc(1, 4),phdc(2, 4)/  1.332422511e 01,  6.710060937e-01/
      data phdc(3, 4),phdc(4, 4)/ -5.430527057e-03, -2.905019120e-04/
      data phdc(5, 4)           /  3.081186969e-05/
      data phdc(1, 5),phdc(2, 5)/  2.117568768e 01,  4.761407454e-01/
      data phdc(3, 5),phdc(4, 5)/ -6.829605473e-03, -1.396958431e-04/
      data phdc(5, 5)           /  2.809666608e-05/
      data phdc(1, 6),phdc(2, 6)/  5.094041020e 00, -1.112013269e-01/
      data phdc(3, 6),phdc(4, 6)/  2.200501812e-02,  6.357961853e-04/
      data phdc(5, 6)           / -1.487828810e-04/
      data phdc(1, 7),phdc(2, 7)/  2.380033769e-01, -2.638387408e-02/
      data phdc(3, 7),phdc(4, 7)/  1.698437288e-02,  2.383813168e-04/
      data phdc(5, 7)           / -1.304718225e-04/
      data phdc(1, 8),phdc(2, 8)/ -2.820651605e 01,  5.894273495e-01/
      data phdc(3, 8),phdc(4, 8)/ -1.160324113e-02, -6.783427343e-04/
      data phdc(5, 8)           / -5.672742767e-07/
      data phdc(1, 9),phdc(2, 9)/ -4.900246297e 00,  2.780563863e-01/
      data phdc(3, 9),phdc(4, 9)/ -2.260741383e-02,  1.036670011e-03/
      data phdc(5, 9)           /  1.117761279e-04/
      data phdc(1,10),phdc(2,10)/ -5.422026555e-01,  5.107501496e-01/
      data phdc(3,10),phdc(4,10)/ -7.624231472e-03, -5.132492009e-05/
      data phdc(5,10)           /  3.390520527e-05/
      data phdc(1,11),phdc(2,11)/ -2.613331783e 00,  4.029352657e-01/
      data phdc(3,11),phdc(4,11)/ -9.692201732e-03, -2.858965384e-04/
      data phdc(5,11)           /  3.589791145e-05/
c
c
c        s phase travel time tables
c
c        depth = surface
c
      data tsth( 1,1),tsth( 2,1),tsth( 3,1)/  -3.299,   0.000,   3.300/
      data tsth( 4,1),tsth( 5,1),tsth( 6,1)/   6.600,   9.900,  16.500/
      data tsth( 7,1),tsth( 8,1),tsth( 9,1)/  19.800,  33.000,  36.250/
      data tsth(10,1),tsth(11,1),tsth(12,1)/  39.500,  51.300,  53.846/
      data tsth(13,1),tsth(14,1),tsth(15,1)/  56.392,  58.937,  61.482/
      data tsth(16,1),tsth(17,1),tsth(18,1)/  89.441, 114.702, 149.901/
      data tsth(19,1),tsth(20,1),tsth(21,1)/ 187.587, 237.422, 298.919/
      data tsth(22,1),tsth(23,1),tsth(24,1)/ 335.300, 406.400, 452.300/
      data tsth(25,1),tsth(26,1),tsth(27,1)/ 497.100, 536.199, 588.900/
      data tsth(28,1),tsth(29,1),tsth(30,1)/ 622.100, 646.205, 740.427/
      data tsth(31,1),tsth(32,1),tsth(33,1)/ 809.393, 897.900, 975.514/
      data tsth(34,1),tsth(35,1),tsth(36,1)/1056.816,1128.100,1189.900/
      data tsth(37,1),tsth(38,1),tsth(39,1)/1231.469,1282.545,1346.900/
      data tsth(40,1),tsth(41,1),tsth(42,1)/1372.419,1425.300,1469.700/
      data tsth(43,1),tsth(44,1),tsth(45,1)/1507.813,1603.544,1607.699/
c
c        depth =  15 km
c
      data tsth( 1,2),tsth( 2,2),tsth( 3,2)/   6.099,   4.451,   5.538/
      data tsth( 4,2),tsth( 5,2),tsth( 6,2)/   7.952,  10.831,  16.670/
      data tsth( 7,2),tsth( 8,2),tsth( 9,2)/  19.535,  31.630,  34.565/
      data tsth(10,2),tsth(11,2),tsth(12,2)/  37.515,  48.477,  51.019/
      data tsth(13,2),tsth(14,2),tsth(15,2)/  53.564,  56.109,  58.653/
      data tsth(16,2),tsth(17,2),tsth(18,2)/  86.600, 111.825, 147.022/
      data tsth(19,2),tsth(20,2),tsth(21,2)/ 184.698, 234.490, 295.927/
      data tsth(22,2),tsth(23,2),tsth(24,2)/ 332.253, 403.248, 449.073/
      data tsth(25,2),tsth(26,2),tsth(27,2)/ 493.683, 532.514, 585.073/
      data tsth(28,2),tsth(29,2),tsth(30,2)/ 618.229, 642.315, 736.494/
      data tsth(31,2),tsth(32,2),tsth(33,2)/ 805.430, 893.894, 971.471/
      data tsth(34,2),tsth(35,2),tsth(36,2)/1052.741,1123.990,1185.757/
      data tsth(37,2),tsth(38,2),tsth(39,2)/1227.306,1278.357,1342.674/
      data tsth(40,2),tsth(41,2),tsth(42,2)/1368.178,1421.029,1465.404/
      data tsth(43,2),tsth(44,2),tsth(45,2)/1503.509,1599.237,1603.391/
c
c        depth =  40 km
c
      data tsth( 1,3),tsth( 2,3),tsth( 3,3)/  11.260,  10.834,  11.239/
      data tsth( 4,3),tsth( 5,3),tsth( 6,3)/  12.371,  14.047,  18.349/
      data tsth( 7,3),tsth( 8,3),tsth( 9,3)/  20.741,  30.729,  33.256/
      data tsth(10,3),tsth(11,3),tsth(12,3)/  35.772,  45.906,  48.494/
      data tsth(13,3),tsth(14,3),tsth(15,3)/  51.091,  53.680,  56.242/
      data tsth(16,3),tsth(17,3),tsth(18,3)/  83.979, 109.124, 144.312/
      data tsth(19,3),tsth(20,3),tsth(21,3)/ 181.916, 231.542, 292.789/
      data tsth(22,3),tsth(23,3),tsth(24,3)/ 328.943, 399.649, 445.310/
      data tsth(25,3),tsth(26,3),tsth(27,3)/ 489.374, 527.682, 579.948/
      data tsth(28,3),tsth(29,3),tsth(30,3)/ 613.005, 637.064, 731.153/
      data tsth(31,3),tsth(32,3),tsth(33,3)/ 800.028, 888.400, 965.903/
      data tsth(34,3),tsth(35,3),tsth(36,3)/1047.107,1118.284,1179.985/
      data tsth(37,3),tsth(38,3),tsth(39,3)/1221.495,1272.496,1336.736/
      data tsth(40,3),tsth(41,3),tsth(42,3)/1362.211,1415.000,1459.328/
      data tsth(43,3),tsth(44,3),tsth(45,3)/1497.415,1593.137,1597.291/
c
c        depth =  50 km
c
      data tsth( 1,4),tsth( 2,4),tsth( 3,4)/  13.442,  13.119,  13.433/
      data tsth( 4,4),tsth( 5,4),tsth( 6,4)/  14.333,  15.713,  19.442/
      data tsth( 7,4),tsth( 8,4),tsth( 9,4)/  21.605,  31.082,  33.543/
      data tsth(10,4),tsth(11,4),tsth(12,4)/  36.018,  46.000,  48.506/
      data tsth(13,4),tsth(14,4),tsth(15,4)/  51.014,  53.524,  56.034/
      data tsth(16,4),tsth(17,4),tsth(18,4)/  83.797, 108.949, 144.095/
      data tsth(19,4),tsth(20,4),tsth(21,4)/ 181.503, 231.064, 292.165/
      data tsth(22,4),tsth(23,4),tsth(24,4)/ 328.198, 398.723, 444.297/
      data tsth(25,4),tsth(26,4),tsth(27,4)/ 488.031, 526.111, 578.239/
      data tsth(28,4),tsth(29,4),tsth(30,4)/ 611.248, 635.300, 729.345/
      data tsth(31,4),tsth(32,4),tsth(33,4)/ 798.192, 886.519, 963.990/
      data tsth(34,4),tsth(35,4),tsth(36,4)/1045.162,1116.306,1177.977/
      data tsth(37,4),tsth(38,4),tsth(39,4)/1219.468,1270.447,1334.651/
      data tsth(40,4),tsth(41,4),tsth(42,4)/1360.114,1412.875,1457.180/
      data tsth(43,4),tsth(44,4),tsth(45,4)/1495.261,1590.980,1595.134/
c
c        depth = 100 km
c
      data tsth( 1,5),tsth( 2,5),tsth( 3,5)/  24.586,  24.439,  24.585/
      data tsth( 4,5),tsth( 5,5),tsth( 6,5)/  25.020,  25.727,  27.864/
      data tsth( 7,5),tsth( 8,5),tsth( 9,5)/  29.239,  36.169,  38.155/
      data tsth(10,5),tsth(11,5),tsth(12,5)/  40.210,  48.930,  51.198/
      data tsth(13,5),tsth(14,5),tsth(15,5)/  53.491,  55.806,  58.139/
      data tsth(16,5),tsth(17,5),tsth(18,5)/  84.503, 108.947, 143.378/
      data tsth(19,5),tsth(20,5),tsth(21,5)/ 180.217, 229.027, 289.213/
      data tsth(22,5),tsth(23,5),tsth(24,5)/ 324.772, 394.287, 439.448/
      data tsth(25,5),tsth(26,5),tsth(27,5)/ 481.231, 518.362, 569.842/
      data tsth(28,5),tsth(29,5),tsth(30,5)/ 602.603, 626.639, 720.465/
      data tsth(31,5),tsth(32,5),tsth(33,5)/ 789.164, 877.262, 954.568/
      data tsth(34,5),tsth(35,5),tsth(36,5)/1035.573,1106.550,1168.066/
      data tsth(37,5),tsth(38,5),tsth(39,5)/1209.461,1260.327,1324.352/
      data tsth(40,5),tsth(41,5),tsth(42,5)/1349.746,1402.368,1446.561/
      data tsth(43,5),tsth(44,5),tsth(45,5)/1484.606,1580.311,1584.465/
c
c        depth = 200 km
c
      data tsth( 1,6),tsth( 2,6),tsth( 3,6)/  46.607,  46.538,  46.607/
      data tsth( 4,6),tsth( 5,6),tsth( 6,6)/  46.814,  47.156,  48.233/
      data tsth( 7,6),tsth( 8,6),tsth( 9,6)/  48.960,  52.984,  54.237/
      data tsth(10,6),tsth(11,6),tsth(12,6)/  55.576,  61.669,  63.348/
      data tsth(13,6),tsth(14,6),tsth(15,6)/  65.080,  66.861,  68.685/
      data tsth(16,6),tsth(17,6),tsth(18,6)/  90.821, 112.810, 144.830/
      data tsth(19,6),tsth(20,6),tsth(21,6)/ 179.771, 226.635, 284.707/
      data tsth(22,6),tsth(23,6),tsth(24,6)/ 319.074, 386.726, 429.111/
      data tsth(25,6),tsth(26,6),tsth(27,6)/ 467.514, 503.369, 553.869/
      data tsth(28,6),tsth(29,6),tsth(30,6)/ 586.159, 610.154, 703.542/
      data tsth(31,6),tsth(32,6),tsth(33,6)/ 771.913, 859.523, 936.475/
      data tsth(34,6),tsth(35,6),tsth(36,6)/1017.106,1087.729,1148.917/
      data tsth(37,6),tsth(38,6),tsth(39,6)/1190.101,1240.687,1304.388/
      data tsth(40,6),tsth(41,6),tsth(42,6)/1329.632,1381.961,1425.921/
      data tsth(43,6),tsth(44,6),tsth(45,6)/1463.906,1559.565,1563.721/
c
c        depth = 350 km
c
      data tsth( 1,7),tsth( 2,7),tsth( 3,7)/  78.352,  78.315,  78.352/
      data tsth( 4,7),tsth( 5,7),tsth( 6,7)/  78.463,  78.648,  79.236/
      data tsth( 7,7),tsth( 8,7),tsth( 9,7)/  79.637,  81.934,  82.673/
      data tsth(10,7),tsth(11,7),tsth(12,7)/  83.474,  87.270,  88.356/
      data tsth(13,7),tsth(14,7),tsth(15,7)/  89.493,  90.680,  91.913/
      data tsth(16,7),tsth(17,7),tsth(18,7)/ 108.053, 125.659, 152.975/
      data tsth(19,7),tsth(20,7),tsth(21,7)/ 184.092, 226.916, 280.982/
      data tsth(22,7),tsth(23,7),tsth(24,7)/ 313.010, 374.450, 412.391/
      data tsth(25,7),tsth(26,7),tsth(27,7)/ 448.388, 482.551, 531.774/
      data tsth(28,7),tsth(29,7),tsth(30,7)/ 563.797, 587.464, 680.367/
      data tsth(31,7),tsth(32,7),tsth(33,7)/ 748.103, 834.880, 911.236/
      data tsth(34,7),tsth(35,7),tsth(36,7)/ 991.285,1061.289,1121.941/
      data tsth(37,7),tsth(38,7),tsth(39,7)/1162.751,1212.890,1276.084/
      data tsth(40,7),tsth(41,7),tsth(42,7)/1301.058,1352.852,1396.511/
      data tsth(43,7),tsth(44,7),tsth(45,7)/1434.420,1530.031,1534.188/
c
c        depth = 600 km
c
      data tsth( 1,8),tsth( 2,8),tsth( 3,8)/ 126.317, 126.298, 126.317/
      data tsth( 4,8),tsth( 5,8),tsth( 6,8)/ 126.375, 126.472, 126.781/
      data tsth( 7,8),tsth( 8,8),tsth( 9,8)/ 126.993, 128.218, 128.617/
      data tsth(10,8),tsth(11,8),tsth(12,8)/ 129.053, 131.152, 131.764/
      data tsth(13,8),tsth(14,8),tsth(15,8)/ 132.410, 133.089, 133.801/
      data tsth(16,8),tsth(17,8),tsth(18,8)/ 143.623, 155.256, 174.741/
      data tsth(19,8),tsth(20,8),tsth(21,8)/ 198.434, 232.639, 277.200/
      data tsth(22,8),tsth(23,8),tsth(24,8)/ 304.065, 356.633, 390.522/
      data tsth(25,8),tsth(26,8),tsth(27,8)/ 423.680, 455.916, 503.633/
      data tsth(28,8),tsth(29,8),tsth(30,8)/ 534.869, 558.057, 649.657/
      data tsth(31,8),tsth(32,8),tsth(33,8)/ 716.018, 801.240, 876.331/
      data tsth(34,8),tsth(35,8),tsth(36,8)/ 955.037,1023.787,1083.481/
      data tsth(37,8),tsth(38,8),tsth(39,8)/1123.428,1172.770,1234.822/
      data tsth(40,8),tsth(41,8),tsth(42,8)/1259.297,1310.014,1353.194/
      data tsth(43,8),tsth(44,8),tsth(45,8)/1391.006,1486.511,1490.676/
c
c        depth = 800 km
c
      data tsth( 1,9),tsth( 2,9),tsth( 3,9)/ 160.159, 160.146, 160.159/
      data tsth( 4,9),tsth( 5,9),tsth( 6,9)/ 160.199, 160.265, 160.475/
      data tsth( 7,9),tsth( 8,9),tsth( 9,9)/ 160.620, 161.458, 161.733/
      data tsth(10,9),tsth(11,9),tsth(12,9)/ 162.032, 163.483, 163.907/
      data tsth(13,9),tsth(14,9),tsth(15,9)/ 164.356, 164.830, 165.327/
      data tsth(16,9),tsth(17,9),tsth(18,9)/ 172.302, 180.813, 195.585/
      data tsth(19,9),tsth(20,9),tsth(21,9)/ 214.248, 242.230, 280.058/
      data tsth(22,9),tsth(23,9),tsth(24,9)/ 303.501, 350.954, 382.501/
      data tsth(25,9),tsth(26,9),tsth(27,9)/ 413.818, 444.940, 491.239/
      data tsth(28,9),tsth(29,9),tsth(30,9)/ 521.992, 544.788, 633.947/
      data tsth(31,9),tsth(32,9),tsth(33,9)/ 698.625, 782.045, 855.774/
      data tsth(34,9),tsth(35,9),tsth(36,9)/ 932.960,1000.396,1058.942/
      data tsth(37,9),tsth(38,9),tsth(39,9)/1098.130,1146.589,1207.493/
      data tsth(40,9),tsth(41,9),tsth(42,9)/1231.446,1281.232,1323.935/
      data tsth(43,9),tsth(44,9),tsth(45,9)/1361.674,1457.120,1461.288/
c
c
c        surface-reflected phase distance limit coefficients
c
      data phco(1, 1),phco(2, 1)/  1.0609283e 00, -6.6538379e-03/
      data phco(3, 1),phco(4, 1)/  1.1436305e-04, -3.7843932e-07/
      data phco(5, 1),phco(6, 1)/  5.4256111e-10, -2.6971844e-13/
      data phco(7, 1),phco(8, 1)/  9.6500000e 01,  5.0000000e-03/
      data phco(1, 2),phco(2, 2)/ -1.2931313e 01,  5.3176677e-01/
      data phco(3, 2),phco(4, 2)/ -2.8067487e-03,  7.1353761e-06/
      data phco(5, 2),phco(6, 2)/ -8.4690868e-09,  3.8106098e-12/
      data phco(7, 2),phco(8, 2)/  1.9700000e 02, -3.7500000e-03/
      data phco(1, 3),phco(2, 3)/  1.4178362e 00,  1.7880756e-01/
      data phco(3, 3),phco(4, 3)/  3.6767405e-04, -2.5536883e-06/
      data phco(5, 3),phco(6, 3)/  3.9001868e-09, -1.7959627e-12/
      data phco(7, 3),phco(8, 3)/  2.2000000e 02,  0.0000000e 00/
      data phco(1, 4),phco(2, 4)/  4.9074615e 01,  2.3114055e-02/
      data phco(3, 4),phco(4, 4)/ -2.4014301e-04,  7.5358417e-07/
      data phco(5, 4),phco(6, 4)/ -1.0345198e-09,  5.1289344e-13/
      data phco(7, 4),phco(8, 4)/  1.3800000e 02,  5.0000000e-03/
      data phco(1, 5),phco(2, 5)/  5.0062943e 01,  2.3742232e-02/
      data phco(3, 5),phco(4, 5)/ -2.1995022e-04,  5.2179223e-07/
      data phco(5, 5),phco(6, 5)/ -4.9546967e-10,  1.6032596e-13/
      data phco(7, 5),phco(8, 5)/  1.7600000e 02,  2.5000000e-03/
      data phco(1, 6),phco(2, 6)/  3.8413208e 01,  2.0823199e-01/
      data phco(3, 6),phco(4, 6)/ -6.2072952e-04,  1.4106790e-06/
      data phco(5, 6),phco(6, 6)/ -1.4945247e-09,  5.7110263e-13/
      data phco(7, 6),phco(8, 6)/  1.0700000e 02,  5.0000000e-03/
      data phco(1, 7),phco(2, 7)/ -2.3044109e 00,  2.3486745e-01/
      data phco(3, 7),phco(4, 7)/ -8.1254891e-04,  1.4729267e-06/
      data phco(5, 7),phco(6, 7)/ -1.3100447e-09,  4.8877775e-13/
      data phco(7, 7),phco(8, 7)/  1.0700000e 02,  3.7500000e-03/
      data phco(1, 8),phco(2, 8)/  4.5817108e 01,  7.3652089e-02/
      data phco(3, 8),phco(4, 8)/  3.8676476e-04, -1.8063265e-06/
      data phco(5, 8),phco(6, 8)/  2.9296843e-09, -1.5943800e-12/
      data phco(7, 8),phco(8, 8)/  1.3700000e 02, -6.2500000e-03/
      data phco(1, 9),phco(2, 9)/  4.7208511e 01,  2.1892202e-01/
      data phco(3, 9),phco(4, 9)/  1.4879042e-05, -1.8430128e-06/
      data phco(5, 9),phco(6, 9)/  4.1163268e-09, -2.5802971e-12/
      data phco(7, 9),phco(8, 9)/  1.8000000e 02, -1.2857143e-02/
      data phco(1,10),phco(2,10)/  1.9509258e 00,  1.4536095e-01/
      data phco(3,10),phco(4,10)/ -1.3281591e-05, -1.1157226e-06/
      data phco(5,10),phco(6,10)/  2.1575144e-09, -1.1656223e-12/
      data phco(7,10),phco(8,10)/  1.9300000e 02,  1.5000000e-02/
      data phco(1,11),phco(2,11)/  1.3764658e 00,  2.0544976e-01/
      data phco(3,11),phco(4,11)/  1.8054503e-04, -1.9400140e-06/
      data phco(5,11),phco(6,11)/  3.0335043e-09, -1.4026480e-12/
      data phco(7,11),phco(8,11)/  2.2000000e 02,  0.0000000e 00/
c
c        core phase distance limits
c
      data phhc/80.,80.,180.,148.,180.,162.,126.,126.,140.,70.,80.,133.,
     1         180.,160.,158.0/
      data phlc/ 0., 0.,104.,130.,143.,  0., 93.,104., 0.,0.,0.,70.,99.,
     1         144.,105.0/
c
c        array of computed phase travel times
c
      data faze/34*0.0/
c
c        minimum and maximum tolerances for faze array
c
CCC GONCZ NEXT LINE MODIFIES .pp  12:10:81      data wndmn/ 6.0,13.0,13.0,11.0,11.0,18.0, 6.0,23.5,10.0,13.0,
      data wndmn/ 6.0,13.0,13.0,20.0,20.0,18.0, 6.0,23.5,10.0,13.0,
     1           21.0,21.0,12.0,12.0,12.0,12.0,14.0,14.0,14.0,27.0,
     2            0.0,12.0,12.0, 9.0, 9.0, 12.0,11.0,11.0,20.0,20.0,
     3           13.0,18.0,900.0,900.0/
CCC GONCZ MEXT LINE MODIFIES .pp 12:10:81      data wndmx/ 7.0,15.0,12.5, 9.0,11.0,20.0, 7.0,22.5,22.5,29.0,
      data wndmx/ 7.0,15.0,12.5,20.0,20.0,20.0, 7.0,22.5,22.5,29.0,
     1           19.0,19.0,15.0,15.0,15.0,15.0,22.0,22.0,22.0,23.0,
     2            0.0,18.0,21.5,22.0,44.0,22.0,22.0,22.0,20.0,25.0,
     3           29.0,40.0,2700.0,2700.0/
CCC GONCZ 23 FEB 82  sP  and S  ,ALSO CHANGED
c        b factors for computing magnitudes
c
c        references for magnitude b factors
c
c          distance range     reference
c            (degrees)
c              0-15           unknown
c             16-110          gutenberg and richter, ann. geofisica,
c                               v.9, pp. 1-15 (1956), decreased by 0.3
c                               to handle p-p rather than o-p.
c            111-180          sweetzer and blandford, sdac-tr-73-9
c                               (1973), for surface arrivals, decreased
c                               by 0.3 to handle p-p rather than 0-p.
c                             chang, internal memorandum (1977),
c                               pkp terms computed for depths from
c                               100 to 700 km, decreased by 0.3 to
c                               handle p-p rather than 0-p.
c                               note - def branch arrival assumed for
c                                      111-142 degrees and 154-180
c                                      degrees, bc path assumed from
c                                      143 to 153 degrees.
c
c        depth = surface
c
      data ibfac(  1,1),ibfac(  2,1),ibfac(  3,1)/00,23,25/
      data ibfac(  4,1),ibfac(  5,1),ibfac(  6,1)/28,31,33/
      data ibfac(  7,1),ibfac(  8,1),ibfac(  9,1)/35,37,39/
      data ibfac( 10,1),ibfac( 11,1),ibfac( 12,1)/40,39,38/
      data ibfac( 13,1),ibfac( 14,1),ibfac( 15,1)/37,33,30/
      data ibfac( 16,1),ibfac( 17,1),ibfac( 18,1)/26,26,26/
      data ibfac( 19,1),ibfac( 20,1),ibfac( 21,1)/27,27,28/
      data ibfac( 22,1),ibfac( 23,1),ibfac( 24,1)/29,30,30/
      data ibfac( 25,1),ibfac( 26,1),ibfac( 27,1)/32,31,32/
      data ibfac( 28,1),ibfac( 29,1),ibfac( 30,1)/33,33,33/
      data ibfac( 31,1),ibfac( 32,1),ibfac( 33,1)/34,34,34/
      data ibfac( 34,1),ibfac( 35,1),ibfac( 36,1)/34,34,33/
      data ibfac( 37,1),ibfac( 38,1),ibfac( 39,1)/32,32,31/
      data ibfac( 40,1),ibfac( 41,1),ibfac( 42,1)/31,32,32/
      data ibfac( 43,1),ibfac( 44,1),ibfac( 45,1)/32,32,34/
      data ibfac( 46,1),ibfac( 47,1),ibfac( 48,1)/35,36,36/
      data ibfac( 49,1),ibfac( 50,1),ibfac( 51,1)/35,34,34/
      data ibfac( 52,1),ibfac( 53,1),ibfac( 54,1)/34,34,35/
      data ibfac( 55,1),ibfac( 56,1),ibfac( 57,1)/35,35,35/
      data ibfac( 58,1),ibfac( 59,1),ibfac( 60,1)/35,35,35/
      data ibfac( 61,1),ibfac( 62,1),ibfac( 63,1)/36,37,36/
      data ibfac( 64,1),ibfac( 65,1),ibfac( 66,1)/37,37,37/
      data ibfac( 67,1),ibfac( 68,1),ibfac( 69,1)/37,37,37/
      data ibfac( 70,1),ibfac( 71,1),ibfac( 72,1)/36,36,36/
      data ibfac( 73,1),ibfac( 74,1),ibfac( 75,1)/36,35,35/
      data ibfac( 76,1),ibfac( 77,1),ibfac( 78,1)/36,36,36/
      data ibfac( 79,1),ibfac( 80,1),ibfac( 81,1)/35,34,35/
      data ibfac( 82,1),ibfac( 83,1),ibfac( 84,1)/36,37,37/
      data ibfac( 85,1),ibfac( 86,1),ibfac( 87,1)/37,36,37/
      data ibfac( 88,1),ibfac( 89,1),ibfac( 90,1)/38,37,37/
      data ibfac( 91,1),ibfac( 92,1),ibfac( 93,1)/38,38,39/
      data ibfac( 94,1),ibfac( 95,1),ibfac( 96,1)/38,39,40/
      data ibfac( 97,1),ibfac( 98,1),ibfac( 99,1)/41,42,42/
      data ibfac(100,1),ibfac(101,1),ibfac(102,1)/41,40,41/
      data ibfac(103,1),ibfac(104,1),ibfac(105,1)/42,43,44/
      data ibfac(106,1),ibfac(107,1),ibfac(108,1)/45,46,46/
      data ibfac(109,1),ibfac(110,1),ibfac(111,1)/47,48,45/
      data ibfac(112,1),ibfac(113,1),ibfac(114,1)/44,44,44/
      data ibfac(115,1),ibfac(116,1),ibfac(117,1)/44,43,43/
      data ibfac(118,1),ibfac(119,1),ibfac(120,1)/43,42,42/
      data ibfac(121,1),ibfac(122,1),ibfac(123,1)/42,41,41/
      data ibfac(124,1),ibfac(125,1),ibfac(126,1)/40,40,40/
      data ibfac(127,1),ibfac(128,1),ibfac(129,1)/40,39,39/
      data ibfac(130,1),ibfac(131,1),ibfac(132,1)/39,39,40/
      data ibfac(133,1),ibfac(134,1),ibfac(135,1)/40,41,41/
      data ibfac(136,1),ibfac(137,1),ibfac(138,1)/41,41,41/
      data ibfac(139,1),ibfac(140,1),ibfac(141,1)/41,41,41/
      data ibfac(142,1),ibfac(143,1),ibfac(144,1)/40,37,34/
      data ibfac(145,1),ibfac(146,1),ibfac(147,1)/32,32,32/
      data ibfac(148,1),ibfac(149,1),ibfac(150,1)/32,32,32/
      data ibfac(151,1),ibfac(152,1),ibfac(153,1)/33,37,37/
      data ibfac(154,1),ibfac(155,1),ibfac(156,1)/37,38,38/
      data ibfac(157,1),ibfac(158,1),ibfac(159,1)/38,38,39/
      data ibfac(160,1),ibfac(161,1),ibfac(162,1)/39,39,39/
      data ibfac(163,1),ibfac(164,1),ibfac(165,1)/39,39,39/
      data ibfac(166,1),ibfac(167,1),ibfac(168,1)/39,39,39/
      data ibfac(169,1),ibfac(170,1),ibfac(171,1)/39,39,39/
      data ibfac(172,1),ibfac(173,1),ibfac(174,1)/39,39,39/
      data ibfac(175,1),ibfac(176,1),ibfac(177,1)/39,39,38/
      data ibfac(178,1),ibfac(179,1),ibfac(180,1)/38,38,38/
c
c        depth = 100 km
c
      data ibfac(  1,2),ibfac(  2,2),ibfac(  3,2)/00,00,00/
      data ibfac(  4,2),ibfac(  5,2),ibfac(  6,2)/00,27,27/
      data ibfac(  7,2),ibfac(  8,2),ibfac(  9,2)/30,31,33/
      data ibfac( 10,2),ibfac( 11,2),ibfac( 12,2)/33,33,31/
      data ibfac( 13,2),ibfac( 14,2),ibfac( 15,2)/30,27,27/
      data ibfac( 16,2),ibfac( 17,2),ibfac( 18,2)/27,27,27/
      data ibfac( 19,2),ibfac( 20,2),ibfac( 21,2)/28,28,28/
      data ibfac( 22,2),ibfac( 23,2),ibfac( 24,2)/28,28,29/
      data ibfac( 25,2),ibfac( 26,2),ibfac( 27,2)/29,30,30/
      data ibfac( 28,2),ibfac( 29,2),ibfac( 30,2)/31,31,32/
      data ibfac( 31,2),ibfac( 32,2),ibfac( 33,2)/32,32,33/
      data ibfac( 34,2),ibfac( 35,2),ibfac( 36,2)/33,34,34/
      data ibfac( 37,2),ibfac( 38,2),ibfac( 39,2)/34,34,34/
      data ibfac( 40,2),ibfac( 41,2),ibfac( 42,2)/33,33,33/
      data ibfac( 43,2),ibfac( 44,2),ibfac( 45,2)/32,32,32/
      data ibfac( 46,2),ibfac( 47,2),ibfac( 48,2)/33,33,33/
      data ibfac( 49,2),ibfac( 50,2),ibfac( 51,2)/34,34,35/
      data ibfac( 52,2),ibfac( 53,2),ibfac( 54,2)/35,35,35/
      data ibfac( 55,2),ibfac( 56,2),ibfac( 57,2)/35,35,35/
      data ibfac( 58,2),ibfac( 59,2),ibfac( 60,2)/35,35,36/
      data ibfac( 61,2),ibfac( 62,2),ibfac( 63,2)/35,35,34/
      data ibfac( 64,2),ibfac( 65,2),ibfac( 66,2)/34,34,34/
      data ibfac( 67,2),ibfac( 68,2),ibfac( 69,2)/34,33,33/
      data ibfac( 70,2),ibfac( 71,2),ibfac( 72,2)/33,33,33/
      data ibfac( 73,2),ibfac( 74,2),ibfac( 75,2)/33,33,33/
      data ibfac( 76,2),ibfac( 77,2),ibfac( 78,2)/33,33,33/
      data ibfac( 79,2),ibfac( 80,2),ibfac( 81,2)/33,33,33/
      data ibfac( 82,2),ibfac( 83,2),ibfac( 84,2)/33,34,34/
      data ibfac( 85,2),ibfac( 86,2),ibfac( 87,2)/34,34,35/
      data ibfac( 88,2),ibfac( 89,2),ibfac( 90,2)/35,36,36/
      data ibfac( 91,2),ibfac( 92,2),ibfac( 93,2)/37,38,38/
      data ibfac( 94,2),ibfac( 95,2),ibfac( 96,2)/39,39,40/
      data ibfac( 97,2),ibfac( 98,2),ibfac( 99,2)/40,40,41/
      data ibfac(100,2),ibfac(101,2),ibfac(102,2)/41,41,42/
      data ibfac(103,2),ibfac(104,2),ibfac(105,2)/43,44,45/
      data ibfac(106,2),ibfac(107,2),ibfac(108,2)/45,46,46/
      data ibfac(109,2),ibfac(110,2),ibfac(111,2)/47,47,44/
      data ibfac(112,2),ibfac(113,2),ibfac(114,2)/43,43,43/
      data ibfac(115,2),ibfac(116,2),ibfac(117,2)/42,42,42/
      data ibfac(118,2),ibfac(119,2),ibfac(120,2)/42,41,41/
      data ibfac(121,2),ibfac(122,2),ibfac(123,2)/40,40,40/
      data ibfac(124,2),ibfac(125,2),ibfac(126,2)/39,39,39/
      data ibfac(127,2),ibfac(128,2),ibfac(129,2)/38,38,38/
      data ibfac(130,2),ibfac(131,2),ibfac(132,2)/38,38,39/
      data ibfac(133,2),ibfac(134,2),ibfac(135,2)/39,40,40/
      data ibfac(136,2),ibfac(137,2),ibfac(138,2)/40,40,40/
      data ibfac(139,2),ibfac(140,2),ibfac(141,2)/40,40,39/
      data ibfac(142,2),ibfac(143,2),ibfac(144,2)/38,36,32/
      data ibfac(145,2),ibfac(146,2),ibfac(147,2)/31,31,31/
      data ibfac(148,2),ibfac(149,2),ibfac(150,2)/31,31,31/
      data ibfac(151,2),ibfac(152,2),ibfac(153,2)/31,36,36/
      data ibfac(154,2),ibfac(155,2),ibfac(156,2)/36,36,37/
      data ibfac(157,2),ibfac(158,2),ibfac(159,2)/37,37,38/
      data ibfac(160,2),ibfac(161,2),ibfac(162,2)/38,38,38/
      data ibfac(163,2),ibfac(164,2),ibfac(165,2)/38,38,38/
      data ibfac(166,2),ibfac(167,2),ibfac(168,2)/38,38,38/
      data ibfac(169,2),ibfac(170,2),ibfac(171,2)/37,37,37/
      data ibfac(172,2),ibfac(173,2),ibfac(174,2)/37,37,37/
      data ibfac(175,2),ibfac(176,2),ibfac(177,2)/37,37,37/
      data ibfac(178,2),ibfac(179,2),ibfac(180,2)/37,37,37/
c
c        depth = 200 km
c
      data ibfac(  1,3),ibfac(  2,3),ibfac(  3,3)/00,00,00/
      data ibfac(  4,3),ibfac(  5,3),ibfac(  6,3)/00,26,27/
      data ibfac(  7,3),ibfac(  8,3),ibfac(  9,3)/27,27,28/
      data ibfac( 10,3),ibfac( 11,3),ibfac( 12,3)/28,29,29/
      data ibfac( 13,3),ibfac( 14,3),ibfac( 15,3)/29,29,29/
      data ibfac( 16,3),ibfac( 17,3),ibfac( 18,3)/29,29,29/
      data ibfac( 19,3),ibfac( 20,3),ibfac( 21,3)/30,30,30/
      data ibfac( 22,3),ibfac( 23,3),ibfac( 24,3)/30,31,31/
      data ibfac( 25,3),ibfac( 26,3),ibfac( 27,3)/31,32,32/
      data ibfac( 28,3),ibfac( 29,3),ibfac( 30,3)/32,32,32/
      data ibfac( 31,3),ibfac( 32,3),ibfac( 33,3)/32,31,31/
      data ibfac( 34,3),ibfac( 35,3),ibfac( 36,3)/31,31,31/
      data ibfac( 37,3),ibfac( 38,3),ibfac( 39,3)/31,30,30/
      data ibfac( 40,3),ibfac( 41,3),ibfac( 42,3)/30,30,30/
      data ibfac( 43,3),ibfac( 44,3),ibfac( 45,3)/30,29,29/
      data ibfac( 46,3),ibfac( 47,3),ibfac( 48,3)/29,29,29/
      data ibfac( 49,3),ibfac( 50,3),ibfac( 51,3)/29,29,30/
      data ibfac( 52,3),ibfac( 53,3),ibfac( 54,3)/30,31,31/
      data ibfac( 55,3),ibfac( 56,3),ibfac( 57,3)/31,32,32/
      data ibfac( 58,3),ibfac( 59,3),ibfac( 60,3)/32,32,32/
      data ibfac( 61,3),ibfac( 62,3),ibfac( 63,3)/32,32,32/
      data ibfac( 64,3),ibfac( 65,3),ibfac( 66,3)/32,32,32/
      data ibfac( 67,3),ibfac( 68,3),ibfac( 69,3)/32,32,32/
      data ibfac( 70,3),ibfac( 71,3),ibfac( 72,3)/32,32,32/
      data ibfac( 73,3),ibfac( 74,3),ibfac( 75,3)/32,32,32/
      data ibfac( 76,3),ibfac( 77,3),ibfac( 78,3)/32,32,32/
      data ibfac( 79,3),ibfac( 80,3),ibfac( 81,3)/32,32,32/
      data ibfac( 82,3),ibfac( 83,3),ibfac( 84,3)/32,32,32/
      data ibfac( 85,3),ibfac( 86,3),ibfac( 87,3)/32,33,33/
      data ibfac( 88,3),ibfac( 89,3),ibfac( 90,3)/33,34,34/
      data ibfac( 91,3),ibfac( 92,3),ibfac( 93,3)/35,35,36/
      data ibfac( 94,3),ibfac( 95,3),ibfac( 96,3)/36,37,38/
      data ibfac( 97,3),ibfac( 98,3),ibfac( 99,3)/38,39,39/
      data ibfac(100,3),ibfac(101,3),ibfac(102,3)/39,40,41/
      data ibfac(103,3),ibfac(104,3),ibfac(105,3)/42,44,45/
      data ibfac(106,3),ibfac(107,3),ibfac(108,3)/46,46,47/
      data ibfac(109,3),ibfac(110,3),ibfac(111,3)/47,47,43/
      data ibfac(112,3),ibfac(113,3),ibfac(114,3)/43,42,42/
      data ibfac(115,3),ibfac(116,3),ibfac(117,3)/42,41,41/
      data ibfac(118,3),ibfac(119,3),ibfac(120,3)/41,40,40/
      data ibfac(121,3),ibfac(122,3),ibfac(123,3)/40,39,39/
      data ibfac(124,3),ibfac(125,3),ibfac(126,3)/39,38,38/
      data ibfac(127,3),ibfac(128,3),ibfac(129,3)/38,37,37/
      data ibfac(130,3),ibfac(131,3),ibfac(132,3)/37,38,38/
      data ibfac(133,3),ibfac(134,3),ibfac(135,3)/39,39,39/
      data ibfac(136,3),ibfac(137,3),ibfac(138,3)/39,39,39/
      data ibfac(139,3),ibfac(140,3),ibfac(141,3)/39,39,39/
      data ibfac(142,3),ibfac(143,3),ibfac(144,3)/38,35,32/
      data ibfac(145,3),ibfac(146,3),ibfac(147,3)/30,30,30/
      data ibfac(148,3),ibfac(149,3),ibfac(150,3)/30,30,30/
      data ibfac(151,3),ibfac(152,3),ibfac(153,3)/31,35,35/
      data ibfac(154,3),ibfac(155,3),ibfac(156,3)/35,36,36/
      data ibfac(157,3),ibfac(158,3),ibfac(159,3)/36,37,37/
      data ibfac(160,3),ibfac(161,3),ibfac(162,3)/37,37,37/
      data ibfac(163,3),ibfac(164,3),ibfac(165,3)/37,37,37/
      data ibfac(166,3),ibfac(167,3),ibfac(168,3)/37,37,37/
      data ibfac(169,3),ibfac(170,3),ibfac(171,3)/37,37,37/
      data ibfac(172,3),ibfac(173,3),ibfac(174,3)/37,37,37/
      data ibfac(175,3),ibfac(176,3),ibfac(177,3)/37,37,37/
      data ibfac(178,3),ibfac(179,3),ibfac(180,3)/37,37,37/
c
c        depth = 300 km
c
      data ibfac(  1,4),ibfac(  2,4),ibfac(  3,4)/00,00,00/
      data ibfac(  4,4),ibfac(  5,4),ibfac(  6,4)/00,23,24/
      data ibfac(  7,4),ibfac(  8,4),ibfac(  9,4)/24,25,25/
      data ibfac( 10,4),ibfac( 11,4),ibfac( 12,4)/26,27,27/
      data ibfac( 13,4),ibfac( 14,4),ibfac( 15,4)/28,28,28/
      data ibfac( 16,4),ibfac( 17,4),ibfac( 18,4)/28,28,28/
      data ibfac( 19,4),ibfac( 20,4),ibfac( 21,4)/28,28,28/
      data ibfac( 22,4),ibfac( 23,4),ibfac( 24,4)/28,28,29/
      data ibfac( 25,4),ibfac( 26,4),ibfac( 27,4)/29,29,29/
      data ibfac( 28,4),ibfac( 29,4),ibfac( 30,4)/29,30,30/
      data ibfac( 31,4),ibfac( 32,4),ibfac( 33,4)/30,30,30/
      data ibfac( 34,4),ibfac( 35,4),ibfac( 36,4)/30,30,30/
      data ibfac( 37,4),ibfac( 38,4),ibfac( 39,4)/29,29,29/
      data ibfac( 40,4),ibfac( 41,4),ibfac( 42,4)/28,28,27/
      data ibfac( 43,4),ibfac( 44,4),ibfac( 45,4)/27,28,28/
      data ibfac( 46,4),ibfac( 47,4),ibfac( 48,4)/28,28,28/
      data ibfac( 49,4),ibfac( 50,4),ibfac( 51,4)/28,28,29/
      data ibfac( 52,4),ibfac( 53,4),ibfac( 54,4)/29,29,29/
      data ibfac( 55,4),ibfac( 56,4),ibfac( 57,4)/29,29,29/
      data ibfac( 58,4),ibfac( 59,4),ibfac( 60,4)/30,30,30/
      data ibfac( 61,4),ibfac( 62,4),ibfac( 63,4)/31,31,31/
      data ibfac( 64,4),ibfac( 65,4),ibfac( 66,4)/32,32,32/
      data ibfac( 67,4),ibfac( 68,4),ibfac( 69,4)/32,32,32/
      data ibfac( 70,4),ibfac( 71,4),ibfac( 72,4)/32,32,32/
      data ibfac( 73,4),ibfac( 74,4),ibfac( 75,4)/32,32,32/
      data ibfac( 76,4),ibfac( 77,4),ibfac( 78,4)/32,32,32/
      data ibfac( 79,4),ibfac( 80,4),ibfac( 81,4)/33,33,33/
      data ibfac( 82,4),ibfac( 83,4),ibfac( 84,4)/33,33,33/
      data ibfac( 85,4),ibfac( 86,4),ibfac( 87,4)/33,33,34/
      data ibfac( 88,4),ibfac( 89,4),ibfac( 90,4)/34,34,34/
      data ibfac( 91,4),ibfac( 92,4),ibfac( 93,4)/34,34,35/
      data ibfac( 94,4),ibfac( 95,4),ibfac( 96,4)/36,36,37/
      data ibfac( 97,4),ibfac( 98,4),ibfac( 99,4)/37,38,39/
      data ibfac(100,4),ibfac(101,4),ibfac(102,4)/39,40,40/
      data ibfac(103,4),ibfac(104,4),ibfac(105,4)/41,43,45/
      data ibfac(106,4),ibfac(107,4),ibfac(108,4)/46,47,47/
      data ibfac(109,4),ibfac(110,4),ibfac(111,4)/47,47,43/
      data ibfac(112,4),ibfac(113,4),ibfac(114,4)/42,42,41/
      data ibfac(115,4),ibfac(116,4),ibfac(117,4)/41,41,41/
      data ibfac(118,4),ibfac(119,4),ibfac(120,4)/40,40,39/
      data ibfac(121,4),ibfac(122,4),ibfac(123,4)/39,39,38/
      data ibfac(124,4),ibfac(125,4),ibfac(126,4)/38,38,37/
      data ibfac(127,4),ibfac(128,4),ibfac(129,4)/37,37,37/
      data ibfac(130,4),ibfac(131,4),ibfac(132,4)/37,37,37/
      data ibfac(133,4),ibfac(134,4),ibfac(135,4)/38,38,39/
      data ibfac(136,4),ibfac(137,4),ibfac(138,4)/39,39,38/
      data ibfac(139,4),ibfac(140,4),ibfac(141,4)/38,38,38/
      data ibfac(142,4),ibfac(143,4),ibfac(144,4)/37,35,31/
      data ibfac(145,4),ibfac(146,4),ibfac(147,4)/30,29,29/
      data ibfac(148,4),ibfac(149,4),ibfac(150,4)/29,30,30/
      data ibfac(151,4),ibfac(152,4),ibfac(153,4)/30,34,35/
      data ibfac(154,4),ibfac(155,4),ibfac(156,4)/35,35,35/
      data ibfac(157,4),ibfac(158,4),ibfac(159,4)/36,36,36/
      data ibfac(160,4),ibfac(161,4),ibfac(162,4)/36,36,36/
      data ibfac(163,4),ibfac(164,4),ibfac(165,4)/36,36,36/
      data ibfac(166,4),ibfac(167,4),ibfac(168,4)/36,36,36/
      data ibfac(169,4),ibfac(170,4),ibfac(171,4)/36,36,36/
      data ibfac(172,4),ibfac(173,4),ibfac(174,4)/36,36,36/
      data ibfac(175,4),ibfac(176,4),ibfac(177,4)/36,36,36/
      data ibfac(178,4),ibfac(179,4),ibfac(180,4)/36,36,36/
c
c        depth = 400 km
c
      data ibfac(  1,5),ibfac(  2,5),ibfac(  3,5)/00,00,00/
      data ibfac(  4,5),ibfac(  5,5),ibfac(  6,5)/00,24,25/
      data ibfac(  7,5),ibfac(  8,5),ibfac(  9,5)/26,27,28/
      data ibfac( 10,5),ibfac( 11,5),ibfac( 12,5)/29,29,29/
      data ibfac( 13,5),ibfac( 14,5),ibfac( 15,5)/29,29,29/
      data ibfac( 16,5),ibfac( 17,5),ibfac( 18,5)/29,29,29/
      data ibfac( 19,5),ibfac( 20,5),ibfac( 21,5)/29,29,29/
      data ibfac( 22,5),ibfac( 23,5),ibfac( 24,5)/29,29,29/
      data ibfac( 25,5),ibfac( 26,5),ibfac( 27,5)/29,29,29/
      data ibfac( 28,5),ibfac( 29,5),ibfac( 30,5)/29,28,28/
      data ibfac( 31,5),ibfac( 32,5),ibfac( 33,5)/28,28,28/
      data ibfac( 34,5),ibfac( 35,5),ibfac( 36,5)/28,28,28/
      data ibfac( 37,5),ibfac( 38,5),ibfac( 39,5)/28,28,28/
      data ibfac( 40,5),ibfac( 41,5),ibfac( 42,5)/28,28,28/
      data ibfac( 43,5),ibfac( 44,5),ibfac( 45,5)/28,28,28/
      data ibfac( 46,5),ibfac( 47,5),ibfac( 48,5)/28,28,28/
      data ibfac( 49,5),ibfac( 50,5),ibfac( 51,5)/28,28,28/
      data ibfac( 52,5),ibfac( 53,5),ibfac( 54,5)/28,28,28/
      data ibfac( 55,5),ibfac( 56,5),ibfac( 57,5)/28,28,29/
      data ibfac( 58,5),ibfac( 59,5),ibfac( 60,5)/29,29,29/
      data ibfac( 61,5),ibfac( 62,5),ibfac( 63,5)/30,30,31/
      data ibfac( 64,5),ibfac( 65,5),ibfac( 66,5)/31,31,32/
      data ibfac( 67,5),ibfac( 68,5),ibfac( 69,5)/32,32,31/
      data ibfac( 70,5),ibfac( 71,5),ibfac( 72,5)/31,31,31/
      data ibfac( 73,5),ibfac( 74,5),ibfac( 75,5)/31,31,32/
      data ibfac( 76,5),ibfac( 77,5),ibfac( 78,5)/32,32,32/
      data ibfac( 79,5),ibfac( 80,5),ibfac( 81,5)/32,32,32/
      data ibfac( 82,5),ibfac( 83,5),ibfac( 84,5)/32,32,32/
      data ibfac( 85,5),ibfac( 86,5),ibfac( 87,5)/33,33,33/
      data ibfac( 88,5),ibfac( 89,5),ibfac( 90,5)/33,33,33/
      data ibfac( 91,5),ibfac( 92,5),ibfac( 93,5)/34,34,35/
      data ibfac( 94,5),ibfac( 95,5),ibfac( 96,5)/35,36,36/
      data ibfac( 97,5),ibfac( 98,5),ibfac( 99,5)/37,38,39/
      data ibfac(100,5),ibfac(101,5),ibfac(102,5)/39,40,41/
      data ibfac(103,5),ibfac(104,5),ibfac(105,5)/42,43,44/
      data ibfac(106,5),ibfac(107,5),ibfac(108,5)/45,46,46/
      data ibfac(109,5),ibfac(110,5),ibfac(111,5)/47,47,42/
      data ibfac(112,5),ibfac(113,5),ibfac(114,5)/42,41,41/
      data ibfac(115,5),ibfac(116,5),ibfac(117,5)/41,40,40/
      data ibfac(118,5),ibfac(119,5),ibfac(120,5)/40,39,39/
      data ibfac(121,5),ibfac(122,5),ibfac(123,5)/39,38,38/
      data ibfac(124,5),ibfac(125,5),ibfac(126,5)/38,37,37/
      data ibfac(127,5),ibfac(128,5),ibfac(129,5)/37,36,36/
      data ibfac(130,5),ibfac(131,5),ibfac(132,5)/36,37,37/
      data ibfac(133,5),ibfac(134,5),ibfac(135,5)/38,38,38/
      data ibfac(136,5),ibfac(137,5),ibfac(138,5)/38,38,38/
      data ibfac(139,5),ibfac(140,5),ibfac(141,5)/38,38,38/
      data ibfac(142,5),ibfac(143,5),ibfac(144,5)/37,34,31/
      data ibfac(145,5),ibfac(146,5),ibfac(147,5)/29,29,29/
      data ibfac(148,5),ibfac(149,5),ibfac(150,5)/29,29,29/
      data ibfac(151,5),ibfac(152,5),ibfac(153,5)/30,34,34/
      data ibfac(154,5),ibfac(155,5),ibfac(156,5)/34,35,35/
      data ibfac(157,5),ibfac(158,5),ibfac(159,5)/35,35,36/
      data ibfac(160,5),ibfac(161,5),ibfac(162,5)/36,36,36/
      data ibfac(163,5),ibfac(164,5),ibfac(165,5)/36,36,36/
      data ibfac(166,5),ibfac(167,5),ibfac(168,5)/36,36,36/
      data ibfac(169,5),ibfac(170,5),ibfac(171,5)/36,36,36/
      data ibfac(172,5),ibfac(173,5),ibfac(174,5)/36,36,36/
      data ibfac(175,5),ibfac(176,5),ibfac(177,5)/36,36,35/
      data ibfac(178,5),ibfac(179,5),ibfac(180,5)/35,35,35/
c
c        depth = 500 km
c
      data ibfac(  1,6),ibfac(  2,6),ibfac(  3,6)/00,00,00/
      data ibfac(  4,6),ibfac(  5,6),ibfac(  6,6)/00,25,25/
      data ibfac(  7,6),ibfac(  8,6),ibfac(  9,6)/26,27,28/
      data ibfac( 10,6),ibfac( 11,6),ibfac( 12,6)/28,29,29/
      data ibfac( 13,6),ibfac( 14,6),ibfac( 15,6)/30,30,31/
      data ibfac( 16,6),ibfac( 17,6),ibfac( 18,6)/31,31,31/
      data ibfac( 19,6),ibfac( 20,6),ibfac( 21,6)/31,31,31/
      data ibfac( 22,6),ibfac( 23,6),ibfac( 24,6)/31,31,31/
      data ibfac( 25,6),ibfac( 26,6),ibfac( 27,6)/30,30,30/
      data ibfac( 28,6),ibfac( 29,6),ibfac( 30,6)/30,30,30/
      data ibfac( 31,6),ibfac( 32,6),ibfac( 33,6)/30,30,30/
      data ibfac( 34,6),ibfac( 35,6),ibfac( 36,6)/30,30,30/
      data ibfac( 37,6),ibfac( 38,6),ibfac( 39,6)/30,30,30/
      data ibfac( 40,6),ibfac( 41,6),ibfac( 42,6)/30,30,30/
      data ibfac( 43,6),ibfac( 44,6),ibfac( 45,6)/30,30,30/
      data ibfac( 46,6),ibfac( 47,6),ibfac( 48,6)/30,30,29/
      data ibfac( 49,6),ibfac( 50,6),ibfac( 51,6)/29,29,29/
      data ibfac( 52,6),ibfac( 53,6),ibfac( 54,6)/28,28,28/
      data ibfac( 55,6),ibfac( 56,6),ibfac( 57,6)/28,28,28/
      data ibfac( 58,6),ibfac( 59,6),ibfac( 60,6)/28,29,29/
      data ibfac( 61,6),ibfac( 62,6),ibfac( 63,6)/29,30,30/
      data ibfac( 64,6),ibfac( 65,6),ibfac( 66,6)/30,31,31/
      data ibfac( 67,6),ibfac( 68,6),ibfac( 69,6)/31,31,31/
      data ibfac( 70,6),ibfac( 71,6),ibfac( 72,6)/30,30,30/
      data ibfac( 73,6),ibfac( 74,6),ibfac( 75,6)/30,30,30/
      data ibfac( 76,6),ibfac( 77,6),ibfac( 78,6)/30,29,29/
      data ibfac( 79,6),ibfac( 80,6),ibfac( 81,6)/29,29,30/
      data ibfac( 82,6),ibfac( 83,6),ibfac( 84,6)/30,30,31/
      data ibfac( 85,6),ibfac( 86,6),ibfac( 87,6)/31,31,32/
      data ibfac( 88,6),ibfac( 89,6),ibfac( 90,6)/32,33,34/
      data ibfac( 91,6),ibfac( 92,6),ibfac( 93,6)/34,35,35/
      data ibfac( 94,6),ibfac( 95,6),ibfac( 96,6)/36,36,37/
      data ibfac( 97,6),ibfac( 98,6),ibfac( 99,6)/37,38,39/
      data ibfac(100,6),ibfac(101,6),ibfac(102,6)/39,40,41/
      data ibfac(103,6),ibfac(104,6),ibfac(105,6)/42,43,44/
      data ibfac(106,6),ibfac(107,6),ibfac(108,6)/44,44,45/
      data ibfac(109,6),ibfac(110,6),ibfac(111,6)/45,45,42/
      data ibfac(112,6),ibfac(113,6),ibfac(114,6)/41,41,40/
      data ibfac(115,6),ibfac(116,6),ibfac(117,6)/40,40,39/
      data ibfac(118,6),ibfac(119,6),ibfac(120,6)/39,39,38/
      data ibfac(121,6),ibfac(122,6),ibfac(123,6)/38,38,37/
      data ibfac(124,6),ibfac(125,6),ibfac(126,6)/37,37,36/
      data ibfac(127,6),ibfac(128,6),ibfac(129,6)/36,36,36/
      data ibfac(130,6),ibfac(131,6),ibfac(132,6)/36,36,36/
      data ibfac(133,6),ibfac(134,6),ibfac(135,6)/37,37,38/
      data ibfac(136,6),ibfac(137,6),ibfac(138,6)/38,37,37/
      data ibfac(139,6),ibfac(140,6),ibfac(141,6)/37,37,37/
      data ibfac(142,6),ibfac(143,6),ibfac(144,6)/36,34,30/
      data ibfac(145,6),ibfac(146,6),ibfac(147,6)/29,28,28/
      data ibfac(148,6),ibfac(149,6),ibfac(150,6)/28,28,29/
      data ibfac(151,6),ibfac(152,6),ibfac(153,6)/29,33,34/
      data ibfac(154,6),ibfac(155,6),ibfac(156,6)/34,34,34/
      data ibfac(157,6),ibfac(158,6),ibfac(159,6)/34,35,35/
      data ibfac(160,6),ibfac(161,6),ibfac(162,6)/35,35,35/
      data ibfac(163,6),ibfac(164,6),ibfac(165,6)/35,35,35/
      data ibfac(166,6),ibfac(167,6),ibfac(168,6)/35,35,35/
      data ibfac(169,6),ibfac(170,6),ibfac(171,6)/35,35,35/
      data ibfac(172,6),ibfac(173,6),ibfac(174,6)/35,35,35/
      data ibfac(175,6),ibfac(176,6),ibfac(177,6)/35,35,35/
      data ibfac(178,6),ibfac(179,6),ibfac(180,6)/35,35,35/
c
c        depth = 600 km
c
      data ibfac(  1,7),ibfac(  2,7),ibfac(  3,7)/00,00,00/
      data ibfac(  4,7),ibfac(  5,7),ibfac(  6,7)/00,24,25/
      data ibfac(  7,7),ibfac(  8,7),ibfac(  9,7)/25,25,25/
      data ibfac( 10,7),ibfac( 11,7),ibfac( 12,7)/26,26,27/
      data ibfac( 13,7),ibfac( 14,7),ibfac( 15,7)/27,28,28/
      data ibfac( 16,7),ibfac( 17,7),ibfac( 18,7)/29,29,29/
      data ibfac( 19,7),ibfac( 20,7),ibfac( 21,7)/30,30,31/
      data ibfac( 22,7),ibfac( 23,7),ibfac( 24,7)/31,31,31/
      data ibfac( 25,7),ibfac( 26,7),ibfac( 27,7)/31,31,31/
      data ibfac( 28,7),ibfac( 29,7),ibfac( 30,7)/31,31,31/
      data ibfac( 31,7),ibfac( 32,7),ibfac( 33,7)/31,31,31/
      data ibfac( 34,7),ibfac( 35,7),ibfac( 36,7)/31,31,31/
      data ibfac( 37,7),ibfac( 38,7),ibfac( 39,7)/31,30,30/
      data ibfac( 40,7),ibfac( 41,7),ibfac( 42,7)/30,30,30/
      data ibfac( 43,7),ibfac( 44,7),ibfac( 45,7)/30,30,30/
      data ibfac( 46,7),ibfac( 47,7),ibfac( 48,7)/30,30,30/
      data ibfac( 49,7),ibfac( 50,7),ibfac( 51,7)/30,30,29/
      data ibfac( 52,7),ibfac( 53,7),ibfac( 54,7)/29,29,28/
      data ibfac( 55,7),ibfac( 56,7),ibfac( 57,7)/28,28,27/
      data ibfac( 58,7),ibfac( 59,7),ibfac( 60,7)/27,27,27/
      data ibfac( 61,7),ibfac( 62,7),ibfac( 63,7)/28,28,28/
      data ibfac( 64,7),ibfac( 65,7),ibfac( 66,7)/29,29,29/
      data ibfac( 67,7),ibfac( 68,7),ibfac( 69,7)/29,30,30/
      data ibfac( 70,7),ibfac( 71,7),ibfac( 72,7)/30,30,30/
      data ibfac( 73,7),ibfac( 74,7),ibfac( 75,7)/30,30,30/
      data ibfac( 76,7),ibfac( 77,7),ibfac( 78,7)/30,29,29/
      data ibfac( 79,7),ibfac( 80,7),ibfac( 81,7)/29,29,29/
      data ibfac( 82,7),ibfac( 83,7),ibfac( 84,7)/30,30,31/
      data ibfac( 85,7),ibfac( 86,7),ibfac( 87,7)/31,32,32/
      data ibfac( 88,7),ibfac( 89,7),ibfac( 90,7)/33,33,34/
      data ibfac( 91,7),ibfac( 92,7),ibfac( 93,7)/35,35,36/
      data ibfac( 94,7),ibfac( 95,7),ibfac( 96,7)/36,37,37/
      data ibfac( 97,7),ibfac( 98,7),ibfac( 99,7)/38,38,39/
      data ibfac(100,7),ibfac(101,7),ibfac(102,7)/39,40,40/
      data ibfac(103,7),ibfac(104,7),ibfac(105,7)/41,42,42/
      data ibfac(106,7),ibfac(107,7),ibfac(108,7)/43,43,43/
      data ibfac(109,7),ibfac(110,7),ibfac(111,7)/44,44,41/
      data ibfac(112,7),ibfac(113,7),ibfac(114,7)/41,40,40/
      data ibfac(115,7),ibfac(116,7),ibfac(117,7)/40,39,39/
      data ibfac(118,7),ibfac(119,7),ibfac(120,7)/39,38,38/
      data ibfac(121,7),ibfac(122,7),ibfac(123,7)/38,37,37/
      data ibfac(124,7),ibfac(125,7),ibfac(126,7)/37,36,36/
      data ibfac(127,7),ibfac(128,7),ibfac(129,7)/36,35,35/
      data ibfac(130,7),ibfac(131,7),ibfac(132,7)/35,36,36/
      data ibfac(133,7),ibfac(134,7),ibfac(135,7)/37,37,37/
      data ibfac(136,7),ibfac(137,7),ibfac(138,7)/37,37,37/
      data ibfac(139,7),ibfac(140,7),ibfac(141,7)/37,37,37/
      data ibfac(142,7),ibfac(143,7),ibfac(144,7)/36,33,30/
      data ibfac(145,7),ibfac(146,7),ibfac(147,7)/28,28,28/
      data ibfac(148,7),ibfac(149,7),ibfac(150,7)/28,28,28/
      data ibfac(151,7),ibfac(152,7),ibfac(153,7)/29,33,33/
      data ibfac(154,7),ibfac(155,7),ibfac(156,7)/33,34,34/
      data ibfac(157,7),ibfac(158,7),ibfac(159,7)/34,34,35/
      data ibfac(160,7),ibfac(161,7),ibfac(162,7)/35,35,35/
      data ibfac(163,7),ibfac(164,7),ibfac(165,7)/35,35,35/
      data ibfac(166,7),ibfac(167,7),ibfac(168,7)/35,35,35/
      data ibfac(169,7),ibfac(170,7),ibfac(171,7)/35,35,35/
      data ibfac(172,7),ibfac(173,7),ibfac(174,7)/35,35,35/
      data ibfac(175,7),ibfac(176,7),ibfac(177,7)/35,35,35/
      data ibfac(178,7),ibfac(179,7),ibfac(180,7)/35,34,34/
c
c        depth = 700 km
c
      data ibfac(  1,8),ibfac(  2,8),ibfac(  3,8)/00,00,00/
      data ibfac(  4,8),ibfac(  5,8),ibfac(  6,8)/00,24,24/
      data ibfac(  7,8),ibfac(  8,8),ibfac(  9,8)/24,24,24/
      data ibfac( 10,8),ibfac( 11,8),ibfac( 12,8)/24,25,25/
      data ibfac( 13,8),ibfac( 14,8),ibfac( 15,8)/25,25,25/
      data ibfac( 16,8),ibfac( 17,8),ibfac( 18,8)/26,26,27/
      data ibfac( 19,8),ibfac( 20,8),ibfac( 21,8)/27,27,27/
      data ibfac( 22,8),ibfac( 23,8),ibfac( 24,8)/28,28,28/
      data ibfac( 25,8),ibfac( 26,8),ibfac( 27,8)/29,29,30/
      data ibfac( 28,8),ibfac( 29,8),ibfac( 30,8)/30,30,30/
      data ibfac( 31,8),ibfac( 32,8),ibfac( 33,8)/30,31,31/
      data ibfac( 34,8),ibfac( 35,8),ibfac( 36,8)/30,30,30/
      data ibfac( 37,8),ibfac( 38,8),ibfac( 39,8)/30,30,30/
      data ibfac( 40,8),ibfac( 41,8),ibfac( 42,8)/30,30,30/
      data ibfac( 43,8),ibfac( 44,8),ibfac( 45,8)/30,29,29/
      data ibfac( 46,8),ibfac( 47,8),ibfac( 48,8)/29,29,29/
      data ibfac( 49,8),ibfac( 50,8),ibfac( 51,8)/29,28,28/
      data ibfac( 52,8),ibfac( 53,8),ibfac( 54,8)/28,28,27/
      data ibfac( 55,8),ibfac( 56,8),ibfac( 57,8)/27,27,27/
      data ibfac( 58,8),ibfac( 59,8),ibfac( 60,8)/27,27,27/
      data ibfac( 61,8),ibfac( 62,8),ibfac( 63,8)/27,27,27/
      data ibfac( 64,8),ibfac( 65,8),ibfac( 66,8)/28,28,28/
      data ibfac( 67,8),ibfac( 68,8),ibfac( 69,8)/28,29,29/
      data ibfac( 70,8),ibfac( 71,8),ibfac( 72,8)/29,29,29/
      data ibfac( 73,8),ibfac( 74,8),ibfac( 75,8)/30,30,30/
      data ibfac( 76,8),ibfac( 77,8),ibfac( 78,8)/30,30,30/
      data ibfac( 79,8),ibfac( 80,8),ibfac( 81,8)/30,30,30/
      data ibfac( 82,8),ibfac( 83,8),ibfac( 84,8)/30,30,30/
      data ibfac( 85,8),ibfac( 86,8),ibfac( 87,8)/31,31,31/
      data ibfac( 88,8),ibfac( 89,8),ibfac( 90,8)/31,32,32/
      data ibfac( 91,8),ibfac( 92,8),ibfac( 93,8)/33,33,34/
      data ibfac( 94,8),ibfac( 95,8),ibfac( 96,8)/35,35,36/
      data ibfac( 97,8),ibfac( 98,8),ibfac( 99,8)/36,37,37/
      data ibfac(100,8),ibfac(101,8),ibfac(102,8)/38,38,39/
      data ibfac(103,8),ibfac(104,8),ibfac(105,8)/39,40,40/
      data ibfac(106,8),ibfac(107,8),ibfac(108,8)/41,41,41/
      data ibfac(109,8),ibfac(110,8),ibfac(111,8)/42,42,41/
      data ibfac(112,8),ibfac(113,8),ibfac(114,8)/40,40,39/
      data ibfac(115,8),ibfac(116,8),ibfac(117,8)/39,39,39/
      data ibfac(118,8),ibfac(119,8),ibfac(120,8)/38,38,38/
      data ibfac(121,8),ibfac(122,8),ibfac(123,8)/37,37,37/
      data ibfac(124,8),ibfac(125,8),ibfac(126,8)/36,36,36/
      data ibfac(127,8),ibfac(128,8),ibfac(129,8)/35,35,35/
      data ibfac(130,8),ibfac(131,8),ibfac(132,8)/35,35,36/
      data ibfac(133,8),ibfac(134,8),ibfac(135,8)/36,37,37/
      data ibfac(136,8),ibfac(137,8),ibfac(138,8)/37,37,37/
      data ibfac(139,8),ibfac(140,8),ibfac(141,8)/36,36,36/
      data ibfac(142,8),ibfac(143,8),ibfac(144,8)/35,33,29/
      data ibfac(145,8),ibfac(146,8),ibfac(147,8)/28,28,27/
      data ibfac(148,8),ibfac(149,8),ibfac(150,8)/28,28,28/
      data ibfac(151,8),ibfac(152,8),ibfac(153,8)/28,33,33/
      data ibfac(154,8),ibfac(155,8),ibfac(156,8)/33,33,33/
      data ibfac(157,8),ibfac(158,8),ibfac(159,8)/34,34,34/
      data ibfac(160,8),ibfac(161,8),ibfac(162,8)/34,35,35/
      data ibfac(163,8),ibfac(164,8),ibfac(165,8)/35,35,34/
      data ibfac(166,8),ibfac(167,8),ibfac(168,8)/34,34,34/
      data ibfac(169,8),ibfac(170,8),ibfac(171,8)/34,34,34/
      data ibfac(172,8),ibfac(173,8),ibfac(174,8)/34,34,34/
      data ibfac(175,8),ibfac(176,8),ibfac(177,8)/34,34,34/
      data ibfac(178,8),ibfac(179,8),ibfac(180,8)/34,34,34/
c
c
c        phase residuals in deciseconds
c
      data irespz/34*0/
c
c        residual array index to phase identification numbers
c
      data idxphz/34*0/
c
c        phase index array to instrument sampling rate and orientation
c           multiples of 2 - short period vertical
c           multiples of 3 - short period horizontal
c           multiples of 5 - long period vertical
c           multiples of 7 - long period horizontal
c
      data indp/  2, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
     1           10, 10, 10, 10, 10, 10, 10, 10,  1, 21, 21, 21,
     2           21, 21, 21, 21, 21, 21, 21, 21,  5,  7/
c
c        phase indices to coefficient arrays
c
      data nid/  20,  1,  2,  1,  1,  2,  1,  3,  4,  5,  3,  4,
     1            2,  5, 14, 15,  6,  7,  8,  9,  1, 10, 11, 12,
     2           13,  3,  6,  7,  8,  9, 10, 11, 33, 34/
c
c        paq search window for phases in integer deciseconds
c
      data schmin(01),schmax(01)/ -600, 1800/
      data schmin(02),schmax(02)/ 6600,10200/
      data schmin(03),schmax(03)/ 4200, 7800/
      data schmin(04),schmax(04)/-1200,13800/
      data schmin(05),schmax(05)/-1200,13800/
      data schmin(06),schmax(06)/ 1200,16200/
      data schmin(07),schmax(07)/-1200,13800/
      data schmin(08),schmax(08)/ 1200,21000/
      data schmin(09),schmax(09)/ 8400,20400/
      data schmin(10),schmax(10)/ 8400,20400/
      data schmin(11),schmax(11)/12000,15000/
      data schmin(12),schmax(12)/12000,15000/
      data schmin(13),schmax(13)/10200,13200/
      data schmin(14),schmax(14)/10800,13800/
      data schmin(15),schmax(15)/10800,12000/
      data schmin(16),schmax(16)/ 9600,12600/
      data schmin(17),schmax(17)/16200,18600/
      data schmin(18),schmax(18)/16200,18600/
      data schmin(19),schmax(19)/16200,19800/
      data schmin(20),schmax(20)/21600,25200/
      data schmin(21),schmax(21)/    0,    0/
      data schmin(22),schmax(22)/ 6600,10200/
      data schmin(23),schmax(23)/ 9000,13800/
      data schmin(24),schmax(24)/12000,16200/
      data schmin(25),schmax(25)/14400,16800/
      data schmin(26),schmax(26)/ -600,16200/
      data schmin(27),schmax(27)/ -600,16200/
      data schmin(28),schmax(28)/ -600,16200/
      data schmin(29),schmax(29)/ 9000,20400/
      data schmin(30),schmax(30)/10800,24600/
      data schmin(31),schmax(31)/ 2400,28800/
      data schmin(32),schmax(32)/ 2400,38400/
      data schmin(33),schmax(33)/ 300,100000/
      data schmin(34),schmax(34)/ 300,100000/
c
      end
