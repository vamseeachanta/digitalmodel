ID DIGITALMODEL, BEAM_ANALYSIS
SOL 101
CEND
TITLE = GENERIC OFFSHORE COMPONENT BEAM MODEL
ECHO = NONE
SUBCASE 1
   LABEL = STATIC LOAD CASE
   LOAD = 1
   SPC = 10
   DISPLACEMENT(PLOT) = ALL
   ELFORCE(PLOT) = ALL
   STRESS(PLOT) = ALL
BEGIN BULK
$
$ GENERIC BEAM TEMPLATE FOR DIGITALMODEL
$
$ GRID Points (X, Y, Z)
GRID    1               0.0     0.0     0.0
GRID    2               0.0     0.0     100.0
$
$ SPC (Constraints)
SPC1    10      123456  1
$
$ CBAR (Beam Elements)
$ EID, PID, G1, G2, X1, X2, X3
CBAR    1       100     1       2       1.0     1.0     0.0
$
$ PBAR (Property)
$ PID, MID, Area, I1, I2, J
PBAR    100     200     10.0    100.0   100.0   200.0
$
$ MAT1 (Material - Steel)
$ MID, E, G, NU, RHO
MAT1    200     3.0E7           0.3     0.283
$
$ FORCE (Loads)
$ SID, G, CID, F, N1, N2, N3
FORCE   1       2       0       1000.0  1.0     0.0     0.0
$
ENDDATA
