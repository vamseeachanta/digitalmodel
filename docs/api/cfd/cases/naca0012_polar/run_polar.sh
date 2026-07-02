#!/bin/bash
# NACA0012 polar: sweep AoA by rotating the freestream (|U|=26, Re~2.6e6).
source /usr/lib/openfoam/openfoam2312/etc/bashrc 2>/dev/null
CASE=/tmp/claude-1000/-mnt-local-analysis/86727272-c4c1-41a9-aab1-739eec70e02d/scratchpad/naca
cd "$CASE"
UMAG=26.003
PI=3.14159265358979
echo "alpha,Cl,Cd" > polar.csv

for A in 0 1 2 3 4 5 6 7 8; do
  COS=$(awk "BEGIN{printf \"%.8f\", cos($A*$PI/180)}")
  SIN=$(awk "BEGIN{printf \"%.8f\", sin($A*$PI/180)}")
  NSIN=$(awk "BEGIN{printf \"%.8f\", -sin($A*$PI/180)}")
  UX=$(awk "BEGIN{printf \"%.6f\", $UMAG*$COS}")
  UY=$(awk "BEGIN{printf \"%.6f\", $UMAG*$SIN}")

  # 0/U with rotated freestream
  cat > 0/U <<EOF
FoamFile { version 2.0; format ascii; class volVectorField; object U; }
dimensions      [0 1 -1 0 0 0 0];
internalField   uniform ($UX $UY 0);
boundaryField
{
    inlet   { type freestreamVelocity; freestreamValue \$internalField; }
    outlet  { type freestreamVelocity; freestreamValue \$internalField; }
    walls   { type noSlip; }
    frontAndBack { type empty; }
}
EOF

  # forceCoeffs in wind axes (drag along freestream, lift normal)
  cat > system/forceCoeffs <<EOF
forceCoeffs
{
    type            forceCoeffs;
    libs            ("libforces.so");
    writeControl    writeTime;
    patches         (walls);
    rho             rhoInf;
    rhoInf          1;
    liftDir         ($NSIN $COS 0);
    dragDir         ($COS $SIN 0);
    CofR            (0.25 0 0);
    pitchAxis       (0 0 1);
    magUInf         $UMAG;
    lRef            1;
    Aref            1.7525;
}
EOF

  # fresh start from time 0
  rm -rf [1-9]* processor* postProcessing
  simpleFoam > log.simpleFoam.$A 2>&1 || { echo "alpha $A FAILED"; continue; }

  CF=$(find postProcessing/forceCoeffs -name 'coefficient*.dat' 2>/dev/null | head -1)
  L=$(grep -v '^#' "$CF" | tail -1)
  CD=$(echo "$L" | awk '{print $2}')
  CL=$(echo "$L" | awk '{print $5}')
  echo "$A,$CL,$CD" >> polar.csv
  echo "alpha=$A  Cl=$CL  Cd=$CD"
done
echo "=== polar.csv ==="
cat polar.csv
