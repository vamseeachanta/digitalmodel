## Summary

- [x] Ensure FST1 and FST2 are consistent
  - STR1: FST2 
  - STR2: FST1
- [ ] Check Hull Shapes. Copy and correct if required?
- [ ] Runs on
  - [x] FSTs Only- ANSYS04
  - [ ] FSTs and LNGC:
    - [ ] 125km3 - PB and SB (ANSYS01)
    - [ ] 180km3 - PB (ANSYS04). SB (ANSYS04). Did not run on ANSYS03 due to C drive space.
    - [ ] Modify LNGC Center distances for both 125km3 and 180 km3
    - [ ] Located on analysis data drive, 1522\ctr7\aqwa\reva_08


### CoG Data (Deck 1)
   - Input FST1 CoG, 15%:
       <code>
           X:	109.050
           Y:	0.132
           Z:	16.965
       </code>
       Output:
       <code>
           198000            109.050     0.132    16.965
           [109.050, 0.132, 16.965]
       </code>
   - Input FST1 CoG, 95%:
       <code>
       X:	105.851
       Y:	0.058
       Z:	19.351
       </code>
       Output:
       <code>
           198000            105.851     0.058    19.351
           [105.851, 0.058, 19.351]
       </code>
   - Input FST2 CoG, 15%:
       <code>
       X:	107.574
       Y:	-0.131
       Z:	17.028
       </code>
       Output:
       <code>
           198000            107.574     -0.131    17.028
              [107.574, -0.131, 17.028]
       </code>
   - Input FST2 CoG, 95%:
       <code>
       X:	105.211
       Y:	-0.059
       Z:	19.363
       </code>
       Output:
       <code>
           198000            105.211     -0.059    19.363
                [105.211, -0.059, 19.363]
       </code>
### Mass data (Deck 3): Enter directly

### Inertia Data (Deck 4)
- Input FST1 Inertia, 15%:
    <code>
Ixx=	9.902E+06
Iyy=	1.386E+08
Izz=	1.392E+08
    </code>
    Output:
    <code>
98000 9.902e+06 0.000e+00 0.000e+00 1.386e+08 0.000e+00 1.392e+08
    </code>
- Input FST1 Inertia, 95%:
    <code>
Ixx=	2.024E+07
Iyy=	2.914E+08
Izz=	2.921E+08
    </code>
    Output:
    <code>
98000 2.024e+07 0.000e+00 0.000e+00 2.914e+08 0.000e+00 2.921e+08
    </code>
- Input FST2 Inertia, 15%:
    <code>
Ixx=	9.878E+06
Iyy=	1.416E+08
Izz=	1.420E+08
    </code>
    Output:
    <code>
98000 9.878e+06 0.000e+00 0.000e+00 1.416e+08 0.000e+00 1.420e+08
    </code>
- Input FST2 Inertia, 95%:
    <code>
Ixx=	2.020E+07
Iyy=	2.941E+08
Izz=	2.945E+08
    </code>
    Output:
    <code>
98000 2.020e+07 0.000e+00 0.000e+00 2.941e+08 0.000e+00 2.945e+08
    </code>
