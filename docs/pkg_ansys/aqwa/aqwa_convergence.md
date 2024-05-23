## Introduction

Lessons Learnt from AQWA Convergence Studies

### Summary

TBA

### Multi Structures (WLNG Project)

A ship structures is moored to shore using following:
    - 4 mooring struts
    - 2 universal joints at either end of strut
    - single dampener system with X and Y Stiffness properties
    - Dampener system is connected to mooring stuts and the shore

Convergence path

| Runname <br> (.wbpj or .dat)| Jackets | Dampener | B/S Joints | Struts | Description | Convergence Status | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- |
| D05315mm_pms_1pms_rigid_jts | (1) Jacket | High Stiffness <br> in all DOFs | Rigid | 5 Te Mass | Rigid Joints & Rigid Dampener | :heavy_check_mark: | - |
| D05315mm_pms_1pms_<br>z_damp_rigid_jts | (1) Jacket | High Stiffness <br> in Z & Rots | Rigid | 5 Te Mass| Rigid Joints | :heavy_check_mark: | - |
| D05315mm_pms_1pms_BS_jts | (1) Jacket | High Stiffness <br> in Z & Rots | B/S Joints <br> small stiffness & friction | 5 Te Mass | representative | &cross; | - |
| D05315mm_pms_1pms_<br>z_damp_rigid_jts | (1) Jacket | High Stiffness <br> in Z & Rots | Hinged | 5 Te Mass| Hinged Joints | &cross; | SOLUTION DIVERGED ON STRUCTURE 4 in Z-Heave at time=0.20 step #2 |
| D05315mm_pms_1pms_<br>z_damp_rigid_jts | (1) Jacket | DCAF Z, RX, RY, RZ | Hinged | 5 Te Mass| Hinged Joints | Stability - &cross;. <br> Time Domain - Needs Fender/Mooring |   |
| D05315mm_pms_1pms_<br>z_damp_BS_jts | (1) Jacket | DCAF Z, RX, RY, RZ | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain-&cross; |  Time Domain - Needs Fender/Mooring |
| D05315mm_1pms_fender | (1) Jacket | Fender/Mooring pair <br> DCAF Z, RX, RY, RZ | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: |  |
| D05315mm_pms_fender | (2) Jackets | Fender/Mooring pair <br> DCAF Z, RX, RY, RZ | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: |  |
| D05315mm_pms_ef| (2) Jackets | EF in python <br> DCAF Constraints | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: |  |
| D05315mm_pms.dat | (2) Jackets | EF in python <br> DCAF Constraints | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: | Prep D05315mm_pms_ef to prep files <br> .HYD, .EQP File <br> perform EF analysis |
| D05315mm_pms.dat | (2) Jackets | EF in python <br> DCAF Constraints | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: | Prep D05315mm_pms_ef to prep files <br> .HYD, .EQP File <br> perform EF analysis |
a/ check and copy dat file again.
b/ Rerun irregular wave analysis
c/ Save 2 dataframes for 2 dampener systems. use index.
d/ POS* run decay tests. Check, D05315mm_pms_fender_decay_test.wbpj for reference.

### Fender Model using External Force (Update)

A ship structures is moored to shore using following:
    - 1 fender (directly via AQWA element)
    - 1 fender modelled using external force

- Forces may be negative. COrrect the function and rerun
- Apply forces only when delta_L > 0 etc. Move forces application inside the if statement.
- Damper may need to be fixed instead of floating to simulate the forces properly - check relevancy.

Benchmarking path

| Runname | Pier | Mooring | Fender| Description | Convergence Status | Notes |
| --- | --- | --- | --- | --- | --- | --- |
| 02_s01_hd_pier | Generic | n/a | n/a  | With Pier | Diffraction - :heavy_check_mark: | - |
| 02_s02_hr_add_fenders | Generic | stiffness | Y-direction | Mooring/fender pair | Stability - :heavy_check_mark: | - |
| 02_s03_hr_wave | Generic | stiffness | Y-direction | Irregular Wave analysis | Time Domain - :heavy_check_mark: | - |
| 02_s04_hr_ext_force | Generic | stiffness | Y-direction | Fender with external force | Time Domain - :heavy_check_mark: | - |
