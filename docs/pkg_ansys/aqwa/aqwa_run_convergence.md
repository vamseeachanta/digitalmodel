## Introduction

AQWA can sometimes be have convergence issues. Lessons learnt from AQWA Convergence Studies are given in this docuemnt.

### Summary

**Way Forward:**

- For Refresh WB to Command mode, follow below steps
- Create new folder:
  - check .dat file using Notepad++ file compare.
    - Make note of all properies to copy
      - Mass
      - Additional damping (frequency independent)
  - Copy dat file.
- Restart file for equilibrium analysis
  - Correct all the mass and damping properties for the FST structure (Done)
  - Delete additional stiffness as required
  - Reference HYD file for all structures (i.e. 2, 3 etc) onwards from appropriate file.
- Restart file for equilibrium analysis
- Saved 2 dataframes for 2 dampener systems.
- Plot the results for the dampener system(s) and compare.
- Run decay tests:
  - Use POS* to redefine structure positions (if required)
  - Check against all other work
- Postprocess Improvements:
  - improve labels for output columns coming out of Aqwa reader
    - eg: "Position of COG" to "CoG"
    - add other structures for CoG
      - Users can use the data in .dat file to find out the number of structures and articulations and use it in Aqwareader. cards STRC, FLIN, NLIN etc.
    - for _EF run, add a flowchart
    - distinguish postprocessing between summary (ending value) and time/iteration progression

**Long-term corrections**

- Model improvements
  - Flare out the springs to have more stability (Done)
  - Correct elevation, geometry, mass for all minor strucures (Struts, dampener, bearings etc.)
  - Add nodes for:
    - Tank accelerations (MOSS)
    - Jumper arm motions (MDR)
- Time step sensivity (basecase: 0.1s, sensitivities: 0.05s, 0.025s)
- Others?

- Simplify reruns with stage restart (ongoing)

Filelog:

- WFRQ_02PMS_ad010_fst2_l015 : Limited frequency run (Done)
  - Filename: F_F2L15_d00_SIMP
  - Filename: F_F2L15_d10_SIMP
- WFRQ_ad010_fst2_l015 : Comprehensive frequency run (Done)
  - restart: F_F2L15_d10_COMP
  - Simplify reruns with stage restart (DONE)
- STAT_ad010_fst2_l015 : Equilibrium run (postprocess?)
  - Postprocess (code enhancement?)
  - restart: S_F2L15_d10_W00V00C00 (Done)
- DYNA_NOL_MOOR_FST2_L015 : Time domain run No Load, with moorings.
  - Postprocess (code enhancement?)
  - Simplify run with stage restart: D_F2L15_d10_W00V00C00
- DYNA_NOL_fst2_l015 : Time domain run No Load, No moorings.
  - Postprocess (code enhancement?)
  - Simplify run with stage restart?

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
| D05315mm_pms_fender | (2) Jackets | Fender/Mooring pair <br> DCAF Z, RX, RY, RZ | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: | For **analysis** with AQWA restart |
| D05315mm_pms_ef| (2) Jackets | EF in python <br> DCAF Constraints | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: |  |
| D05315mm_pms.dat | (2) Jackets | EF in python <br> DCAF Constraints | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: | Prep D05315mm_pms_ef to prep files <br> .HYD, .EQP File <br> perform EF analysis |
| D05315mm_pms.dat | (2) Jackets | EF in python <br> DCAF Constraints | B/S | 5 Te Mass| Representative | Stability - :heavy_check_mark:. <br> Time Domain - :heavy_check_mark: | Prep D05315mm_pms_ef to prep files <br> .HYD, .EQP File <br> perform EF analysis |

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
