## Introduction

For AQWA Advanced concepts. For introduction concepts, see [Intro concepts](docs\pkg_ansys\aqwa\_aqwa_intro.md)

### Summary

|  Program | Analysis Type |  Solver  |
|:--------:|:-------------:|:--------:|
| Aqwa-Line| Hydrodynamic Diffraction Analysis | Frequency Domain |
| Aqwa-Librium | Hydrodynamic Response Stability Analysis | Frequency Domain |
| Aqwa-Fer | Hydrodynamic Response Frequency Statistical Analysis | Frequency Domain |
| Aqwa-Drift | Hydrodynamic Response Time Domain Analysis | Time Domain |
| Aqwa-Naut | Hydrodynamic Response Time Domain Analysis | Time Domain |

- Aqwa-Line is the frequency domain radiation/diffraction solver used in a Hydrodynamic Diffraction Analysis
- Aqwa-Librium is the frequency domain solver used to calculate equilibrium positions and static/dynamic stability modes in a Hydrodynamic Response Stability Analysis
- Aqwa-Fer is the frequency domain solver used for linearized calculations in a Hydrodynamic Response Frequency Statistical Analysis
- Aqwa-Drift is the time domain solver performing transient calculations including slow drift effects in a Hydrodynamic Response Time Domain Analysis with Analysis Type of Irregular Wave Response with Slow Drift or Slow Drift Only
- Aqwa-Naut is the time domain solver performing transient calculations including nonlinear estimations of incident and hydrostatic pressures in a Hydrodynamic Response Time Domain Analysis with Analysis Type of Irregular Wave Response or Regular Wave Response

### References

<https://www.inas.ro/en/ansys-structures-aqwa>

 ANSYS AQWA Documentation link

<https://ansyshelp.ansys.com/account/secured?returnurl=/Views/Secured/prod_page.html?pn=Aqwa&pid=Aqwa&lang=en>

### Damping

Frequency analysis: Added damping

This damping may not carry into further restart analysis. This need to be reviewed and added to the restart analysis.
These are all DECK 7 - entiries.

FIAM - Frequency independent added mass
FIDA - Frequency independent added mass

- FIDP - Frequency independent damping
- FIDD - Frequency independent damping

For restart runs, FIDP and FIDD are typically imported from previous analysis when using WFRQ in DRIF analysis

FIDP,FIDD,  - Works only for diffraction analysis
DDEP (and MDIN) ? works only for static analysis.

**References**

[damping introduction](docs\sub_vibrations\intro.md)