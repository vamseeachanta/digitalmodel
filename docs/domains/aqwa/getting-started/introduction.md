## Introduction

### Integrated System

Ansys AQWA software is an integrated system for undertaking hydrodynamic and mooring analyses. Functionally is split into six operations:

Diffraction/radiation
Static and dynamic initial stability including the effects of mooring systems and other physical connections
Frequency domain dynamic analysis
Time domain with irregular waves including slow drift
Nonlinear time domain with large-amplitude regular or irregular waves
Hydrodynamic load transfer to a structural finite element analysis
ANSYS Aqwa - Integrated System
An optional facility for coupled cable dynamics is available with the static and dynamic solvers (in both frequency and time domain) to provide more rigorous modeling of mooring system loading and response, especially in deep waters.

![Overview](image.png)

![alt text](image-1.png)

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
