# Wall selection basis and arithmetic

The pressure screening comparison TAB-WT-01 covers twelve cases and six selected
sections. The independent arithmetic below checks every returned candidate before
taking each strategy maximum. The frozen readback remains regression evidence.
Inputs without an independent public source are **assumed, based on previous
experience**. These calculations establish agreement with repository equations;
they do not qualify those equations against licensed standards.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-25 | Record input provenance, study conventions and independent equations for all code and section maxima; define comparison columns. |
| 2 | 2026-10-26 | Distinguish W1 hoop from W2 burst in the RP 2RD readback to match the detailed checks. |

## Versioned repository references

All references in this section identify repository revision
`3e6db58adf07aef1e7b9516833c4dd8f171030f1`. The `references` map in
[wall_basis.json](wall_basis.json) records revision, path and source-byte SHA-256
for each reference. They can be read with `git show <revision>:<path>`.
The regenerated manifest separately hashes the current source bytes, this
derivation and its executable arithmetic tests.

| Reference | Repository path and equation or input location |
| --- | --- |
| Matrix | `scripts/riser_study/wall_basis_matrix.csv`, all twelve rows |
| Previous basis | `scripts/riser_study/wall_basis.json`, study input values |
| Selection | `scripts/riser_study/wall_basis_table.py`, `_checked_wall`, `evaluate`, `select_walls` |
| Grid precedent | `scripts/reporting/build_riser_pipe_comparison.py`, `_minimum_wall` |
| Material | `src/digitalmodel/materials/grades.py`, X65 registry row |
| Defaults | `src/digitalmodel/structural/analysis/wall_thickness.py`, `PipeMaterial`, `DesignFactors` |
| RP 1111 | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_1111.py`, `EDITION_FACTORS[2015]` and all `_check_*` methods |
| RP 2RD | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py`, `C_A` and all `_check_*` methods |
| STD 2RD | `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py`, all `_check_*` methods |
| F101 | `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f101.py`, `EDITION_FACTORS[2021]`, all `_check_*` methods and `_solve_collapse_pressure` |

These are versioned implementation references, not independent standard examples.
The pinned matrix documents the adopted sizing projection; it is not evidence of
measured geometry or independently validated engineering inputs. No independent
external matrix source is available. The basis maps every matrix column to its
assumed input, study convention or derived regression evidence.

## Inputs and study conventions

Units below are mm and MPa unless stated otherwise. The adopted nominal sizes are
8, 10 and 12 inches with outside diameters D of 219.1, 273.1 and 323.9 mm.
Pressure p is 20.0 MPa gauge. Depth h is 1500 m, seawater density rho is
1025 kg/m³ and gravity g is 9.80665 m/s². The material registry provides
Y = 450.0 MPa and S = 535.0 MPa for the adopted X65 grade. E is
207000.0 MPa and Poisson ratio nu is 0.3. Ambient temperature is 20.0 °C.
These engineering inputs are assumed, based on previous experience; neither
dimensions nor material certificates have been qualified here.

Common checked wall t = (1 - 0.125) * nominal wall - 3.0 mm. This deduction
order, the 0.1 mm search grid, the additional 0.60 Pb guard, seamless fabrication,
zero effective tension and bending moment, and the no-arrestor condition are
study conventions, assumed, based on previous experience. W1 is the first grid
wall for which all returned checks pass. W2 is the first passing grid wall with
checked wall at least 1.25 times W1. These choices are not standard requirements.
SCR and SLWR labels share the same pressure screen, without dynamic analysis.

The two loads are (internal, external) = (p, 0) and (0, e), where
e = rho * g * h / 10^6 MPa. No external support is credited for burst. Using
propagation as a wall constraint follows the study's no-arrestor convention.
RP 1111 and F101 are pipeline cross-checks. Implemented factors below are read
from the pinned repository, not transcribed from a licensed publication.

## Equations and independent substitution

Let r = t/D, n = internal - external, and define:

- Pb = 0.45 * (Y + S) * ln(1/(1 - 2r)).
- Pel = 2 * E * r³ / (1 - nu²); Py = 2 * Y * r.
- Pc = 1 / sqrt(1/Pel² + 1/Py²).

The executable derivation is `_independent_checks` in
[test_wall_basis_table.py](../../tests/riser_study/test_wall_basis_table.py).
It hard-codes the documented inputs and factors, uses MPa rather than the
production Pa arithmetic, and imports no strategy, factor or basis loader.
Each of the six sections checks all 27 returned results, including signed empty
burst ratios, before comparing all five maxima. The original 30 frozen maxima
remain in a separate regression test.

For RP 2RD, substitute burst n/((2/3)*Pb), collapse external/((2/3)*Pc),
and hoop abs(n)*(1 - 2r)/(2*r*0.60*Y). These are the three candidate checks
for each load, using the RP 2RD implementation reference.

For RP 1111 (2015), use Pb above when D/t <= 15; otherwise use
Pb1111 = 0.90*(Y + S)*r/(1 - r). Its candidates are burst n/(0.72*Pb1111),
collapse external/(0.80*Pc), and propagation external/(0.80*24*Y*r^2.4).
The factors and branch are from the RP 1111 implementation reference.

For STD 2RD, candidates are burst n/(0.80*Pb), collapse external/(0.60*Pc),
and combined = 0.0 because both moment and effective tension are zero.
The combined pressure correction has no mechanical demand in this study.
These equations follow the STD 2RD implementation reference.

For F101 (2021), the adopted seamless alpha is 1.0, ovality f0 is 0.005,
and G = 1.138*1.15*1.0 uses the actual `DesignFactors` defaults. The
edition table alone does not set the active resistance factors. Define
Fcb = min(0.96*Y, 0.96*S/1.15) and Pbd = (2*r/(1-r))*(2/sqrt(3))*Fcb.
Solve the dimensionless collapse equation for x = Pcd/Py:

```text
(1 - x*Py/Pel)*(1 - x²) - x*f0/r = 0
0 < x < min(1, Pel/Py)
```

The reference bisects this bracket 100 times and checks the residual. Production
bisects the pressure-scale cubic to a 1 Pa bracket. The independent comparison
allows only the resulting numerical resolution. F101 candidates are containment
n*G/Pbd, collapse external*G/Pcd, propagation external*G/(35*Y*r^2.5), and
combined abs(n)/Pcd for zero mechanical loads. This retains the implemented
combined pressure term for both loads. References are the F101 strategy and
the defaults above. The separate study guard is p/(0.60*Pb).

## Section readback

Maxima below are percentages rounded to 0.1 %. The CSV retains unrounded ratios.
RP 1111 empty propagation governs all sections. RP 2RD maxima are hoop for W1
and burst for W2, both under internal pressure. STD 2RD maxima are empty collapse;
F101 maxima are empty propagation. All other candidates are independently checked
in the tests.

| Nominal size (in) | Wall | Nominal wall (mm) | RP 1111 (%) | RP 2RD (%) | STD 2RD (%) | F101 (%) | 0.60 Pb (%) |
| --- | --- | --- | --- | --- | --- | --- | --- |
| 8 | W1 | 21.2 | 99.8 | 44.8 | 42.3 | 93.4 | 49.1 |
| 8 | W2 | 25.7 | 58.1 | 34.6 | 32.4 | 53.1 | 38.4 |
| 10 | W1 | 25.6 | 99.6 | 44.7 | 42.2 | 93.2 | 49.1 |
| 10 | W2 | 31.2 | 58.0 | 34.5 | 32.3 | 53.0 | 38.4 |
| 12 | W1 | 29.7 | 99.8 | 44.8 | 42.3 | 93.4 | 49.1 |
| 12 | W2 | 36.3 | 58.3 | 34.6 | 32.4 | 53.3 | 38.5 |

## Table contract and limits

`tab-wt.csv` is TAB-WT-01: case identifier, nominal size, wall option, nominal
wall, checked wall, maxima in RP 1111 / RP 2RD / STD 2RD / F101 order,
the 0.60 Pb guard, and the governing check. Each maximum spans both loads and
every check returned by that strategy. `tab-wt-checks.csv` retains all 324
individual checks. The shared manifest binds both tables and their sources.

These results cover pressure screening only. Procurement, standard applicability,
dynamic resistance, measured ovality and fabrication qualification remain open.
The next step is independent engineering qualification of inputs and standard
applicability before using the selected walls for a design.
