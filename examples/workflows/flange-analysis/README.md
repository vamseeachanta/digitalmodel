# Analytical flange rating (ASME App 2)

This offline workflow screens integral, non-cast flanges for bolt area,
longitudinal hub, radial and tangential stresses, both combined stress
averages, and rigidity at operating and gasket-seating conditions. Units are
inches, lbf and psi. Geometry is net of corrosion; material allowables and
elastic moduli are supplied directly. Loose, lap-joint and optional types are
rejected. This screen does not establish full construction-code compliance,
material suitability, neck/weld qualification, fatigue life or leakage performance.
Engineer sign-off on the golden numbers remains required before use.

Run from the repository root:

```sh
python -m digitalmodel examples/workflows/flange-analysis/input.yml
python -m pytest 'tests/workflows/test_durable_workflows.py::test_workflow_registry[flange-analysis]' tests/structural/test_flange_analysis.py -q -m 'not solver'
```

The engine emits `results/input.yml`; the workflow writes a summary JSON,
a per-check CSV and a self-contained HTML report under `results/flange_analysis`.
Each check carries units, demand, allowable, utilization, pass/fail and its
validated clause. Generated outputs are not tracked.

## Published validation

Primary reference: Pressure Vessel Engineering, *Loads on Flanges - The ASME
Way*, last updated 9 September 2008, Flanges v4.26, pp. 12–14:
[original vendor publication](https://www.pveng.com/wp-content/uploads/2016/06/Loads_On_Flanges.pdf).
The downloaded source SHA-256 is recorded in `input.yml`. Its integral straight
hub is the uniform-thickness special case of the same integral-flange method.
The published temperature is unreported; `temperature_F: null` preserves that
limitation. The explicitly published allowables and moduli determine the check;
no temperature or material properties are inferred.

| Quantity | Published | Calculated | Difference (%) |
| --- | ---: | ---: | ---: |
| Wm1 (lbf) | 32941 | 32941.238 | 0.001 |
| Wm2 (lbf) | 164849 | 164765.389 | -0.051 |
| Am (in²) | 8.242 | 8.238269 | -0.045 |
| Ab (in²) | 9.056 | 9.056 | 0.000 |
| Operating moment (lbf·in) | 55998 | 55999.232 | 0.002 |
| Seating moment (lbf·in) | 269196 | 269131.037 | -0.024 |
| Operating SH (psi) | 5945 | 5944.600 | -0.007 |
| Operating SR (psi) | 1759 | 1758.662 | -0.019 |
| Operating ST (psi) | 1444 | 1444.341 | 0.024 |
| Seating SH (psi) | 28577 | 28569.612 | -0.026 |
| Seating SR (psi) | 8454 | 8452.092 | -0.023 |
| Seating ST (psi) | 6943 | 6941.471 | -0.022 |
| Operating J (1) | 0.094 | 0.094090 | 0.095 |
| Seating J (1) | 0.452 | 0.452192 | 0.043 |

Table 1. Reproduction of the published primary case, before engineer sign-off.

Bolt loads/areas are tested within 0.1%, moments within 0.2%, and stresses and
rigidity within 0.5%. The implementation uses the code's 0.785 and 3.14 load
coefficients; the vendor uses mathematical pi in some rows. This accounts for
the small seating-load difference. The governing check is seating SH:
28569.612 psi / 30000 psi = 0.952320; all 13 checks have utilization below 1.0.
The two averages use stress magnitudes, so opposite stress signs cannot cancel.

A second independent source exercises the tapered-hub closed forms:
Pressure Vessel Engineering, *Heat Exchanger Sample*, PVE-4293, PV Elite 2012,
18 September 2012, printed pp. 12–14:
[vendor calculation](https://www.pveng.com/wp-content/uploads/2016/06/HeatExchanger_Calcs.pdf).
Net dimensions B = 18.25 in, g0 = 0.125 in, g1 = 0.500 in and h = 1.125 in
reproduce F = 0.736, V = 0.081 and f = 2.9303 at printed precision. Diameter
ratio A/B = 23.5/18.25 reproduces T = 1.803, U = 8.595, Y = 7.822 and Z = 4.039.
This source is used only for factor validation, not as a second full rating.

## Method and review limits

The calculation references ASME VIII-1, 2019 Edition, Appendix 2. The vendored
clause index contains metadata and original paraphrases, not standard text.
Citation metadata and resolution are checked before results are returned.
The hub polynomial/cofactor equations implement Table 2-7.1; the three linear
systems use Cramer's rule, equivalent to the expanded equations (18)–(27).

The example is a non-lethal-service screen without a requested bolt-spacing
correction, consistent with the primary source. Setting
`design.apply_bolt_spacing_correction: true` applies Appendix 2-6 equation (7)
to stress moments. Rigidity always uses uncorrected moments per 2-6.
Lethal-service qualification and maximum-spacing requirements are outside this
screen; the flag does not establish that qualification. The standard integral
rigidity factor KI = 0.3 and the width, root-area and material-input bases are
visible in the report assumption ledger.

Plotly JavaScript is embedded with a stable chart identifier. The bundle
contains inert URL strings; the report has no external script or stylesheet
loads and the utilization chart requires no network connection.

The approved plan still requires independent engineering verification of the
factor equations and golden numbers before merge or live use. No such sign-off
is asserted by the automated comparisons.
