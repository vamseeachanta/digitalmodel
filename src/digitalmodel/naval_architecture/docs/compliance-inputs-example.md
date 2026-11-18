# Compliance input completeness and heave response

The compliance report exposes missing evidence as `NOT_EVALUATED` and keeps
`overall_pass` false. The heave oscillator raises at undamped resonance.
These examples exercise input handling and a linear oscillator, not a design
approval or a class-rule assessment.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-21 | Record missing-data behavior and oscillator answers for #2597. |
| 2 | 2026-10-21 | Align direct and aggregate validation, require curve coverage and identify the limited stability scope. |

## Input contract

`run_compliance_checks` returns every intended check. Each row carries `pass`
and `status`; missing-data rows carry a `reason`. A missing field, `None`,
nonfinite scalar, nonpositive principal dimension, empty curve, mismatched
curve lengths or nonincreasing angles prevents evaluation of the affected row.
Curve inputs are lists or tuples of finite numbers. Zero trim and zero
freeboard are valid supplied values. No GM, propeller diameter or actual
section modulus is inferred from absent data. Direct `check_*` calls apply the
same validation and return `status` on evaluated and unevaluated rows. Finite
real scalars, including NumPy real scalars, are supported; booleans are rejected.
The block coefficient requires `0 < cb <= 1`; displacement is positive and
the supplied section modulus is nonnegative. Caller-supplied ratio bounds are
finite, positive and ordered.

GZ angles start at zero and cover at least 40 degrees. A truncated interval
returns `NOT_EVALUATED` without criterion results. The interface does not take a
downflooding angle and cannot evaluate an alternative integration endpoint.

| Check | Required input keys |
| --- | --- |
| Intact stability, Part A 2.2 only | `angles_deg`, `gz_m`, `gm_m` |
| Freeboard | `freeboard_m`, `lwl_m` |
| Beam/draft | `beam_m`, `draft_m` |
| Length/beam | `lwl_m`, `beam_m` |
| Draft/depth | `draft_m`, `depth_m` |
| Displacement/length | `displacement_tonnes`, `lwl_m` |
| Hull girder section modulus | `lwl_m`, `beam_m`, `draft_m`, `cb`, `actual_z_cm3` |
| Propeller immersion | `draft_m`, `prop_diameter_m` |
| Trim | `trim_m`, `lwl_m` |

Aggregate status is `FAIL` if an evaluated row fails; otherwise it is
`NOT_EVALUATED` if any intended row lacks usable data, and `PASS` only if all
rows pass. `failed` counts evaluated failures; `not_evaluated` counts missing
or invalid rows. Their sum with `passed` equals `total`. Consumers that used
`not row['pass']` to count failures can now distinguish unknown evidence.
An empty input produces only unevaluated rows.

The stability row and aggregate name identify the Part A 2.2 scope. The Part A
2.3 severe wind and rolling criterion is not evaluated. A passing aggregate
therefore denotes these preliminary screens only; it does not establish full
intact-stability compliance.

## Worked example: partial compliance input

The dimensions below are synthetic arithmetic inputs. Draft/depth is a
geometrical ratio: 7.0 m / 10.0 m = 0.7. This one row can be computed; the
remaining input sets are incomplete, so the aggregate cannot pass.
The legacy draft/depth bound remains an unqualified screening assumption.

```pycon
>>> from digitalmodel.naval_architecture.compliance import run_compliance_checks
>>> report = run_compliance_checks({'draft_m': 7.0, 'depth_m': 10.0})
>>> row = next(c for c in report['checks'] if c['name'] == 'Draft/depth ratio')
>>> row['value']
0.7
>>> (report['status'], report['overall_pass'])
('NOT_EVALUATED', False)
>>> (report['total'], report['passed'], report['failed'], report['not_evaluated'])
(9, 1, 0, 8)
>>> run_compliance_checks({})['status']
'NOT_EVALUATED'

```

## Worked example: heave oscillator

Source: Mechanics Map, 2021 online edition, section 15.5, magnification-factor
equation and resonance discussion ([public text](https://eng.libretexts.org/@go/page/55503)).
For frequency ratio r = omega / omega_n and damping ratio zeta,
H = 1 / sqrt((1 - r²)² + (2 zeta r)²). This is the normalized single-degree
oscillator magnitude; it omits hull geometry, radiation and wave excitation.

Synthetic omega = omega_n = 1.0 rad/s gives r = 1.0. Declared zeta = 0.05
is an illustrative input, not a recommended damping coefficient. Hand
substitution gives H = 1 / (2 × 0.05) = 10.0. With zeta = 0.0 the denominator
vanishes and no finite steady-state magnitude exists. Near resonance the
finite magnitude is preserved without a cap. The independent hand cases and
floating-point tolerances are written in `test_heave_rao_resonance.py`.

```pycon
>>> from digitalmodel.naval_architecture.seakeeping import simple_heave_rao
>>> simple_heave_rao(1.0, 1.0, damping_ratio=0.05)
10.0
>>> simple_heave_rao(1.0, 1.0, damping_ratio=0.0)
Traceback (most recent call last):
    ...
ValueError: Heave RAO is unbounded at undamped resonance

```

## Limits and way forward

The pre-existing freeboard, proportion, displacement/length, section-modulus,
propeller and trim screens retain approximate coefficients and bounds whose
edition and clause have not been qualified here. No new engineering limit is
introduced. A `PASS` denotes the implemented preliminary screen only. Supply
missing measurements, adopt the applicable published rule basis and obtain
an approved stability and structural assessment before drawing a compliance
conclusion. The legacy strength coefficient, section-modulus unit conversion
and per-clause stability citations remain unqualified. GZ coverage is checked
over the implemented integration interval; downflooding applicability and the
weather criterion still require a separate assessment.
