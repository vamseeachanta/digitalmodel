# Pipeline collapse analysis

The collapse routine solves the physical root of the existing interaction
equation. Both external-collapse and combined-loading results now expose
`standard_compliance: NOT_EVALUATED` and list the unqualified parts of the design
basis. Combined loading also accepts explicit elastic properties. A passing
`pass` flag denotes the legacy numerical screen only; it does not qualify a design.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-21 | Document explicit collapse inputs, assessment limits and an independent arithmetic example to distinguish numerical results from design qualification. |

## Method and sources

The public [EPRG paper, Deeper, Longer and Thicker: Challenges for the Design of
Subsea Pipelines (2022)](https://www.eprg.net/fileadmin/EPRG_Dokumente/00222_Position_Paper_-_Final__EPRG__49__2_29_.pdf),
section 3.1, equation (3), gives the interaction model:

```text
(pc - pel) (pc² - pp²) = pc pel pp f0 D/t
```

The component definitions retained from `dnv_collapse_pressure` are:

```text
pel = 2 E (t/D)³ / (1 - nu²)
pp  = 2 SMYS t/D
```

The existing implementation cites TU Delft, *Local Buckling and Collapse of Thick
Walled pipe* (2016), equations (3.9) and (3.10), for these definitions. The
[institutional copy](https://repository.tudelft.nl/file/File_09e16762-ff32-49eb-bff8-85cffa889063)
could not be opened during this change (request timed out). The component model
is retained, rather than independently requalified. No fabrication or strength
reduction coefficient is selected from an unavailable source.

For positive component pressures and positive ovality, the physical root lies
between zero and the smaller component pressure. Dividing the factored equation
by `pc` shows that its left side decreases strictly on this interval, so the root
is unique there. Bisection uses this bracket. At zero ovality the first positive
root is `min(pel, pp)`, including coincident components. The earlier numerical
correction and its independent root regressions are already in the codebase.

## Input basis

Pressures and elastic modulus use MPa; diameter and thickness use metres.
The routine uses outside diameter and the thickness supplied by the caller.
It does not select a design thickness from corrosion or fabrication allowances.
Ovality is `(Dmax - Dmin)/D`, expressed as a fraction.

The legacy defaults for ovality, elastic properties and water properties are
**assumed, based on previous experience**. They are not qualified design inputs.
Supply measured or justified values explicitly. A `PipeMaterial` strength alone
does not establish temperature derating or fabrication qualification.

`dnv_external_collapse_check` already accepts `E` and `nu`.
`dnv_combined_loading_check` now accepts the same keyword-only arguments, passing
them to the collapse solver. Existing calls retain their numerical defaults.
Both result dictionaries retain existing keys and add:

| Key | Meaning |
| --- | --- |
| `standard_compliance` | Always `NOT_EVALUATED`; a numerical pass cannot establish complete standard compliance. |
| `not_evaluated` | Lists `fabrication_factor`, `strength_derating`, `ovality_and_thickness_basis`, and `design_criterion_and_factors`. |

These statuses apply to passing and failing screens, including explicit inputs.
They also travel with the external-collapse dictionary returned by the sizing
workflow. No downstream design-factor qualification is implied.

## Worked example

The inputs below are synthetic, chosen to permit an exact hand calculation.
They are not properties of a standard material grade or an engineering acceptance
case. The tensile strength is unused by the collapse model.

| Input | Value | Basis |
| --- | --- | --- |
| Outside diameter | 1.0 m | Synthetic geometry |
| Thickness | 0.05 m | Synthetic geometry |
| Yield strength | 350.0 MPa | Synthetic material |
| Tensile strength | 400.0 MPa | Synthetic material; unused here |
| Elastic modulus | 144000.0 MPa | Chosen for exact component arithmetic |
| Poisson ratio | 0.0 | Chosen for exact component arithmetic |
| Ovality | 0.005 | Explicit synthetic input |
| Internal pressure | 0.0 MPa | Synthetic demand |
| External pressure | 14.0 MPa | Synthetic demand |
| Moment and effective tension | 0.0 | Synthetic demand |

Using the equations above, `pel = 36.0 MPa` and `pp = 35.0 MPa`.
At `pc = 28.0 MPa`, the interaction residual is exactly zero:

```text
(28 - 36) (28² - 35²) = 3528
28 × 36 × 35 × 0.005 / 0.05 = 3528
```

The root lies below both component pressures. With no moment or effective
tension, the legacy combined-loading screen reduces to the pressure ratio:
`U = abs((0 - 14)/28) = 0.5`. This arithmetic passes the legacy unity comparison;
standard compliance remains `NOT_EVALUATED`.

Run the executable example with the repository source available:

```bash
PYTHONPATH=src python examples/pipeline_collapse.py
```

Expected principal outputs are `28.0 MPa`, utilization `0.5`, and
`NOT_EVALUATED`. The example also prints each outstanding qualification item.
`test_dnv_collapse_basis.py` executes the same example and compares it against
the independent hand calculation written in the test docstring.

The earlier issue regression uses a second synthetic geometry and brackets its
root by independent decimal substitution in `test_dnv_collapse_root.py`.
That case returns **37.4 MPa**; the former result was **161.5 MPa**, an
overstatement by a factor of **4.3**. These displayed values are rounded to one
decimal place; the regression tests use the unrounded calculation bounds.

## Way forward and limits

Before design use, establish the intended standard edition, geometry and
minimum-ovality basis, temperature-adjusted strength, fabrication coefficient,
applicability range, and load/resistance factors from qualified inputs and
accessible authoritative sources. Check external pressure and combined loading
against the applicable criteria. Until then, complete compliance with
DNV-ST-F101, *Submarine pipeline systems* (2021), **System collapse** and
**Local buckling**, remains `NOT_EVALUATED`. No licensed clause text or table
values are reproduced here.
