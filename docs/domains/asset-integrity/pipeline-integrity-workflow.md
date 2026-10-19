# Pipeline integrity workflow: pressure design stage

This first slice of the callable workflow in issue #388 evaluates nominal-wall
pressure design with an explicit material and factor basis. It returns required
wall thickness, pressure utilisation and a pressure-stage comparison. FFS and the
overall integrity status remain `NOT_EVALUATED`, including when pressure demand
does not exceed design pressure.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Add the pressure stage with required source references so callers can inspect its basis. |

## Source and equations

The implemented equation is from [49 CFR §192.105(a), Design formula for steel
pipe, 2025 annual edition](https://www.govinfo.gov/content/pkg/CFR-2025-title49-vol3/xml/CFR-2025-title49-vol3-sec192-105.xml).
The public source was opened successfully on 2026-10-19. This is a gas-pipeline
pressure-design equation, not an offshore pipeline design-code selection.

```text
P_design = (2 S t / D) F E T
t_required = P_demand D / (2 S F E T)
utilisation = P_demand / P_design
meets_pressure_design = P_demand <= P_design
```

The coefficient in the first equation is specified in §192.105(a). The required
thickness equation is its algebraic rearrangement; the utilisation ratio is the
definition of demand divided by capacity. No rounding or numerical tolerance is
applied to the pressure comparison.

`D` and `t` are nominal outside diameter and nominal wall thickness in mm.
`S` is yield strength in MPa, `F` is the design factor, `E` is the longitudinal
joint factor, and `T` is the temperature derating factor. Pressures are gauge
pressures in MPa. Consistent units preserve the source equation without a
conversion coefficient.

Strength and all factors are required inputs. The caller must provide nonempty
references identifying their source, edition and clause or table. The source
points to §§192.107, 192.111, 192.113 and 192.115 for their selection. This slice
does not implement those selection rules or validate the references.

## Single invocation

```python
from dataclasses import asdict
from digitalmodel.asset_integrity.pipeline_integrity_workflow import (
    PressureDesignInput,
    assess_pressure_design,
)

# Synthetic arithmetic example, not a material specification or factor selection.
case = PressureDesignInput(
    outer_diameter_mm=500.0,
    nominal_wall_thickness_mm=10.0,
    pressure_mpa=4.0,
    yield_strength_mpa=200.0,
    design_factor=0.5,
    joint_factor=1.0,
    temperature_factor=1.0,
    strength_reference="Synthetic strength for hand calculation",
    factors_reference="Synthetic factors for hand calculation",
    formula_applicable=True,
)
result = assess_pressure_design(case)
payload = asdict(result)  # JSON-compatible inputs, outputs, source and limitations
```

The tests write out the independent substitutions for this example and for
higher demand and reduced-factor configurations. No grade table or material
default is used. Invalid geometry, nonfinite numbers, negative demand, missing
references and nonpositive strength or factors raise `ValueError`. Calculated
overflow or underflow to zero capacity also raises `ValueError`.

`formula_applicable` is a required caller confirmation. It covers the scope of
§192.105(a), including wall thickness attributable to concurrent external loads,
and the heated, cold-expanded pipe limitation in §192.105(b). If applicability
is unknown or the formula does not apply, pass `False`: the pressure stage is
`NOT_EVALUATED`, with `None` for its quantities and comparison. Nominal wall
thickness must not be replaced by a corroded minimum reading to infer FFS.

## Scope and later slices

The new entry point is separate from the existing `pipeline_skill` API, whose
grade defaults and FFS verdicts are unchanged. This slice establishes an explicit
input and result contract for one pressure case; it does not chain the existing
parametric engine or FFS modules.

Later slices can add an adapter to the existing wall-thickness Cartesian sweep,
retaining every case and its design basis, followed by a sourced FFS adapter with
defect inputs and applicability checks. Until then, collapse, propagation
buckling, combined loading, corrosion and defect acceptance are unevaluated.
Pressure design alone does not establish an operating pressure limit, inspection
interval or integrity disposition.
