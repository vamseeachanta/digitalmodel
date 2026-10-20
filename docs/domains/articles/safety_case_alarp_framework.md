# Major accident hazard inventory and scenario QRA

The safety-case package records major accident hazards (MAHs), assesses supplied
mooring-break and passing-traffic scenarios, and exports an inventory to CSV.
Unknown quantities produce `NOT_EVALUATED`, with no numerical risk result.
These scenario results support further assessment; they establish neither hazard
coverage nor an ALARP demonstration.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-20 | Added the inventory, scenario methods and worked example to document inputs and outstanding model requirements. |

## Method and sources

[UK SCR-2015, regulation 16(1)(c)-(d)](https://www.legislation.gov.uk/uksi/2015/398/regulation/16)
requires identification of major accident hazards, assessment of likelihood and
consequences, and measures to control the risks. The CSV schema records a
scenario identifier and description, frequency and consequence with their bases,
barriers, and responsible roles. The scenario description includes the initiating
event and outcome.

[IMO MSC-MEPC.2/Circ.12/Rev.2, Revised guidelines for Formal Safety Assessment
(2018)](https://wwwcdn.imo.org/localresources/en/OurWork/Safety/Documents/MSC-MEPC%202-Circ%2012-Rev%202.pdf)
provides the public quantitative basis:

- Annex sections 5.2.1.2 and 5.3: record hazards and accident scenarios.
- Annex section 6.2.2: identify accident/failure data and the analysis basis.
- Appendix 3 section 2.2: multiply initiating frequency by conditional branch
  probabilities to obtain an outcome frequency.
- Appendix 5 section 3.2.3: potential loss of life (PLL) is the sum of each
  scenario frequency multiplied by its mean fatality consequence.

For one scenario, the implementation calculates:

```text
expected fatalities/year = events/year * mean fatalities/event
collision events/year = transits/year * probability(collision | counted transit)
```

Mean fatalities/event can exceed one. Event frequency can exceed one per year;
it is not an annual occurrence probability. The conditional collision probability
has mathematical bounds of zero and one (IMO annex section 2). All empirical
rates, conditional probabilities and consequences are required inputs. Barrier
names do not apply automatic risk reductions: the input bases must explain any
control credit already applied.

ISO 17776:2016, *Major accident hazard management during the design of new
installations*, remains a contextual reference for the inventory. Its public
catalogue and preview could not be read during implementation. Clause-level ISO
conformance is `NOT_EVALUATED`. NORSOK Z-013 and the existing HSE ALARP references
remain prose references in `alarp_demonstration`; no standards resolver pages or
licensed tables are introduced by this change. The public IMO and SCR clauses
above support the implemented methods without claiming ISO or NORSOK compliance.

## Interfaces and validation

`MajorAccidentHazard` and `MAHRegister` use immutable Pydantic models because CSV
and external inventory inputs require schema validation. The existing ALARP
calculation dataclasses remain in place. QRA result dataclasses contain a
validated hazard and retained calculation context.

| Field | Meaning |
| --- | --- |
| `hazard_id`, `description` | Nonblank unique scenario identifier and initiating event/outcome description |
| `frequency_per_year` | Nonnegative finite events/year, or explicit `None` |
| `fatalities_per_event` | Nonnegative finite conditional mean, or explicit `None` |
| `frequency_basis`, `consequence_basis` | Nonblank provenance, assumptions, missing evidence and control credit |
| `barriers` | Tuple of nonblank controls; empty means no controls recorded |
| `responsible_roles` | At least one nonblank responsible role |

CSV uses exactly these column names. The two tuple fields are JSON arrays within
CSV cells; standard CSV quoting handles embedded commas and quotation marks.
Blank numeric cells mean unknown. A known zero remains zero. Non-finite values,
negative quantities, extra/missing/duplicate columns, malformed arrays and
duplicate identifiers are rejected. `MAHValidationError.row` is the physical CSV
line at the end of the record; `.field` locates the invalid field. The entire
ingest fails rather than silently retaining only valid rows.

```python
import io
from digitalmodel.asset_integrity.safety_case import MAHRegister

# register is populated by one of the scenario assessments below.
csv_text = register.to_csv()
restored = MAHRegister.from_csv(io.StringIO(csv_text))
assert restored == register
# MAHRegister.from_csv also accepts a CSV file path.
```

Empty inventories are representable for exchange but demonstrate no risk
assessment. The register deliberately provides no automatic total: scenarios
may overlap or contain missing values. `EVALUATED` means the scenario arithmetic
has all inputs; it is not a verdict on risk acceptance.

## Worked example: inputs, method and answer

The executable example is
[`example_safety_case.py`](../../../examples/asset_integrity/example_safety_case.py).
Run it from the repository root with the repository dependencies installed:

```sh
PYTHONPATH=src python examples/asset_integrity/example_safety_case.py
```

All following inputs are **synthetic assumptions for arithmetic demonstration**.
No measured or historical rates are represented.

### Mooring line break

The example calls the real `orcaflex.mooring_design.solve_catenary` with depth
100 m, line length 500 m, submerged weight 1000 N/m and top pretension 300 kN.
The upstream module cites *Sea Loads on Ships and Offshore Structures* (1990),
chapter 6 for its catenary method.
Its identity `T = H + w*h` gives `H = 300 - 1*100 = 200 kN`; suspended length
satisfies `s*s = h*h + 2*(H/w)*h = 50000 square metres`, below the squared line
length, so seabed contact is possible. `assess_mooring_failure` retains the actual
top and horizontal tensions; it does not derive failure frequency from them.

An assumed line-break scenario frequency of 0.02 events/year and mean consequence
of 0.5 fatalities/event give `0.02 * 0.5 = 0.01` expected fatalities/year.
These are scenario inputs, not a per-line rate multiplied by an unstated number
of lines. Progressive failure and post-break load redistribution require separate
models and input evidence.

### Passing-traffic collision

The example supplies 1000 transits/year, a conditional collision probability
of 0.0002 per counted transit and 0.5 mean fatalities/collision. Conditional
multiplication gives `1000 * 0.0002 = 0.2` collisions/year and
`0.2 * 0.5 = 0.1` expected fatalities/year.

`VesselCollisionInputs` also requires traffic, probability and consequence bases.
`assess_vessel_collision` retains these inputs, creates a register entry and
optionally accepts `passing_ship_forces=calculator.calculate_forces(...)`.
The actual `hydrodynamics.passing_ship` output is surge/sway interaction force
(N) and yaw moment (N*m). These loads are retained as context. They provide
neither collision probability nor impact damage, exposure or fatality
consequences. This boundary also supplies no mooring response. No interaction
force is treated as an impact load or converted to fatalities.

The example's alternative traffic scenario explicitly supplies `None` for
probability and consequence, giving `NOT_EVALUATED`. The result's `missing_inputs`
identifies both external inputs even if interaction-force context is supplied.

| Scenario | Expected fatalities/year | Arithmetic status |
| --- | --- | --- |
| Line break | 1.0e-2 | EVALUATED |
| Passing collision | 1.0e-1 | EVALUATED |
| Alternative traffic without external models | unknown | NOT_EVALUATED |

The entries include alternatives and must not be added as independent hazards.
The worked-example test executes the script, checks these hand calculations and
round-trips the inventory through CSV. Separate integration tests consume the
real catenary result and the real passing-ship calculator output without a
licensed solver or mocks.

## ALARP boundary and remaining evidence

The existing `classify_risk` accepts individual annual fatality risk. The QRA
results here are expected fatalities across exposed persons per year. They must
not be passed directly to that classifier. A separately justified exposure and
individual-risk model is required before applying individual-risk thresholds.

| Outstanding assessment | Current treatment | Way forward |
| --- | --- | --- |
| Empirical mooring failure frequency | Required input or NOT_EVALUATED | Supply qualified failure data with applicability and control credit |
| Post-break consequences | Required input or NOT_EVALUATED | Supply response, escalation and exposure assessment |
| Collision causation and avoidance | Required conditional probability or NOT_EVALUATED | Supply traffic/geometry and conditional collision model |
| Collision damage and fatalities | Required conditional mean or NOT_EVALUATED | Supply impact, damage and exposure assessment |
| ISO clause conformance | NOT_EVALUATED | Review authorized clause text separately |
| Risk acceptance and ALARP conclusion | Not performed by these QRAs | Define risk measure, criteria, uncertainty and further controls |

Sensitivity analysis, uncertainty propagation, environmental/property-loss
consequences, fault/event-tree construction and regulatory-submission packaging
remain outside this implementation. The arithmetic is only as applicable as its
input evidence.
