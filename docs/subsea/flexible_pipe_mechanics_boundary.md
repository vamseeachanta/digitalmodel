# Flexible pipe mechanics boundary

## Summary

This `research_only` specification defines descriptive metadata, unresolved
qualification inputs and later mechanics work for unbonded flexible pipes,
dynamic risers and dynamic jumpers. No capacity, design verdict or export-ready
section is established. The machine-readable companion is
[flexible_pipe_mechanics_boundary.yaml](flexible_pipe_mechanics_boundary.yaml).

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-11-16 | Define the metadata/mechanics boundary and follow-up tests before extending the existing section core. |

Table 1. Specification revision history.

## Boundary

The generic [cross-section schema](../../src/digitalmodel/subsea/cross_sections/schema.py)
describes cable, umbilical and pipeline geometry; it does not qualify flexible-pipe
mechanics. The existing [flexible-pipe core](../../src/digitalmodel/subsea/flexible_pipes/cross_section.py)
already describes layers and derives geometric, shell-mass and indicative smeared
EA/EI properties. It will remain the property core rather than be duplicated.

Layer role, material identity, duty and geometry are metadata. A liner is represented
by `pressure_sheath`; carcass, pressure armour, tensile armour and outer sheath map
to existing roles. Optional anti-wear maps to `anti_wear`; optional insulation has
no enum equivalent. This taxonomy does not require every role or a universal order.
The YAML specimen describes one synthetic layer, not a complete construction.

Operating and storage minimum bend radius (MBR), pressure/collapse ratings and
their pressure, temperature, tension, annulus and duty conditions require supplied
qualification evidence. Missing values remain null with a reason and an evidence
requirement; zero cannot stand for missing evidence. Friction, slip/contact,
hysteresis, pressure-dependent stiffness, torsion, annulus condition and fatigue
require separate mechanics or qualification work. No default is inferred here.

## Evidence

Source identities are inherited from the unchanged
`docs/research/ffs-unbonded-flexible-pipe-integrity-screen.sources.yaml`
([source records](../research/ffs-unbonded-flexible-pipe-integrity-screen.sources.yaml)).
S1 is the regulator-hosted construction report, revision 5, dated 31 December 2013:
section 2.2, printed pages 12–14 supports the construction roles; section 4.3.3,
printed page 37 supports optional anti-wear and insulation context. These passages
support taxonomy only, without adopting their numeric properties or reproducing figures.

S3 is the regulator inspection page, issued 19 February 2008: “Good practice
guidance” and “Recommendations” support inspection and monitoring context.
Its API designations are historical; they do not establish current editions.
Both public sources were retrieved successfully on 16 November 2026; source-byte
digests and locators are recorded in the YAML. The historical source sidecar's
retrieval date is preserved. API 17J, API 17B and ISO 13628-2 governing editions,
clauses and factor provenance remain unresolved. No licensed text or criteria are added.

## Export requirements

The [riser schema](../../src/digitalmodel/solvers/orcaflex/modular_generator/schema/riser.py)
and [line-type builder](../../src/digitalmodel/solvers/orcaflex/modular_generator/builders/riser_linetype_builder.py)
define the destination contract. A future adapter will apply these conversions:

| Current section output | Destination field | Forward conversion | Reverse conversion |
| --- | --- | --- | --- |
| `outer_diameter`, `bore_diameter` (m) | OD, ID (m) | Identity | Identity |
| `dry_mass_per_length()` (kg/m) | MassPerUnitLength (te/m) | Divide by 1000 | Multiply by 1000 |
| `axial_stiffness()` (N) | EA (kN) | Divide by 1000 | Multiply by 1000 |
| `bending_stiffness()` (N·m²) | EI (kN·m²) | Divide by 1000 | Multiply by 1000 |

Table 2. Unit requirements for a future adapter; these are not qualified properties.

GJ is absent from the section API, rather than zero. Qualified bend limits,
coupling and nonlinear stiffness are also not established. The destination's
torsional default and builder's zero tension-torque coupling are not supplied
section data. The builder treats a false-valued bend radius as absent; an adapter
will distinguish missing versus zero before construction. No model is exported here.

## Follow-up plan

1. A metadata validator will distinguish missing versus zero MBR, operating versus storage
   conditions and incompatible pressure-rating conditions. Fixtures will include null,
   zero, negative/non-finite limits, mismatched duty/temperature/annulus state and
   a qualified positive limit. Tests will reject incomplete inputs for engineering use.
2. A source-qualified property adapter will test the forward and reverse conversions
   above, absent GJ, absent coupling and nonlinear stiffness, and missing bend limits.
   Tests will reject destination defaults as evidence and retain source/condition provenance.
3. Separately sourced mechanics cases will address pressure/collapse, layer interaction,
   torsion and fatigue. Each formula will require redistributable public page/table/equation
   evidence, governing applicability and an independent reference before implementation.
   Missing formula evidence will stop that work as `BASIS-UNRESOLVED`.
   Native solver readback will require a separately authorized licensed execution task.

## Limitations

Offline tests check specification consistency and completeness, not physical performance,
current-standard compliance or native solver compatibility. The incomplete specimen cannot
support a calculation. The two issue-linked wiki pages were unavailable at the inspected
revision; no content is reconstructed from them. Existing formula, omission and unit risks
remain listed in the [knowledge pack, section 6](../../src/digitalmodel/subsea/flexible_pipes/docs/agent-pack.md#6-known-failure-modes-and-pitfalls).
