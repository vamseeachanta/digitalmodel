# Dent workflow qualification record — 2026-10-09

Workflow: `api579-dent-screen`. Dataset owner: digitalmodel; source: synthetic
manual geometry in `examples/workflows/api579-dent-screen/input.yml`, version:
synthetic-input-v1. Method: existing ASME B31.8 Nonmandatory Appendix R strain estimate
with parabolic apex radii and reversed curvature in both planes. API 579-1 Part
12 defines the scope; its worked-example method is not reproduced.

## Formula anchor

Case SYNTHETIC-DENT: OD 24 in, WT 0.500 in, dent depth 1.2 in, axial length
12 in, circumferential length 10 in. Weld association, restraint and coincident
gouge/metal loss are explicitly false. The inputs are synthetic, not transcribed
from a licensed example or measured asset.

The hand recomputation uses eps1 = 0.25 × (1/12 + 8 × 1.2/10²),
eps2 = 0.25 × 8 × 1.2/12², eps3 = 0.5 × (1.2/12)²;
eps_inside = sqrt(eps1² − eps1(eps2+eps3) + (eps2+eps3)²), and
eps_outside = sqrt(eps1² + eps1(eps3−eps2) + (eps3−eps2)²).
The greater value governs. Source of the anchor relations:
`dent_assessment.py` methodology and `test_dent_assessment.py` derivation anchor;
this record does not establish independent primary-source qualification.

| Case / quantity | Hand recomputation | Workflow value | Absolute difference | Criterion |
|---|---:|---:|---:|---|
| SYNTHETIC-DENT, maximum strain (1) | 0.04028750840314319 | 0.04028750840314319 | < 1e-12 | Absolute tolerance 1e-9 |
| Dent depth / OD (1) | 0.050000 | 0.050000 | < 1e-12 | Below existing 0.07 plain-dent depth screen |

Table 1. Regenerated synthetic formula checks, not published worked examples.

The calculated strain is below the existing 0.06 strain criterion. Both depth
and strain screens pass under the explicitly entered plain-dent conditions;
the shared screening verdict is ACCEPT. This disposition does not establish
fatigue life, pressure-cycle suitability or a pressure rating.

## Published cases and applicability

API 579-1 Part 12 published worked examples: **not evaluated**. No verified
published input/reference-value pair within this parabolic geometry method is
established in the repository. A published numerical difference cannot be
calculated. A strain-only formula anchor does not qualify a Part 12 assessment.

Manual geometry entry is required; dents are not detected from UT wall grids.
All geometry is finite, positive and in inches, depth < OD, WT < OD/2. Feature
flags shall be explicit booleans; missing/unknown values fail input validation.
The engine's joint depth, strain, weld, restraint and gouge checks are retained.
A depth greater than half either dent length invalidates the parabolic estimate
and yields ESCALATE. Dent-gouge interaction yields ESCALATE. Screening REJECT
maps to REPAIR (repair or higher-level assessment), MONITOR remains MONITOR,
and ACCEPT remains ACCEPT. Unknown engine dispositions raise an error.
No physical RSF, remaining life or pressure rerating is inferred from that map.

Fatigue, pressure cycling, liquid-service code qualification and quantitative
dent-gouge fracture are **not evaluated**. Catalog disposition: **workflow**;
`live` promotion requires a verified supported published case and a passing
comparison against its reference value. No licensed tables are committed.

Input integrity: SHA-256 `07a1b4a4fe70978bd7f7ce5214cd0759339a2c32eb93824957441ebcde3df827`.
