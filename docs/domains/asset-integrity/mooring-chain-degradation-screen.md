# Mooring-chain degradation screen

The offline `mooring-chain-degradation-screen` workflow combines the existing
local dimensional screen, mechanism evidence gaps, the shared FFS decision
engine and an HTML report. A local dimensional result does not establish
continued-service fitness. The overall verdict is `ESCALATE` until qualified
strength, fatigue and defect methods are available.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| Initial | 2026-10-20 | Dimensional intake, evidence gaps and reporting are connected to prevent unsupported acceptance. |

## Method and scope

The dimensional calculation follows [IACS Recommendation 38, revision 2,
July 2020](https://iacs.s3.af-south-1.amazonaws.com/wp-content/uploads/2022/05/20093306/rec38rev2.pdf),
sections 1, 5.1, 5.2 and 5.4. Only the existing mobile-unit studded-chain
subset is supported. Permanent installations and studless chains are outside
this dimensional method. API RP 2MIM and DNV-OS-E301 are governing-practice
references for future qualification; this workflow does not claim compliance.
No licensed criteria, MBL relationship, pit coefficient or fatigue curve is
embedded. Every operator limit and measurement uncertainty is required.

For perpendicular diameters a and b, mean diameter d = (a + b) / 2.
With absolute uncertainty u, the interval is [d - u, d + u]. Diameter loss
is (D - d) / D. Equivalent circular area ratio is (d / D) squared, derived
from circular geometry. These equations and limits are implemented by
`asset_integrity.mooring_chain_dimensions`; the existing
[method note](mooring-chain-dimensional-screen.md) defines detailed applicability.
This ratio is neither an irregular-section area measurement nor residual MBL.

Bend and fairlead-shoulder readings receive local interval comparisons.
Interlink double diameters do not resolve individual worn diameters:
[BSEE, Study on Mooring System Integrity Management for Floating Structures,
2015](https://www.bsee.gov/sites/bsee.gov/files/tap-technical-assessment-program/730-aa.pdf),
Table 7, identifies the measurement limitation. No wear projection is performed.

Visible cracks trigger withdrawal pending engineering disposition. Loose or
missing studs, geometry anomalies, pitting and unknown inspection observations
raise applicability flags. These are escalation policies, not replacement or
repair qualifications. Dimensional exceedance also triggers withdrawal; an
uncertainty interval crossing a limit requires remeasurement.

## Three slices and remaining evidence

1. Intake and local corrosion/fairlead dimensional screening reuse typed
   inspection records, required operator limits and uncertainty propagation.
2. Mechanism records explicitly mark residual MBL, pitting classification and
   stress concentration, interlink wear, ovalization tolerance and remaining
   fatigue life `NOT_EVALUATED`, naming the missing evidence. Shared
   `ffs_decision` receives applicability flags and issues `ESCALATE`.
3. The router supplies a JSON-compatible result and HTML through `ffs_report`.
   The basename is `mooring_chain_degradation`; the stable workflow identifier
   is `mooring-chain-degradation-screen`. No solver is imported or started.

Chain FEARS small/medium/mega boundaries and validated pit strength factors
are not in the opened sources. Qualified degraded-link capacity and break-test
benchmarks are also absent. These gaps cannot be closed by multiplying nominal
MBL by area ratio. The existing `mooring_fatigue` workflow and S-N library use
welded-detail curves; no qualified reduced-section chain curve has been
established. They are deliberately not called for a chain-life verdict.

Deckhand service routing is outside this repository change; the callable router
and stable identifier provide the integration boundary. Published corrosion/MBL
golden tests and T2 fatigue qualification remain later slices requiring accessible
reference data and applicability evidence. The tests validate arithmetic and
fail-closed disposition, not engineering capacity.

## Worked example: inputs, method and answer

All values below are synthetic arithmetic inputs, not inspection data or
standard acceptance criteria. The supplied limits have no design authority.

```yaml
basename: mooring_chain_degradation
mooring_chain_degradation:
  component_id: demonstration-chain
  inspection:
    nominal_diameter_mm: 100.0
    application: mobile_offshore_drilling_unit
    grade: R3
    stud_attachment: mechanical_and_other_end_fillet_weld
    min_area_ratio: 0.8
    max_diameter_loss_fraction: 0.08
    operator_limit_reference: Synthetic arithmetic example only
    readings:
      - location_id: bend
        region: bend
        diameter_a_mm: 96.0
        diameter_b_mm: 98.0
        uncertainty_mm: 1.0
    uniform_loss_confirmed: true
    perpendicular_readings_confirmed: true
    coverage_confirmed: true
    visual:
      crack: false
      loose_stud: false
      missing_stud: false
      geometry_anomaly: false
      pitting: false
```

Independent calculation: mean = (96 + 98) / 2 = 97.0 mm. The uncertainty
interval is 96.0 to 98.0 mm; diameter loss is 2.0 to 4.0 %; circular area
ratio is 92.2 to 96.0 %. The whole interval meets the synthetic limits.
Local status is `WITHIN_LIMITS`; overall verdict is `ESCALATE` because MBL,
fatigue and other mechanism qualification remain unestablished. Remaining MBL
and fatigue life are JSON null, never zero or inferred acceptance.

Load the YAML as `cfg` and call
`digitalmodel.asset_integrity.mooring_chain_degradation.router(cfg)`.
The result retains the inputs, `result` and `report_html` without writing files.
The caller controls report persistence. `tests/asset_integrity/test_mooring_chain_degradation.py`
executes this embedded example and gives independent hand calculations for
numerical expectations. The existing dimensional tests cover limit equality,
uncertainty overlap, invalid inputs and exclusions. No published chain capacity
validation is claimed.
