# Riser-joint FFS workflow validation record

Scope: [issue 2183](https://github.com/vamseeachanta/digitalmodel/issues/2183),
[epic 1057](https://github.com/vamseeachanta/digitalmodel/issues/1057) refreshed Wave 1 plan-lite.
This record establishes composition and offline execution of the existing engines.
Independent qualification of the measured fleet is not established.

The fixture-backed example is [input.yml](../../../examples/workflows/riser-joint-ffs/input.yml).
Its generated [report](../../../examples/workflows/riser-joint-ffs/report.html) carries
SHA-256 digests of all four grids, the GML register and the anonymization README.

## Envelope comparison

The governing comparison case uses 21.25 in OD, 0.875 in nominal wall, X80 and
3,000 psi design pressure. This elevated pressure exposes finite envelope
boundaries; the inspection demonstration uses 500 psi. Values below are
regenerated outputs, not transcribed standards tables. Base-metal depth is
0.75 of nominal wall (0.65625 in); length is rounded to 0.001 in by the engine.

| Method | Envelope length (in) | Direct engine pressure (psi) | Difference from 3,000 psi (psi) | Criterion |
|---|---:|---:|---:|---|
| Original B31G | 17.751 | 3000.001014 | +0.001014 | absolute difference ≤ 0.2 psi |
| Modified B31G | 8.176 | 2999.986934 | -0.013066 | absolute difference ≤ 0.2 psi |
| DNV-RP-F101 | 7.664 | 2999.966540 | -0.033460 | absolute difference ≤ 0.2 psi |

Table 1. Rounded envelope points reproduce the direct pressure engine within the stated tolerance.

For each row, 0.999 times the envelope length returns pressure above 3,000 psi;
1.001 times the length returns pressure below 3,000 psi. The 0.2 psi tolerance
covers 0.001 in length rounding, not uncertainty in the inspection measurements.
Original B31G has a discontinuity at its long-defect branch; a pressure equality
criterion does not apply at that branch switch. The tabulated point avoids it.

For the weld region, BS 7910:2013 Option 1 gives an envelope length of 7.887 in
at depth fraction 0.35 (7.77875 mm), using the engine's default Charpy correlation
and the exact Barlow membrane stress. Direct crack-FAD evaluation gives:

| Length multiplier | Lr | Kr | F(Lr) | Disposition |
|---|---:|---:|---:|---|
| 0.995 | 0.637917467 | 0.894906400 | 0.895619265 | Kr < F(Lr), Lr < Lr_max; inside envelope |
| 1.100 | 0.642480073 | 0.907140225 | 0.893848149 | Kr > F(Lr); outside envelope |

Table 2. The weld envelope brackets the direct FAD acceptance boundary.

These checks are pinned by `test_riser_joint_workflow.py` and the existing
`test_riser_joint_ffs.py` boundary tests. Supporting engine records are
[B31G validation](b31g-validation-2026-06-27.md),
[consolidated validation, B31G and F101 rows](ffs-validation-record-2026-06-27.md),
and [BS 7910 engine validation anchors](../../../tests/asset_integrity/test_crack_fad.py)
(curve against the legacy implementation, Newman–Raju limit and Charpy correlation).
No separate BS 7910 narrative validation record exists on the inspected revision;
the executable anchors supply that comparison. No licensed source table is copied here.

## Fixture and fleet criteria

Four scan assessments are returned with start/end envelopes, measured minimum
wall, collapse-limited water depth and one placement assessment per scan.
RJ-101 has two grids representing the same Box End scan: neither is counted as
an additional fleet joint. Roll-up uses the Main register's 26 distinct joint IDs;
fit plus repair equals 26 for each of four campaign horizons. Campaign-end
lengths shall not exceed corresponding start lengths. Hashes shall match the
exact bytes parsed, including the register and provenance README. Missing cells
are excluded and their count is reported; empty, nonpositive and nonfinite
measured grids are rejected. Every Main register row contributing to fleet counts
shall carry a joint ID and finite nonnegative remaining life; invalid unselected
rows are rejected before group-by, rather than silently dropped. Numeric practice
zone margins, default weld Charpy energy, bending assumption and collapse factor
are shown in the report basis table.

The example references the existing anonymized baseline inspection excerpts in
`tests/asset_integrity/test_data/real_inspection/`; it makes no copies. Their
README documents removal of operator, contractor, rig, well, personnel, project,
timestamps and serial identifiers, replacement with synthetic RJ IDs, rounding
and axial downsampling. Private originals remain off this public repository;
this workflow neither accesses nor republishes them. Public fixture use is
limited to the existing excerpts authorized for this demonstration.

## Grid-bound results

| Grid | Measured minimum (mm) | Collapse depth limit (ft) | Placement |
|---|---:|---:|---|
| RJ-101 Box End ds8 | 15.024 | 2133.7 | REPAIR: below half of 5000 ft campaign depth |
| RJ-101 Box End full resolution | 15.024 | 2133.7 | REPAIR: below half of 5000 ft campaign depth |
| RJ-102 Box End ds8 | 15.348 | 2265.4 | REPAIR: below half of 5000 ft campaign depth |
| RJ-103 Pin End ds8 | 16.549 | 2791.3 | RESTRICTED: above half-depth threshold, below 5000 ft |

Table 3. Regenerated collapse/placement outputs use the fully evacuated head basis.

The four-grid registry tests pin measured minimum after mm/in conversion to
1e-12 in and rounded depth to 0.05 ft. The minima refer only to the retained
measured stations. The unrounded collapse depths are 2133.699867, 2265.424402
and 2791.315171 ft, respectively. The governing joint lives are 3.46, 8.70 and
30.73 yr for RJ-101, RJ-102 and RJ-103. A lower-life sibling-row regression
requires every placement assessment to use the joint-minimum life. For the
3,000 psi comparison case, all three base-metal methods also pin a strict
campaign-end envelope reduction; the demonstration's low-pressure caps do not
provide that discriminating check.

## Limits and catalog disposition

Nominal-wall idealized metal-loss envelopes are not a spatial assessment of the
C-scan pit morphology. Collapse and placement use measured minimum wall at the
time of inspection; placement life is the minimum over every Main row of the joint; envelope
corrosion rate comes from the matching Main/joint/scan-location register row.
One governing placement per joint uses the lowest collapse limit among supplied
grids. Axial downsampling does not establish a full-resolution minimum. No fatigue growth calculation is performed.
Weld bending, measured toughness, full coverage of unmeasured stations and
independent engineering qualification of fleet operation are not established.
Fleet roll-up is life-based and does not establish collapse qualification for
unscanned joints. Practice zone margins and default weld assumptions remain
visible in the report. The catalog is promoted to `workflow`, with this bounded
composition record linked; `live` qualification is not asserted.
