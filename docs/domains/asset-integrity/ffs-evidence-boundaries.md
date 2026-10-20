# FFS adapter evidence boundaries

The adapters separate observed wall measurements, absent evidence and screening
vocabulary. Missing pit populations do not establish nominal-wall measurements
or unity strength. Missing life does not establish a short-life finding, and a
rerating verdict requires a calculated limit. These are software boundary checks;
published-case qualification and operating approval are not established.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-20 | Separate observations from defaults and disposition tokens to prevent unsupported physical findings. |

## Scope and caller inventory

The inventory covers Python callers in `src`, `scripts` and `examples`, including
imports and re-exports. Refs #2339; related #2182. Three slices are implemented:
measurement characterization, decision evidence, and adapter/report integration.

| Caller | Evidence consumed | Disposition and limits |
| --- | --- | --- |
| `assessment/pitting.py:assess_pitting` | Grid readings, nominal wall, supplied or uniform-index pitch | Pit-only minimum/mean are null without pits; measured minimum/mean remain separate. No pits or unknown pitch makes equivalent-LTA strength `NOT_EVALUATED`. |
| `assessment/ffs_router.py:_pitting_morphology` | Pit counts and background median | Missing background cannot satisfy the morphology heuristic. The heuristic is classification, not acceptance. |
| `assessment/ffs_coordinator.py:assess_component` | Characterization and Level 1/2 results | Unsupported pitting escalates; nonfinite strength/rating is null in the indexed summary. Unknown corrosion rate remains absent. Explicit pitch is accepted. |
| `assessment/ffs_workflow.py:FFSWorkflow.router` | Configuration component/grid/pitch | Forwards explicit pitch without inferring it from a single coordinate. |
| `assessment/pitting_workflow.py:_assess` | Explicit pitch, pit statistics and independent applicability gates | Uses shared pitch handling; no missing-life numeric sentinel. Preserves the existing synthetic-screen qualification limits. |
| `assessment/ffs_decision.py:FFSDecision.decide` | Measured minimum, required wall, rate, RSF and optional pressure | Reports both wall and RSF comparators. Missing rate is distinct from an explicit zero rate. |
| `dent_workflow.py:_decision` | Dent engine disposition and actual dent comparator | Uses `from_screening`; no artificial RSF or life numbers are passed to the strength decision tree. |
| `pipeline_defect_screen.py:_decision` | Calculated allowable/demand ratios and pressure/stress limits | Compares the actual ratio against unity and reports calculated demand limits directly; life remains absent. No infinity token is needed. |
| `scripts/reporting/build_pipeline_ffs_report.py` | Calculated strength, life and ratings | Existing physical evidence is retained. A future deficient-margin case without an injected rating callable escalates under the shared contract; no report is regenerated here. |
| `assessment/__init__.py` | Public re-exports | No additional physical calculation or default. |

Paths in the table are relative to `src/digitalmodel/asset_integrity`, except the
explicit script path. Remaining-life and rating compatibility helpers in
`FFSDecision` delegate to the same physical functions; they do not interpret
screening tokens.

## Input and output contract

- Raw observations: `measured_min_in` and `measured_avg_in` reduce finite grid
  readings. No finite readings means null. Pit-only minimum/mean are null when
  there are no detected pits; a pit-free grid still has observed wall readings.
- Background median is null when no background was observed. The classification
  adapter treats it as unavailable rather than substituting pit wall.
- Pitch: explicit `row_spacing_in` is positive and finite, in inches. Otherwise,
  at least two increasing, uniformly spaced numeric index values are required.
  `spacing_basis` distinguishes supplied pitch, uniform index and unavailable
  pitch. A numeric unit index is a caller convention, not measurement provenance.
  Nonuniform or ambiguous pitch requires explicit input. Circumferential pitch
  remains assumed equal to axial pitch for centroid spacing.
- `None` or NaN life means not evaluated. A failed independent screen with an
  acceptable margin and absent life escalates instead of selecting a short-life
  repair. A passing current strength screen may be reported with life absent.
- Positive infinite life retains its existing meaning only for an explicit
  zero-degradation projection. Adapters must not use it as a decision token.
- Below-allowable margins without a calculated reduced rating escalate. Existing
  policy replacement/monitor/repair bands are retained; they are not newly
  qualified standard limits. Applicability failures suppress rating evaluation.
- `from_screening` translates ACCEPT, MONITOR, REPAIR or ESCALATE without any
  physical margin, life or rating. It requires the mechanism's actual governing
  comparator. Dent reports continue to identify the dent engine criterion.
- Reports render missing, NaN and negative-infinite life as “Not evaluated”. Only
  positive infinity is rendered as the explicit no-further-corrosion assumption.
  Legacy decision reports identify both minimum-wall and RSF comparators, so a
  wall failure is not described as an RSF failure.

## Method and sources

No standard coefficients or acceptance limits are added. The geometric identities
are cell-centred length = occupied axial row count × pitch and Euclidean distance
between component centroids, using the existing equal-pitch convention in
[`pitting.py`](../../../src/digitalmodel/asset_integrity/assessment/pitting.py).
Two half-pitch rows preserve a single cell's physical length when passed to the
existing equivalent-LTA engine; the reported flaw extent is the supplied pitch.

The existing linear-life definition is excess wall divided by corrosion rate,
implemented by `ffs_decision.remaining_life`. The existing pressure scale is
initial rating × min(unity, RSF/allowable), cited in `reduced_mawp` to API
579-1/ASME FFS-1, 2021, section 2.4.2.2. These repository equations are retained;
this change does not independently validate their standard attribution. The
pit threshold and deepest-ligament policy remain the existing documented choices
in `pitting.py`; no licensed chart or table is transcribed.

The attempted public ASME FFS-1 product-page lookup returned HTTP 404. No numeric
value is taken from that response. Edition-matched Part 6 coupled-pit coefficients
and published worked examples remain unavailable and not evaluated.

## Worked example

Run with the source package and its numerical dependencies available:

```sh
PYTHONPATH=src python examples/asset_integrity/ffs_evidence_boundaries.py
```

All inputs are synthetic choices for arithmetic, not measured-component data.
Nominal wall is 0.5 in. Pit-free readings 0.48 and 0.49 in give an observed minimum
of 0.48 in and no pit-only minimum. A separate single row `[0.3, 0.5, 0.3]` with
explicit pitch 0.4 in has one occupied axial row: extent = one × 0.4 = 0.4 in.
Its two separated pit centroids are two columns apart: spacing = two × 0.4 =
0.8 in. The equivalent-LTA engine receives that same 0.4 in axial extent.

Separately supplied RSF 0.72, allowable 0.9 and initial pressure 1000 psi give
1000 × 0.72 / 0.9 = 800 psi through the existing rating callable. These strength
inputs are independent of the example pit grid. No life/rate evidence is supplied,
so remaining life is null. This arithmetic example establishes no component
acceptance or published-case qualification. The test docstrings write out each
expected result independently of the implementation.

## Later work

Reference-wall and outside-flaw applicability qualification across all Level 2
callers remains tracked separately in #2345. Part 6 coupled-pit charts,
published-example qualification, full circumferential geometry and validation of
the legacy engineering-policy bands are not added. General LML spacing defaults
outside the pitting-characterization path are unchanged. Existing generated
catalogs and report artifacts are not regenerated by this boundary audit.
