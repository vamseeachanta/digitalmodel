Build on the existing structural-ffs source/domain catalogs, AISC shapes data, marine materials and plate/panel algorithms. Related #2202 and #1057; coordinated with pipe study #2287 and CP owner. User authorizes public-source research, structured database curation, ordinary calculations, independent review/tests and draft PRs; no heavy/licensed solver jobs, security/trust changes or certification.

Database first: retain source URL, issuer, edition/date (explicit unknown where unpublished), retrieved date, units, material/condition, shape/dimension constraints, section-property axes/basis, existing-source IDs and readiness/rights status. Preserve existing data/aisc_shapes.yaml instead of copying it. Distinguish manufacturer supply ranges from discrete stock availability and project geometry. Class standards remain metadata-only, no licensed originals or tables copied.

Audit/validate existing algorithms before generating research curves. Bound to supported geometry/loading. Existing local-patch algorithm's assumed supports and claim of conservatism require independent validation before local damage length/breadth can yield acceptance. Quantify uniform whole-field thinning and plate/stiffener loss separately; do not label buckling alone API579 or asset FFS acceptance.

Extend study to precomputed minimum remaining-thickness views. Structure/profile/span/breadth/material/stress/corrosion/support controls retrieve only already computed qualified combinations; missing/out-of-range/inapplicable values explicit. No automatic simulation.

Execution: isolated clone under task-9, branch study/ship-plate-profile-buckling. Owned: data/ship_structure_reference/, scripts/studies/ship_plate_buckling.py, tests/structural/test_ship_plate_study.py, docs/domains/plate-buckling/ship-structure-study.md. Readonly: source catalogs, AISC and existing algorithms. Forbidden: CP, asset_integrity shared helpers/registry row, client data, credentials/lockfiles. Shared catalog/discovery additions require owner coordination and separate claim.

Compute: existing Linux2 sim-worker route inspected; strict host-key probe failed. Never change trust. Lightweight fallback on current Windows host; no licensed or heavy FE run. Qualification limitations and evidence must be exposed. Persist solver outputs in private digitalmodel-data through existing ownership rules; temporary outputs are replicas. Source-specific rights unresolved records cannot be published as reusable extracted datasets. Continue permissible authored research metadata and synthetic computational fixtures.

Validate schema/units/provenance/unsupported cases and independently check elastic square-plate formula and thickness-squared scaling. Run existing panel tests; review plan/code independently and record unavailable providers explicitly. Draft PR only, no merge.

## Inventory and research checkpoint, 2026-10-06 UTC

Existing owner paths: source catalog and structural-ffs entry in llm-wiki;
data/aisc_shapes.yaml (v15.0 header; inch units); models.MARINE_GRADES;
structural_analysis/buckling.py, panel_buckling.py, grillage_buckling.py;
plate_metal_loss_ffs.py; examples/workflows/plate-buckling; existing panel
validation record 2026-06-26. No general released report registry was established
in inspected paths; query_sources.json is a discovery surface, not a result store.
No CP, pipe helper, shared registry or client artifact was edited.

Local source_research.yml and catalog.yml contain factual manufacturer research
observations, deliberately excluded from this public draft while source-specific
rights and editions remain unresolved. Existing DataCatalog YAML format is reused;
live import is blocked in the existing environment by NumPy2/pyarrow14 incompatibility.
Plain YAML readback is verified; integrated catalog loading is not claimed.
The data README states minimum schema, coverage and gaps. No original PDF copied.

## Method audit and bounded calculation scope

Current DNV landing page gives edition 2023-09, while codes.DNV_RP_C201 identifies
2010. No current-edition clause compliance is established. The implementation
uses k=4 for a/b >=1 and Johnson-Ostenfeld inelastic correction. This is a model
approximation, not an exact finite-panel mode search or engineering qualification.

The plate routine ignores sigma_y and its boundary_conditions argument. The study
rejects transverse compression, shear, supports other than simply-supported,
unknown input keys, invalid/nonfinite values and unsupported profiles. Material
is fixed assumed AH36 (fy355, E206000 MPa, nu0.3); FCA0, gamma1.15, fixed stress,
physical span/breadth, initial plate thickness12 mm. Fixed force would raise local
stress as thickness falls and requires a separate study. Numerical floor0.5 mm is
only an evaluation bound, never a practical remaining-thickness acceptance limit.

The local-patch wrapper creates supported edges at the damage boundary. This can
suppress parent-panel buckling modes and increase computed capacity for smaller
patches. Its claimed conservatism is not substantiated for embedded corrosion.
Damage length/breadth acceptance remains INAPPLICABLE; no API579 L1/L2 claim.
An independently benchmarked variable-thickness/support model is required before
local-damage thresholds. Such a model/FE job has not been executed or authorized.

Panel results are illustrative only: full effective plate width, one stiffener,
no lateral pressure, angle eccentricity, girder interaction or effective-width
reduction. Web loss is total uniform web-thickness deduction; flange dimensions
unchanged. Existing intermediates reproduce one tee example; that does not
validate arbitrary panel threshold crossings. Bulbs are not approximated as tees.

40 synthetic combinations: 8 plates and 32 flatbar/welded-tee panels; spans600/1200,
breadths400/600 mm, fixed compression50/100 MPa, web deductions0/1 mm for panels.
All are authored model cases, not stock products or actual bulkhead geometry.
Plain plate thresholds at b400 are 3.5147/4.9706 mm for 50/100 MPa; at b600,
5.2721/7.4559 mm. Span invariance follows the k=4 approximation, not universal
structural behavior. Panel thresholds are not qualified engineering curves.

40 tests pass: 19 bounded adapter checks and 21 existing panel tests. Independent
elastic square-plate formula, t-squared elastic stress scaling, JO continuity,
threshold crossing, failed nominal, invalid inputs and exact lookup are covered.
No interpolation or runtime solver dispatch occurs in the Plotly lookup.
Separate structure/profile/span/breadth/stress/web-loss/loss-model controls select
saved rows only. Browser checks verified plain-plate display, uncomputed span900,
inapplicable local damage and illustrative panel status. Material/support/FCA
remain fixed visibly; no arbitrary new parameter combination is computed.

## Execution, ownership and remaining gates

Actual host ACMA-WS014, Windows, Python3.11.15. Linux2 registry identifies a
sim-worker, but strict SSH probe failed for absent trusted host key. Trust remains
unchanged. No GPU/licensed/heavy job ran. Lightweight Windows work was retained.

Numerical output owner: private digitalmodel-data, branch
study/ship-plate-synthetic-results, docs/reviews/ship-plate-study/. Temporary
ship-plate-results is an execution replica. manifest.json pins algorithm hashes,
workflow revision/dirty status, result digest, execution time and content-based
run identity. No client input or existing dataset/run identity is substituted.

Codex independent plan and artifact review identified ignored transverse stress,
imaginary patch supports, extra ignored inputs, thickness floor and run evidence;
adapter gates address these. Claude plan review approved with limitations and a
data-checkout persistence gate; checkout rebuilt sparse from main before writing.
Gemini unavailable: authentication missing. Provider settings were not changed.
Final Codex artifact review passes and independently reran 40 tests. Claude
artifact review approved with limitations; its minor input/evidence corrections
were applied and rereviewed by Codex. Source schema and source relationships pass;
schema is a staging contract, not a complete engineering readiness validator.
Results must still be committed and remote head/hash verified for retention.
This issue stays open for catalog rights/current-edition
audit, discrete stock coverage, qualified panel/local-damage thresholds and richer
independent lookup controls. No source catalog readiness or asset acceptance.
