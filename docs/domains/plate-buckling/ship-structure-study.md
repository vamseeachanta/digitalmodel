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

Execution used a Windows workstation, Python3.11.15; exact host is in the private
owner manifest. Linux2 registry identifies a
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
Result retention is verified in private digitalmodel-data draft PR44, commit
95ddcd3a4ce34378309339e2a191484b372f0a98, owner-relative
docs/reviews/ship-plate-study/manifest.json. Run identity
ship-plate-synthetic-43aa52bfd6b78cc4; precomputed.json SHA256
43aa52bfd6b78cc42c01ce5550ddc1e4a16634d8a7a49ae0a7f59989c06b5c23.
All nine remote evidence blobs match retained bytes; JSON and HTML digests match
the manifest. Published workflow095312dd source blobs match the executed code
after Git line-ending normalization; exact executed byte hashes remain recorded.
Draft PR2290 owns public methods/schema/tests; no merge or engineering acceptance.
This issue stays open for catalog rights/current-edition
audit, discrete stock coverage, qualified panel/local-damage thresholds and richer
independent lookup controls. No source catalog readiness or asset acceptance.

## Continuation: numerical evidence and local-loss diagnostics

The eight plate rows are **numerically verified screens of the implemented k=4
idealization**, not engineering-qualified cases. The exact screen is longitudinal
compression only, zero shear/transverse stress, actual simply-supported full-field
geometry, uniform loss, assumed AH36, E=206000 MPa, nu=0.3, fy=355 MPa, gamma=1.15,
FCA=0, nominal thickness12 mm, and fixed applied stress50/100 MPa. The eight rows
are the Cartesian product L600/1200, b400/600 and stress50/100. At every crossing,
gamma*sigma_x is 57.5 or115 MPa, below fy/2=177.5 MPa, so the crossing lies on the
elastic branch. Independent solution:

`t_cross = b * sqrt(gamma*sigma_x * 12*(1-nu^2) / (4*pi^2*E))`.

This gives b400:3.5147/4.9706 mm and b600:5.2721/7.4559 mm, for either saved span.
The adapter independently checks this formula, thickness-squared scaling, JO
continuity, pass-side threshold crossing and explicit unsupported-input rejection.
The original 40-case result digest remains43aa52bfd6b78cc42c01ce5550ddc1e4a16634d8a7a49ae0a7f59989c06b5c23.
The32 panel rows have null minimum_remaining_thickness_mm because the combined
panel threshold has no equivalent independent benchmark; full effective width,
torsional restraint assumptions, mode switches and excluded pressure/interaction
remain unresolved. Their separate illustrative_threshold_mm must not be promoted.

A separate18-row/54-point diagnostic now retrieves the **existing**
assess_plate_local_loss wrapper for patches300/600/1200 mm long and150/300/600 mm
wide, parent1200x600x12 mm, stress50/100 MPa, losses0/4/8 mm. No new mechanics is
implemented. Every row explicitly has acceptance_status=inapplicable_unvalidated_local_patch
and null minimum_remaining_thickness_mm/maximum_accepted_loss_mm. Wrapper passes,
Level2 framing, max acceptable loss and conservatism claims are not projected.
The data-only HTML compares the isolated artificially-supported patch response
with uniform thinning of the full parent; neither is an embedded-damage solution
or a proven bound. Missing saved patch combinations display UNCOMPUTED.

The full-sized patch reproduces uniform thinning, which verifies orchestration
only. At zero loss a smaller named patch already changes the returned utilization
although the physical parent is unchanged: parent modes are omitted. An elastic
limit check using parent1200x600x2 mm and patch600x300 mm gives patch utilization
one quarter of parent utilization. This is a reproducible failure of the wrapper
as a complete parent-panel assessment, not proof of a measured capacity benefit
or a general safety bound. Nine diagnostic tests pass, including the limiting
checks and invalid/nonfinite/oversized input rejection. The original40 calculations
were not rerun or overwritten.

Local-damage acceptance requires a validated variable-thickness parent-field
method preserving real edge supports, damage location/shape, membrane-load
redistribution and governing global/local modes; independent limiting-case and
published/mesh-converged benchmarks; and a governing class-edition criterion.
The inspected ecosystem contains no such verified route. New heavy/licensed FE
jobs remain outside this authorization; additional ordinary wrapper runs cannot
resolve this missing model. Patch position and measured thickness map are also
required for any later project assessment. No asset inputs have been invented.

Source staging is extended to five sources, two supply records and four sections:
two equal-angle rows are sourced to British Steel's printed CEAD:ENG:072026
identifier with publisher x/y and principal u/v axes distinguished. Marine-grade
and actual heat certification are unknown; angle solver eccentricity qualification
is still absent. SSAB's inspected Delivery Conditions now resolves the previously
unknown typical product condition while keeping individual delivered condition
unknown. [Structured source pointers](../../../data/ship_structure_reference/source_metadata.yml)
provide discoverability without publishing the excluded numeric source records or
introducing a replacement registry. Existing corrugated_bulkhead.py was also found;
it is PRELIMINARY and requires a combined CSR benchmark, so remains outside this
flat-plate study. Flat-bar/rolled-tee stock matrices, plate width/length tables,
IACS governing edition, bulb axes and source-specific distribution rights remain
precise database gates.

The separate diagnostic manifest records UTC with +00:00. The preserved original
manifest's executed_utc string has offset-05:00; it denotes2026-10-06T01:19:43Z,
and is not a naive UTC timestamp. No original run record was rewritten.

Continuation readback also exercised the existing DataCatalog.load_catalog/load
methods by loading their existing module file directly. Both local research and
the unchanged AISC YAML datasets loaded successfully. This avoids the legacy
package initializer, not the existing data pipeline; no replacement loader or
mocked dependency was written. Optional pyarrow ABI warnings persist. This verifies
the YAML catalog route only, not package initialization, Parquet or database readiness.

Independent Codex and Claude artifact reviews passed: both reran nine tests;
Claude independently reproduced all54 diagnostic points from k and JO equations.
Gemini remains unavailable; no authentication or trust configuration changed.
Browser checks verified saved-patch, full-size-patch and UNCOMPUTED retrieval.
The previous public CI client scan rejected a physical hostname in this document;
it is replaced with a generic workstation label while exact private provenance
is retained. No deny pattern, exclusion, baseline or security configuration changed.

Diagnostic length sensitivity is specifically limited: k=4 whenever patch length
is at least its breadth. Only the300x600 mm patch in this grid uses the shorter-field
branch, k=6.25. These artificial-support effects are not measured corrosion-length
benefits. Patch position is absent from the wrapper.

The diagnostic owner is private digitalmodel-data commit
4754b1cea1590f06c00e8e6f7956a3fd0eb2646a, path
docs/reviews/ship-plate-study/local-loss-diagnostic/manifest.json, run
ship-local-diagnostic-a30c6daa794e7cf8. Result SHA256
a30c6daa794e7cf8be81ae775be67034779d474632cfa86818360e52f3745036;
published code10bbc73af131f6cb85ff477ec7dca350d0882dae. Original40-case owner
and result hashes remain unchanged. Private draftPR44 carries both runs.
