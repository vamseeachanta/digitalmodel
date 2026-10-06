# Uniform local metal-loss screening envelopes: API 579 Part 5 qualification and parametric study

Status: adversarial-reviewed preparation; qualified curves blocked by source audit.
Complexity: T2 preparation, T3 eventual engineering qualification.
Date: 2026-10-06 UTC (host local clock was still 2026-10-05). Client: N/A. Lane: codex.
Issue: https://github.com/vamseeachanta/digitalmodel/issues/2287 .
Execution: single-lane implementation, independent read-only adversarial review.
Owned paths: new diagnostic and tests, this specification, source-audit note,
asset_integrity row in module-routing and its routing test, study handoff.
Read-only: all production physics, private wiki and digitalmodel-data.
Forbidden: CP lanes, Elliott, client originals, secrets, licensed originals,
other registry rows, host/network/security configuration.

Create reusable screening curves with axial damaged length on X and minimum remaining wall on Y, with separate circumferential-width slices and separate Level 1/Level 2 results for four representative straight-pipe geometries. A Level 3 numerical comparison is a planned, gated follow-up. This study is independent of the Elliott plot revision and CP lanes.

## Existing-method evidence

Base digitalmodel revision: 2a52374d401e2f445d4baf435322f2cf18346c98.
The canonical path is assessment/ffs_coordinator.py (architecture record 2026-06-27); legacy API579_components is frozen. Existing ffs_acceptance_curves and ffs_lookup invert B31G/RSTRENG/DNV engines and cannot be relabelled API 579 levels. Level1Screener checks code-required thickness only. Level2Engine LML uses a simplified Folias relation, code-required t_min as reference thickness, a 10% loss segmentation threshold and row-count spacing; it does not use width. circumferential_defect provides a separate longitudinal RSF and net-section axial membrane screen, not a full Level 2 implementation. Level3 escalation only prepares a handoff.

## Governing basis and source gate

Target API 579-1/ASME FFS-1 2021 Part 5 for a single, blunt, locally uniform rectangular LTA in straight cylindrical pipe; Part 2 overall procedure and Part 3 brittle-fracture suitability remain prerequisites. Part 4 only for genuinely general thinning. No cracks, pits, weld flaws, interacting defects, creep, fatigue, dents, branches, elbows, supports, external-pressure collapse or nonpressure loads in the initial envelope.

Publisher ASME catalog confirms FFS-1 2021: https://www.asme.org/codes-standards/find-codes-standards/fitness-for-service . Local private wiki metadata states 2021 is not on disk, with older 2016/2007 records. Source IDs api-standards and ace-codes-standards-library remain the authority; metadata or passing self-consistency tests do not qualify equations. Obtain authorized exact 2021 clause/errata access, record edition, source digest/access evidence privately, verify permitted use, then independently audit Part 5 L1/L2 equations, Folias form, Tc/FCA, minimum-thickness limits, length/width, weld-efficiency limits, applicability and rerating rules. Do not copy licensed originals or tables to common Git. Resolve conflicting Folias implementations before publishing allowable walls.

Resource intelligence: private wiki joint-standard metadata is metadata-only;
the older asset-management page links 2007 mechanical extraction under a 2021
header. This is an edition-mismatch finding, not verified 2021 equation evidence.
The hub standards-transfer-ledger, code-registry and online-resource-registry
were inspected; no edition-matched computational authority was established.
Existing common validation and fixture anchors disagree with the square-root
polynomial in circumferential_defect; these are independent implementation
references, not a substitute for the normative source.

Named applicability gates to source-verify include Rt bounds, remaining-wall
floor, Lmsd (distance to major structural discontinuity), adjacent-flaw spacing,
weld proximity/efficiencies, length/width convention and the Part 4 versus Part 5
routing decision. Diagnostic samples can fall below candidate normative floors;
they are not normative PASS/FAIL. ROUTING_FORCED and NO_SOUND_REGION_IN_GRID are
explicit limitations on this synthetic algorithm-characterization fixture.

## Parametric basis (synthetic assumptions, not measured/project cases)

Four OD x nominal-wall geometries in inches: 12.750 x 0.375 (existing standalone FFS vector); 16.000 x 0.625 (legacy example geometry); 20.000 x 0.500 (lookup demonstrator); 30.000 x 0.375 (validation reference). These are representative ecosystem geometries, not claims of schedule or universal population coverage. Common assumed API 5L X52 SMYS 52,000 psi is a controlled comparison, not the grade of each precedent. B31.8-2022 pressure-thickness baseline F=0.72, E=T=1.0; pressure = 0.70 of 2*SMYS*F*t_nom/OD; ambient 20 C, closed-end pressure load, no superimposed force/moment/torque. Explicitly verify code applicability before a field screen. Nominal wall and sound-region wall initially equal; mill tolerance=0 as a study assumption, FCA=0.000 in, historical corrosion allowance=0.000 in, no future-life claim. Follow-up FCA sensitivity 0.020/0.050 in with separate sound-region and flaw reductions, after source qualification. RSFa=0.90 is a declared input to be source-verified, not an unexplained universal threshold.

Axial length: 0.5,1,2,4,8,16,32 in; normalized length s/sqrt(D_i*Tc) also retained. Width c measured as arc at mean diameter Dm=OD-t_sound; slices c/(pi*Dm)=0.02,0.10,0.25,0.50; angle=360*fraction deg. Internal and external loss require separate geometry labels; initial idealized external loss only. Synthetic grids have physical cell-center coordinates, 16 axial cells, 8 circumferential cells, explicit edge extent; baseline diagnostic force-selects LML to avoid grid-coverage routing artifacts. Residual wall bracket: 0.20*t_nom to t_nom; this 20% lower bound is a study bound, not normative acceptance. Never extrapolate beyond verified method bounds.

## Output and inversion contract

The qualified-output contract below is future work; the diagnostic instead
raises ValueError for invalid caller input and always withholds allowable walls.
Its pressure is a synthetic assessment demand passed to the existing
design_pressure_psi field; the intact Barlow reference is not a qualified MAWP.
At exact ties the unmodified engine comparison is retained, including floating
point behavior; no decision is inferred from the 0.70 sampling tie. Qualified
results must separate design/operating/assessment pressure and actual MAWP.

Each qualified cell includes inputs/units, pressure, grade/allowables, nominal/sound/flaw/FCA thicknesses, axial length and width/angle, level, method, standard edition/errata, source/code/config digests, applicability reason codes, criterion and margin. States: PASS, FAIL (valid calculation below criterion), INAPPLICABLE (outside method), UNQUALIFIED (source/validation incomplete), INVALID_INPUT, and BOUND_CENSORED. Null carries a reason; zero is physical zero. Raw algorithm verdicts remain separate and never overrule these states.

Evaluate applicability first. Invert each verified level by bracketed bisection in remaining wall only after checking monotonicity and no routing discontinuity; retain passing and failing brackets and residuals, tolerance <=0.0001 in. If the lowest evaluated wall passes, report lower-bound censoring; if nominal fails, report no solution at stated load; inapplicable cells have no allowable ordinate. Verify boundary plus points on both sides, grid refinement, unit conversion, width/orientation response and independent edition-matched worked examples. Do not interpolate across applicability boundaries or fabricate a Level 3 line. Level 2 is not assumed less restrictive than Level 1 until demonstrated.

Before inversion use at least 101 evenly spaced wall evaluations per qualified
length/width/level cell plus analytical monotonicity review of the verified
equations. Any increasing-to-decreasing criterion reversal or routing/status
change rejects bisection for that cell and requests specialist assessment.
Sampling alone is not proof of monotonicity. Candidate Level 3 stress-analysis
basis is Annex 2D in the target 2021 edition; exact Part 5 Level 3 links, plastic
collapse/local failure/strain criteria and safety factors remain source gates,
not predetermined engineering acceptance criteria.

## Authorized constructive preparation

Run existing L1/L2 routines in a small reproducible diagnostic sweep to characterize implementation behavior, not to deliver allowable walls. Add tests before runner implementation: no qualified outputs, correct physical extent, width omission visible, long-flaw flags override raw verdict, finite input validation, provenance and reproducible case counts. Numerical diagnostic files stay task-local; reusable runner/spec and links enter digitalmodel. No change to production physics until exact source verification supports it.

## Level 3 concrete compute proposal (execution not authorized here)

The spoken 'ICA' remains unresolved; no solver is inferred. Inspect existing digitalmodel ANSYS/CalculiX support and retained digitalmodel-data cylinder benchmarks for compatible cases before selecting a solver. Start with one intact and one damaged pipe and two mesh refinements (four pilot runs), closed-end internal pressure, large-deformation elastic-plastic solid analysis, measured/qualified temperature-dependent true stress-strain curve, smooth transition radius as explicit sensitivity, sound-boundary distance convergence, documented failure criterion and API579 Part5 Level3/Annex stress-analysis basis. After pilot, compare at three axial lengths x two widths x two walls near L1/L2 brackets for one size (12 damaged cases, two meshes each); expand only after review. Proposed cap: four pilot runs, <=2 h wall time/run, <=8 CPU cores, <=16 GB RAM/run, one concurrent licensed slot; actual host, solver/version/license, availability, disk quota, cost and stop conditions require preflight and compute approval. Stop on nonconvergence, >2% mesh sensitivity, criterion ambiguity or resource overrun. Numerical collapse pressure alone is not an FFS acceptance criterion. No new heavy/licensed runs or remote trust/security changes in preparation.

Inspected solver coverage: CalculiX fem_chain currently exposes plate-with-hole
creation/validation, not a verified damaged-pipe nonlinear collapse workflow.
Private digitalmodel-data ansys-cylinder-benchmark r4 publication metadata
identifies an existing retained dataset and limited cylinder-check evidence;
its receipt explicitly excludes engineering acceptance. Result compatibility,
rights and nonlinear damaged-pipe criterion were not verified; no values copied
or reused as Part 5 Level 3 results. This prevents needless rerunning of an
existing intact benchmark while preserving its actual validation scope.

Routing: the small diagnostic ran locally on a Windows workstation; no remote dispatch was
made. Existing hub fleet metadata declares ace-linux-2 as an SSH alias, but that
file is for fleet configuration fan-out, not verified solver readiness.
Preference for sustained generic computation is ace-linux-1/ace-linux-2;
licensed computation requires the designated licensed host such as RDS02,
subject to actual solver/license/resource verification. No live route, capacity
or license status is claimed by this preparation.

## Discovery and ownership

Extend existing digitalmodel docs/registry/module-routing.yaml asset_integrity entry with study/report/validation locators and status. This is navigation metadata, not a new result index. Preserve ffs_lookup and existing result matrices as query authorities. The broader report-artifact index is only an existing hub proposal; do not invent a parallel catalog. Reuse existing report/provenance helpers when publishing qualified results. Common digitalmodel owns workflow/schema/validation; private digitalmodel-data owns reusable numerical results with stable dataset/run IDs, pinned manifests and digests. Measured/client originals remain in owning private project repository; retain private evidence references rather than copying reports/grids. Catalog ace-share-ffs-tubular-precedent identifies existing studies but rights and current-source eligibility are unresolved; no private result promotion occurs in this slice.

Private wiki data/query_sources.json remains the established query-discovery
authority; it is not edited by this lane. The new navigation metadata is in the
existing common module row only. Coordination with the structural study owner
identified disjoint asset_integrity/structural rows; integration of shared
registry files remains serialized.
The division is explicit: query_sources navigates wiki knowledge; common
module-routing navigates code/report helpers; existing ffs_lookup/result
matrices query numerical results within their qualified methods.

## Acceptance and review

- Comprehensive specification, four-size input basis and bounded runner committed on isolated study branch; no CP or Elliott writes.
- L1/L2 diagnostic run and appropriate existing tests recorded with exact revision/digests and limitations.
- Source qualification findings explicit; requested allowable curves stay blocked until exact edition audit and independent validation pass.
- Report navigation links resolve; no new catalog/raw-report duplication.
- Adversarial plan and artifact review completed with findings addressed; unavailable providers recorded, never treated as approval.
- Draft PR, issue summary and handoff document remaining source/Level3 blockers; issue remains open until qualified envelopes are delivered.
