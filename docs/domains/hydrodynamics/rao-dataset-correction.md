# Half-model RAO dataset correction

The offline correction tool stages warnings and evidence-bound replacements for
issue #3122. It preserves the supplied snapshot and writes a new directory. It
does not upload data, run a solver, infer physical acceptance limits or establish
engineering qualification. Missing evidence leaves publication NOT_EVALUATED.

## Revision history

| Revision | Date | Change and reason |
|---|---|---|
| Initial | 2026-10-21 | Add warning, replacement and release controls for half-model results. |

## Source basis and limits

- Issue #3122 (2026), Decision items 1–3: warn both affected entries without
  removing other entries; replacement requires full-body buoyancy, mesh and
  solver-agreement gates; both publication steps require claims verification.
- Issue #3121 (2026), Problem and Wanted: a half-model declared as a whole body
  invalidates the resulting whole-body RAOs. Multiplying those RAOs by a guessed
  factor cannot establish a corrected solution.
- Issue #1548 and the investigation in PR #3066: native results and reviewed
  meshes are still needed. This tool consumes exported observations and gate
  evidence, independently of the investigation package.
- The public dataset card, schema 1.1.0, inspected 2026-10-21, declares
  `rao.parquet`, `hydrostatics.parquet` and a `vessel_id` column. Its identifiers
  do not fully resolve the issue's affected-entry mapping. Mapping every affected
  identifier is required input; neither an alias nor an absent entry is guessed.

No hydrodynamic equation, numerical coefficient or acceptance limit is introduced.
Qualification criteria and their cited engineering sources belong in the supplied
gate evidence. The tool verifies evidence presence and byte digests; it cannot
determine whether a reported engineering conclusion is correct or a reviewer is
authorized. Maintainers must inspect that evidence before publication.

## Slice 1: warning bundle

Retain a complete local snapshot, including the existing `README.md` and every
table and ancillary file. Provide `affected.json` with both required entries:

```json
{
  "installation": ["confirmed-identifier-a"],
  "parametric": ["confirmed-identifier-b"]
}
```

Lists may include confirmed aliases; identifiers must be distinct, nonempty and
present in `rao.parquet`. An incomplete mapping refuses the entire operation.
Confirm the correspondence with the authoritative source before supplying it.

```bash
python -m scripts.hf.rao_correction flag snapshot flagged \
  --affected affected.json --date 2026-10-21
python -m scripts.hf.rao_correction verify snapshot flagged
```

Each Parquet table carrying `vessel_id` gains nullable `correction_status` and
`correction_warning` columns. Affected observations carry `UNDER_CORRECTION` and
the exact warning: **under correction: solved from a half-model mesh; do not use**.
Other observations have null correction fields. All original columns, row order,
prior reliability flags and schema metadata are retained. Schema metadata also
records the affected mapping, status and warning under `rao_correction`.

The card receives a correction notice and revision entry. A `rao-correction.json`
manifest binds the complete source and staged inventories by SHA-256 and records
affected counts by entry and table. Parquet bytes change during serialization;
original numerical observations do not. Ancillary files are copied unchanged.
Original and staged directories must differ; existing destinations and source
symlinks refuse. An I/O failure can leave a partial destination; no bundle without
a complete, verified manifest is eligible for review or publication. Retain it for
diagnosis and use a fresh destination for a later attempt.

## Slice 2: full-body replacement

Supply a candidate Parquet table with exactly the original RAO schema, containing
only affected observations. Supply the complete sample key, including all columns
needed to distinguish the source's solver, damping, motion, frequency and heading
series. All original affected keys must occur exactly once in the candidate.
Only the declared `magnitude` and `phase` value columns may differ; all such columns
present in the original must be supplied for replacement. Every sample coordinate
present in the original must occur in the key. Units, identifiers,
keys, prior reliability flags and unrelated fields must match the original.
Null and non-finite replacement values refuse; no samples are interpolated.

The gate report uses this structure; placeholders are required inputs:

```json
{
  "candidate_sha256": "REQUIRED",
  "checks": {
    "buoyancy": {"status": "PASS", "file": "buoyancy.json", "sha256": "REQUIRED"},
    "mesh": {"status": "PASS", "file": "mesh.json", "sha256": "REQUIRED"},
    "solver_agreement": {"status": "PASS", "file": "agreement.json", "sha256": "REQUIRED"},
    "claims_verifier": {"status": "PASS", "file": "claims.json", "sha256": "REQUIRED"}
  }
}
```

Evidence files must be retained beside the report or in its subdirectories.
Each check must report PASS and identify a matching retained file digest. A missing
or failed gate, a changed candidate, missing evidence or a stale evidence digest
refuses replacement. Evidence must identify the candidate run and establish:

- Full-body buoyancy against the governing cited criterion.
- Mesh correctness, including full-body representation and qualification.
- Solver agreement on the governing corrected candidate.
- A claims-verifier review of the proposed replacement and its source evidence.

```bash
python -m scripts.hf.rao_correction correct snapshot flagged \
  --output corrected --candidate candidate.parquet --gates gates.json \
  --keys vessel_id solver damping_zeta_pct dof freq_rad_s heading_deg \
  --values magnitude phase --date 2026-10-21
python -m scripts.hf.rao_correction verify flagged corrected
```

Selected RAO values and correction statuses change; the card records a full-body
RAO replacement. Other tables remain flagged because RAO replacement does not
qualify their values. Prior reliability flags remain until separately reconciled.
The original warning remains in the card's history; the later replacement entry
records the changed RAO status. Retain the original, flagged and corrected bundles,
the candidate, the report and the evidence files as a review chain.

## Slice 3: claims verification before publication

After staging either bundle, run the external claims-verifier on the exact final
artifacts. Record its receipt separately:

```json
{
  "status": "PASS",
  "bundle_sha256": "SHA256_OF_RAO_CORRECTION_MANIFEST",
  "evidence_ref": "retained-external-review-reference"
}
```

```bash
python -m scripts.hf.rao_correction release-check snapshot flagged \
  --claims-report flag-claims.json
python -m scripts.hf.rao_correction release-check flagged corrected \
  --claims-report correction-claims.json
```

The command rechecks every source and output digest, then requires a PASS receipt
bound to the final manifest digest. It reports PASS_RECORDED, which describes the
supplied receipt and does not authenticate its origin. Publication remains
NOT_EVALUATED: maintainers must validate the review, authority and actual upload.
No command in this workflow publishes a dataset. A flag-stage receipt cannot
authorize a different corrected bundle or a later edit.

## Worked example and independent answer

Run with Python and PyArrow available and a new output directory:

```bash
python -m examples.hydrodynamics.rao_correction_example example-output
```

Inputs are three synthetic identifiers, a, b and c, with arbitrary magnitudes
2, 4 and 8. The mapping selects a and b. Counting those selections by hand gives
two affected observations and one unaffected observation. The warning operation
copies values without arithmetic, so the answer remains 2, 4 and 8. A proposed
candidate containing 3 and 5 has no qualifying evidence and is refused:

```text
magnitudes: [2.0, 4.0, 8.0]
statuses: ['UNDER_CORRECTION', 'UNDER_CORRECTION', None]
replacement: NOT_EVALUATED (required gate evidence absent)
publication: NOT_EVALUATED (external claims-verifier required)
```

The test suite additionally substitutes 3 and 5 with explicitly synthetic gate
fixtures and checks the independently prescribed answer 3, 5 and 8. These
software tests establish preservation and refusal behavior, not engineering
accuracy. Native full-body results, identifier reconciliation, external claims
review and dataset publication remain required follow-up work.
