# Material Grade Matrix — Validation Record (2026-06-28)

Backing for `src/digitalmodel/materials/` (issue #1088, EPIC #1080 workstream B).
The matrix stores edition-qualified strengths and separate compatibility
snapshots. API 5L product levels are selected explicitly; an unavailable product
level raises an error. Original-standard qualification is **NOT_EVALUATED**
pending the maintainer's licensed-text check for
[issue 2726](https://github.com/vamseeachanta/digitalmodel/issues/2726).

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 1 | 2026-10-18 | Added the #2726 supersession note to distinguish historical conversions from the current product-level basis. |
| 2 | 2026-10-18 | Reworded the grade designation sentence to pass the engineering register check. |
| 3 | 2026-10-21 | Replaced the superseded PSL interpretation with the current lookup basis and added an executable example to prevent stale answers. |

## Scope

This initial family inventory is historical. The current registry also includes
ASTM A312 austenitic and ASTM A790 duplex pipe grades; `materials.grades` is the
authoritative inventory.

| Standard | Grades | Count |
|---|---|---|
| API 5L / ISO 3183 | A25, A, B, X42, X46, X52, X56, X60, X65, X70, X80, X90, X100, X120 | 14 |
| IACS UR W11 (hull) | Grade A/B/D/E (235) + {A,D,E,F}H 32 (315) / 36 (355) / 40 (390) | 16 |
| EN 10025-2 | S275, S355, S420 | 3 |

Stored in **MPa** (SI); psi properties and back-compat adapters provided.

## Product-level basis and historical conversions

The repository cites API 5L, *Line Pipe*, 46th edition (2018), clause 9.3.1,
Table 6 (PSL1 tensile-test requirements), and clause 9.3.2, Table 7 (PSL2
tensile-test requirements). `materials.grades` stores the SI entries directly;
a ksi conversion is not used to derive those entries. Each available product level
has its own source string, which is retained in `materials.database` records.

The common PSL1/PSL2 grades have equal SI tensile minima in this repository
basis. PSL1 ends at X70; X80 and higher grades have no PSL1 record. A25 and A
have no PSL2 record. Unsupported or unknown API product levels raise
`ValueError` instead of selecting a different product level.

Earlier versions treated converted US-customary tensile snapshots as PSL1
SI minima and mixed truncation with rounding. That interpretation is
superseded. The `legacy_*` adapters preserve historical consumer inputs;
their outputs do not establish a current product-standard requirement.
Consumer migration remains tracked in
[issue 1089](https://github.com/vamseeachanta/digitalmodel/issues/1089).

The public [API publications catalogue (2023)](https://www.api.org/-/media/files/publications/2023_catalog/2023_api_publications_catalog.pdf)
identifies the edition and publication year; it does not establish the table
entries. A separate public [supplier specification](https://uniasen.com/standards/api-5l/),
UNIASEN, *API 5L Pipe Specification*, names the same edition in its
Specification section. Its Mechanical Requirement tables provide the
independent published cross-check for the API strength fixtures and example
(accessed 2026-10-21). That cross-check does not replace an inspection of the
licensed standard. The maintainer's licensed-text check remains required
before issue closure.

## Repository regression anchors

These entries are read from `src/digitalmodel/materials/grades.py`, with its
API 5L (46th edition) / ISO 3183, IACS UR W11 and EN 10025-2 citations.
They describe the repository basis; original-source qualification is pending.

| Grade | SMYS (MPa) | SMTS (MPa) | Note |
|---|---|---|---|
| X52 | 360 | 460 (PSL1 and PSL2) | ISO L360 |
| X65 | 450 | 535 (PSL1 and PSL2) | ISO L450 |
| X80 | 555 | 625 (PSL2) | ISO L555; PSL1 absent |
| X120 | 830 | 915 | PSL2 only |
| Grade A (hull) | 235 | 400 | NS, Charpy +20 C |
| AH36 / EH36 / FH36 | 355 | 490 | letter = toughness only |
| EH40 | 390 | 510 | HS-40 |
| S355 | 355 | 510 | EN, t ≤ 16 mm |

Young's modulus by family: line pipe 207 GPa, hull 206 GPa, EN structural
210 GPa; ν = 0.30, ρ = 7850 kg/m³.

## Worked example: explicit PSL1 lookup

Inputs are grade `X52` (alias `L360`) and product level `PSL1`. The method
selects the cited SI table entry, then compares the direct lookup and the
indexed database. No strength equation or ksi rounding is applied. The
expected answer below is read independently from the supplier's Mechanical
Requirement PSL1 table, row L360/X52, and agrees with the existing grade
source's API 5L, 46th edition (2018), clause 9.3.1/Table 6 citation.
Original-standard qualification remains **NOT_EVALUATED**.

```pycon
>>> from digitalmodel.materials import smts_mpa
>>> from digitalmodel.materials.database import load_database
>>> database = load_database()
>>> record = database.lookup("L360", psl="psl1")
>>> (record.designation, record.product_level, record.strengths.units)
('X52', 'PSL1', 'MPa')
>>> smts_mpa("X52", psl="PSL1")
460.0
>>> record.strengths.smts_mpa
460.0
>>> smts_mpa("X52", psl="PSL2") == record.strengths.smts_mpa
True
>>> record.edition
'46th ed.'
>>> record.provenance.citation
'API 5L (46th ed.), 9.3.1 Table 6 (PSL1), SI'

```

The answer is the stored tensile minimum for the selected product level;
it is not a complete pipe qualification. An unsupported request is rejected:

```pycon
>>> smts_mpa("X80", psl="PSL1")
Traceback (most recent call last):
    ...
ValueError: X80: PSL1 is not listed in API 5L 46th ed. Table 6
>>> database.lookup("X80", psl="PSL1")
Traceback (most recent call last):
    ...
ValueError: X80: PSL1 is not listed in API 5L 46th ed. Table 6

```

`tests/materials/test_psl1_example.py` executes these prompts and checks their
answers. Run it together with the grade, database and audit regression tests:

```sh
PYTHONPATH=src uv run python -m pytest tests/materials
```

## Back-compat verification

The legacy FFS dicts are **regenerated from the matrix** (not copied) and the
test-suite asserts equality against the live imports, which both documents the
convention and guards against drift:

* `legacy_smys_psi_dict()` == `asset_integrity.corroded_pipe.SMYS_PSI`
  — convention: SMYS psi = grade number (ksi) × 1000 (X52 → 52,000 psi).
* `legacy_smts_psi_dict()` == `asset_integrity.dnv_rp_f101.SMTS_PSI`
  — convention: PSL2 SMTS (MPa) × 145.0377, rounded to nearest 100 psi
  (460 MPa → 66,700 psi).

## Tests

`tests/materials/test_grade_matrix.py` covers all API 5L registry grades,
product-level availability, the IACS / EN anchors, compatibility views,
aliases and registry completeness. `tests/materials/test_audit_regressions.py`
covers API product-level citations and rejected requests across the public
strength views. `tests/materials/test_database.py` checks indexed records and
repository provenance. Test results describe regression coverage, not
licensed-text verification.

## Deferred

Migrating the remaining scattered consumers (`pipe_db_client`, `riser_config`,
`pipeline_pressure`, `wall_thickness_parametric`, structural `models.py`, …)
onto this matrix is tracked by issue #1089. Some of those carry US-customary or
PSL1 roundings; #1089 will surface and resolve each value delta on migration.
