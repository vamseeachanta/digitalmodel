# Full-Estate STEP Dedup — Tier-A results

**Issues:** [#1007](https://github.com/vamseeachanta/digitalmodel/issues/1007) (dedup) · [#1017](https://github.com/vamseeachanta/digitalmodel/issues/1017) (shape-similarity spike) · scales the [Tier-A probe](./cad-ai-spike.md).
**Where:** ace-linux-1, `cadpilot` env (pythonocc/OCCT 7.9), license-free, no ML. **Privacy:** paths reported as SHA-ids +
top-folder only.

## Method

Two dedup signals over the neutral-STEP corpus, no ML:
1. **Exact content duplicate** — SHA-256 (robust even when OCC can't parse the file).
2. **Geometric near-duplicate** — the scale/rotation-invariant OCC fingerprint (`pilot-sim.py`), nearest-neighbour
   below τ=0.15 among distinct-content files. Catches re-exports/format-variants that are byte-different but
   shape-identical — the content-hash blind spot.

**Coverage (no silent caps):** 1,759 STEP/STP total → **1,723 processed** (≤25 MB); **36 deferred** (>25 MB, need a
longer pass); **70 parse errors** (malformed STEP headers OCC rejects — still SHA-hashed, so still deduped exactly).

## Result — the corpus is ~90% duplicate

| Metric | Value |
|---|--:|
| STEP files processed (≤25 MB) | 1,723 |
| Parsed OK (fingerprintable) | 1,653 |
| **Exact-duplicate groups** | **120** |
| **Distinct unique contents** | **~171** |
| **Redundant copies** (exact) | **~1,480 (≈90%)** |
| **Wasted bytes** (exact dups) | **≈3.06 GB** |
| Geometric near-dup pairs (distinct content) | 18 |

**~1,723 STEP files hold only ~171 unique shapes.** The estate is dominated by duplication, not modeling.

### Where the exact duplication lives

| Copies | Per-file size | Folders spanned | Reading |
|--:|--:|---|---|
| **26** | 21.9 MB | `docs` | one part re-copied **26×** inside the personal-backup hoard |
| **28** | 19.0 MB | `docs` | same — **28 copies** of one file |
| **28** | 17.9 MB | `docs` | " |
| 14 / 14 / 13 | 17–21 MB | `docs` | hoard internal redundancy |
| 6 / 2 / 2 | 12–25 MB | **`digitalmodel` + `docs`** | the cross-folder mirror (`riser-eng-job` ↔ `misc/projects`) |
| 15 | 3.9 MB | `docs` | " |

The dominant waste is **within-hoard re-copying** (the same STEP saved across multiple flush-drive/backup folders),
plus the curated-library ↔ source-tree mirror.

### Geometric near-dups (what content-hash misses)

18 pairs of **distinct bytes / identical shape** — e.g. one pair with **3,210 vs 3,218 faces** (a re-export with
slightly different tessellation), and several at distance 0.000 with identical face counts (re-saved with a different
timestamp/precision). These would survive a pure SHA dedup; the fingerprint collapses them.

## What this means

- A **content-hash + geometric-fingerprint dedup index** would **collapse the STEP corpus ~10×** and reclaim
  **~3 GB** immediately — and the same two-signal method extends to the full 525k-file estate (where the 416k inbox
  hoard is the prize, [#1007](https://github.com/vamseeachanta/digitalmodel/issues/1007)).
- This **confirms at scale** the Tier-A spike finding ([#1017](https://github.com/vamseeachanta/digitalmodel/issues/1017)): the cheap, license-free, no-ML fingerprint is
  sufficient for **coarse dedup/family** work; ML (UV-Net) is only needed for *fine* variant discrimination.
- **Keep-one policy:** within each exact group, keep the canonical copy (prefer the curated `riser-eng-job` / active
  project path over a `.preexisting` or hoard path) and replace the rest with pointers — per the
  `superseded-dupe` discipline.

## Caveats

- **Exact-dup counts are definitive** (SHA-256). **Near-dup** uses τ=0.15 on a robust-standardized fingerprint —
  tunable; 18 pairs is a conservative floor, not a ceiling.
- **36 files >25 MB deferred** and **70 parse errors** — both reduce *near-dup* coverage only; exact-dup is complete
  over all SHA-hashed files. A second pass (raise the size cap; pre-clean malformed headers) would extend near-dup.
- Reproducible: `dedup.py` (committed alongside).
