# Riser Database (#1245 / #1199a)

First slice of the riser domain database (parent #1199, exemplar of the
data-source flywheel epic llm-wiki#799). This slice ships the tabular
foundation: a fail-closed loader, three public reference tables, two
citation-emitting getters, and the leak gate.

## Tables (`data/riser_database/`)

Generated deterministically by `scripts/riser_database/build_tables.py` from
the externalized seed spec `scripts/riser_database/sources.yml` — regenerate
and `git diff --exit-code data/riser_database` to verify the committed tables
match the seeds. `manifest.yaml` carries per-table sha256 fingerprints; the
loader refuses a table whose fingerprint drifted.

| Table | Route | Content |
|---|---|---|
| `config_catalog` | public | Riser configuration envelopes seeded from `examples/riser_input/*.yml` (5) + `config/pipe/*riser*.yml` (22, explicit list). Depth/position for the pipe configs derive from the FILENAME (`<n>ft` → m × 0.3048); OD/WT are inches → mm × 25.4. A missing token yields an empty cell — never guessed. |
| `standards_crosswalk` | public | Clause **identifiers** + `wiki_path` only: `api-std-2rd` (there is no `api-rp-2rd` — API reclassified RP 2RD as STD 2RD), `api-rp-16q`, `dnv-os-f201`, `dnv-rp-c203`. `registry_revision` mirrors llm-wiki `data/standards-registry.json` verbatim. |
| `material_sn_scf_dff` | public | References into `fatigue/sn_curves.py` vocabulary (plain detail-category letters + environment — the REAL riser fatigue consumer, used by `riser_fatigue/workflow.py` and `touchdown.py`; NOT the composite ids of `sn_library_api`). `scf_ref`/`dff_ref` name getters. |

**Licensing rule:** no DNV/API clause text, table content, or S-N parameter
values in this public repo. Values come through `CitedValue` getters resolving
against the **private** llm-wiki (`.claude/rules/codes-standards-data-routing.md`
in workspace-hub). The schema tests enforce exact column allowlists plus
content-shape tripwires (clause-locator regex for `section`; length and
numeric-token-density caps on free text) so values cannot ride free-text
columns either.

## Loader

```python
from digitalmodel.riser_database import RiserDatabase

db = RiserDatabase.load()          # fail-closed: raises RiserDatabaseError
row = db.config("riser_scr")       # unknown key raises — never fuzzy-matches
```

Bounded reads (known filenames, 5 MB cap, `yaml.safe_load`), fingerprint
verification against `manifest.yaml`, exact column checks, duplicate-key
rejection. Escalation mirrors `parametric.atlas.Atlas.predict`: unknown key →
raise with reason.

## Citation getters

```python
from digitalmodel.riser_database import get_riser_dff, get_riser_scf, riser_citations

get_riser_dff().value              # 10.0 — DNV-OS-F201 (allowable damage = 1/DFF)
get_riser_scf().value              # 1.0  — DNV-RP-C203 methodology (neutral default)
get_riser_dff(override=3.0)        # user override wins; note records both values
riser_citations()                  # consumer helper: {} + one-shot RuntimeWarning standalone
```

Direct getter calls are **fail-closed** (`CitationResolutionError` carrying the
`code_id`); only the consumer helper degrades in standalone/`pip install` mode
(one-shot `RuntimeWarning`) — same contract as the mooring pilot. Values match
today's riser literals (`touchdown.py` `dff=10.0`, `scf=1.0`) so #1246 wiring
inherits day-one parity.

Getter provenance discipline: a getter only cites a standard the code actually
sources the value from. Hence this slice has **no** API RP 16Q getters (the
wiki page's revision frontmatter is a not-citation-ready sentinel — fix it in
llm-wiki first; deferred to #1246) and **no** Barlow-SF citation
(`stackup.py` gives it no standards attribution; it is a project default).

## Leak gate (suite 6 — hard merge gate)

`tests/riser_database/test_leak_gate.py`:

* **Layer A** (always runs, public CI included): regex structural checks —
  absolute paths, e-mail addresses, URLs — over every loaded public row and
  the raw data files. Patterns are generic and public-safe.
* **Layer B** (in-context): every public row matched against the PRIVATE
  workspace-hub `.legal-deny-list.yaml` with case/separator normalization.
  Skips loudly when workspace-hub is absent.

**Merge-gate procedure:** Layer B must run GREEN in a workspace-hub checkout
**before the branch is first pushed** (exposure happens at push, not merge).
Additionally run `workspace-hub/scripts/legal/legal-sanity-scan.sh
--repo=digitalmodel --diff-only` **while the changes are still uncommitted**
(the flag diffs the working tree against HEAD; at PR time it is vacuous) and
attach the output to the PR as evidence.

## Extension map

* **#1246** — wire `drilling_riser/stackup.py` + `riser_fatigue` dff to the
  getters (user-override-wins; day-one parity), add the 16Q getters after the
  llm-wiki frontmatter fix.
* **#1247** — orcaflex riser citation surface (net-new; no parity baseline).
* **#1199d** (T3, frontier-gated) — de-identified ACE result precedent,
  `private_sidecar` route; extends the leak gate to those rows.
* Future numeric response-surface atlases (metocean, VIV) live under
  `atlases/riser_database*` per `parametric.atlas` conventions — deliberately
  NOT used for these relational tables (binary Parquet is invisible to
  text-based leak scanning).
