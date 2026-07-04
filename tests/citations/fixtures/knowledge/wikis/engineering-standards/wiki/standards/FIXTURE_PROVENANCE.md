# Fixture Provenance - `dnv-rp-f103.md`

This directory contains a minimal vendored wiki fixture for DNV-RP-F103.
It exists so digitalmodel citation tests can validate fail-closed frontmatter
resolution in standalone CI without requiring access to the private llm-wiki
checkout.

The fixture intentionally includes only resolver frontmatter and a short
description. It does not vendor standard text, tables, formulas, or licensed
source material.

## Canonical source

- **Repo:** `vamseeachanta/llm-wiki`
- **Path:** `wikis/engineering-standards/wiki/standards/dnv-rp-f103.md`
- **Canonical SHA at vendoring time:** `76303d8be41969d7c5bb66171ce2c20a99bd39a8`

## Vendored copy

- **Path in this repo:** `tests/citations/fixtures/knowledge/wikis/engineering-standards/wiki/standards/dnv-rp-f103.md`
- **Vendored on:** 2026-06-11
- **Used by:** `tests/citations/test_registry.py` and `tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py`

## Freshness contract

Review monthly. If the canonical page frontmatter changes, update the fixture,
the citation expectations, and this provenance file in the same commit, then
rerun:

```bash
PYTHONPATH=src uv run pytest tests/citations/test_registry.py tests/marine_ops/marine_engineering/test_cathodic_protection_dnv.py -q
```

---

# Fixture Provenance - `dnv-os-f201.md` and `dnv-rp-c203.md` (#1245)

Vendored for the riser-database citation getters (`get_riser_dff`,
`get_riser_scf`). Same contract as the F103 fixture above: resolver
frontmatter and a short description only — no standard text, tables,
formulas, or licensed source material.

## Canonical sources

- **Repo:** `vamseeachanta/llm-wiki`
- `wikis/engineering-standards/wiki/standards/dnv-os-f201.md` —
  canonical SHA at vendoring time: `acdbc9f842b59340d384392941fa0c3177af90b2`
- `wikis/engineering-standards/wiki/standards/dnv-rp-c203.md` —
  canonical SHA at vendoring time: `23f3251def0dac360eb6b6b80e585fbcb1608efe`

## Vendored copies

- **Vendored on:** 2026-07-01
- **Used by:** `tests/riser_database/test_citations.py`,
  `tests/riser_database/test_material_refs.py`

## Freshness contract

Review monthly. If a canonical page's frontmatter changes, update the fixture,
the getter templates in `src/digitalmodel/riser_database/getters.py`, the
`registry_revision` column in `data/riser_database/standards_crosswalk.csv`,
and this provenance file in the same commit, then rerun:

```bash
.venv/bin/python -m pytest tests/riser_database/ -q
```

---

# Fixture Provenance - `api-rp-16q.md` (#1246)

Vendored for the riser-database 16Q tension-factor getters
(`get_tension_weight_factor`, `get_buoyancy_tension_factor`). Same contract as
the fixtures above: resolver frontmatter and a short description only — no
standard text, tables, formulas, or licensed source material.

## Canonical source

- **Repo:** `vamseeachanta/llm-wiki`
- `wikis/engineering-standards/wiki/standards/api-rp-16q.md` —
  canonical SHA at vendoring time: `2252d2c3f9c69b11a70ccce011e33d9f306bb63e`
  (the revision "1993" pin landed by llm-wiki PR #820; before that the page
  carried the `public-metadata-required-before-citation-use` sentinel).

## Vendored copy

- **Vendored on:** 2026-07-02
- **Used by:** `tests/riser_database/test_citations.py`

## Freshness contract

Review monthly. If the canonical page frontmatter changes, update the fixture,
the `_API_RP_16Q_CITATION_TEMPLATE` in
`src/digitalmodel/riser_database/getters.py`, and this provenance file in the
same commit, then rerun:

```bash
.venv/bin/python -m pytest tests/riser_database/ -q
```

---

# Fixture Provenance - `api-std-2rd.md` (#1281a)

Vendored for the operating-envelope von Mises design-factor getter
(`get_von_mises_design_factor`). Same contract as the fixtures above: resolver
frontmatter and a short description only — no standard text, tables, formulas,
or licensed source material.

## Canonical source

- **Repo:** `vamseeachanta/llm-wiki`
- `wikis/engineering-standards/wiki/standards/api-std-2rd.md` —
  canonical SHA at vendoring time: `74aeb6f3e47806c377fc614b152d80c31b61bb5e`
  (revision `3e-2025`; the page was already citation-ready — no paired wiki PR
  was needed for #1281a).

## Vendored copy

- **Vendored on:** 2026-07-03
- **Used by:** `tests/riser_database/test_citations.py`,
  `tests/drilling_riser/test_envelope.py`

## Freshness contract

Review monthly. If the canonical page frontmatter changes, update the fixture,
the `_API_STD_2RD_CITATION_TEMPLATE` in
`src/digitalmodel/riser_database/getters.py`, and this provenance file in the
same commit, then rerun:

```bash
.venv/bin/python -m pytest tests/riser_database/ tests/drilling_riser/ -q
```
