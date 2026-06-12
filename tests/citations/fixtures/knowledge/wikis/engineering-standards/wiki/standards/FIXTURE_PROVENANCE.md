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
