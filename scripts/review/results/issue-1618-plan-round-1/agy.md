# Issue 1618 plan review — Agy — round 1

Verdict: **MAJOR**

- The plan referenced `tests/marine_ops/vessel_db/test_wed_adapter.py` without declaring it in the artifact map.
- `hull_candidates` had no owned provider or catalog-evidence interface.
- YAML-to-JSON canonicalization lacked a strict typed boundary and could lose YAML type semantics.
- The PR #1627 gate lacked an explicit repository/link.
- Canonical coordinate axes/origin semantics were underspecified.
- The conditional crosswalk timing was ambiguous relative to the #1050 gate.

Disposition: the plan remains draft. The revision removes undeclared adapter-test work, adds an existing-catalog evidence provider, rejects YAML type coercion before canonicalization, links the digitalmodel PR, and freezes canonical axes plus an explicit origin datum.
