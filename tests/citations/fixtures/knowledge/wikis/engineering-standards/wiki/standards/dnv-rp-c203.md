---
title: "DNV-RP-C203 Fatigue Design of Offshore Steel Structures — citation fixture"
code_id: dnv-rp-c203
publisher: DNV
revision: "2024-10"
tags: ["standard", "dnv", "fatigue", "fixture"]
---

# DNV-RP-C203 — Fatigue Design of Offshore Steel Structures (vendored citation fixture)

Minimal fixture so riser citation tests can validate frontmatter resolution
standalone (see FIXTURE_PROVENANCE.md). Resolver frontmatter only — no
standard text, tables, formulas, or licensed source material.

DNV-RP-C203 is the S-N fatigue methodology source referenced by
`digitalmodel.riser_database.getters.get_riser_scf` and the
`material_sn_scf_dff` reference table. Note: `digitalmodel.fatigue.sn_curves`
implements the 2021 edition tables while the canonical wiki page tracks the
2024-10 revision — the drift is recorded in the riser database crosswalk.
