# Engineering Codes Register

The single index of every engineering standard implemented in digitalmodel:
standard → edition → module(s) → tests. The reference strings are defined once in
`src/digitalmodel/codes.py` (`CodeReference` constants + `REGISTER`), which the
strength/FFS results carry as `code_reference` (issue #1092). This register is
kept in sync with `codes.REGISTER` by
`tests/asset_integrity/test_codes_register.py`.

> Kept current: when you add a `CodeReference` to `codes.py`, add its row here —
> the consistency test fails otherwise.

## Remaining-strength / fitness-for-service

| Standard | Edition | Module(s) | Tests |
|---|---|---|---|
| ASME B31G | 2012 | `asset_integrity/corroded_pipe.py` | `test_corroded_pipe`, `test_code_provenance` |
| DNV-RP-F101 | January 2015 | `asset_integrity/dnv_rp_f101.py` | `test_dnv_rp_f101` |
| API 579-1/ASME FFS-1 | 2021 | `asset_integrity/assessment/*`, `structural_analysis/plate_metal_loss_ffs.py` | `test_ffs_*`, `test_code_provenance` |
| BS 7910 | 2019 | `asset_integrity/common/fad.py` | `test_ffs_assessment` |

## Buckling / plated structures

| Standard | Edition | Module(s) | Tests |
|---|---|---|---|
| DNV-RP-C201 | 2010 | `structural_analysis/buckling.py`, `panel_buckling.py`, `girder_web_frame.py`, `plate_metal_loss_ffs.py` | `test_panel_buckling_parametric`, `test_girder_web_frame`, `test_code_provenance` |
| EN 1993-1-1 (Eurocode 3) | 2005 | `structural_analysis/capacity.py`, `buckling.py` (column) | `test_code_provenance` |
| DNV CN 30.1 | — | `structural_analysis/girder_web_frame.py` | `test_girder_web_frame` |

## Pipe / riser wall thickness & pressure containment

| Standard | Edition | Module(s) | Tests |
|---|---|---|---|
| API RP 1111 | 2015 | `structural/analysis/wall_thickness_codes/api_rp_1111.py` | `test_wall_thickness_codes` |
| API RP 2RD | 2013 | `.../wall_thickness_codes/api_rp_2rd.py` | `test_wall_thickness_codes` |
| API STD 2RD | 2013 | `.../wall_thickness_codes/api_std_2rd.py` | `test_wall_thickness_codes` |
| DNV-ST-F201 | 2021 | `.../wall_thickness_codes/dnv_st_f201.py` | `test_wall_thickness_codes` |
| DNV-ST-F101 | 2021 | `.../wall_thickness_codes/dnv_st_f101.py`, `subsea/pipeline/pipeline_pressure.py` | `test_wall_thickness_codes`, `test_wall_thickness_collapse_solver` |
| PD 8010-2 | 2015 | `.../wall_thickness_codes/pd_8010_2.py` | `test_wall_thickness_codes` |
| ISO 13623 | 2017 | `.../wall_thickness_codes/iso_13623.py` | `test_wall_thickness_codes` |
| ASME B31.4 | 2019 | `.../wall_thickness_codes/asme_b31_4.py` | `test_wall_thickness_codes` |
| ASME B31.8 | 2022 | `.../wall_thickness_codes/asme_b31_8.py`, `asset_integrity/assessment/level1_screener.py` | `test_wall_thickness_codes`, `test_pipeline_ffs_assessment` |

### Riser label evidence and unresolved editions

The publisher catalogue opened on 2026-10-12 carries the STD entry,
not the RP entry. Its exact metadata wording is "Std 2RD",
"Dynamic Risers for Floating Production Systems" and
"2nd Edition | September 2013| Reaffirmed: September 2020".
[Publisher catalogue](https://www.api.org/-/media/files/publications/2024-catalog/2024-publication-catalog.pdf).
This establishes publication metadata only, not implemented equations.

| Document | Field | Value retained from main | Publisher response and retrieval time (UTC) | Disposition |
|---|---|---|---|---|
| API RP 2RD | Title | Dynamic Risers for FPS | [Catalogue](https://www.api.org/-/media/files/publications/2024-catalog/2024-publication-catalog.pdf), HTTP 200, 2026-10-12 21:10:46; no RP entry | not verified: source did not open; restored unchanged from main |
| API RP 2RD | Edition | 2013 | Same catalogue response; no RP entry | not verified: source did not open; restored unchanged from main |
| API RP 2RD | Title and edition | Same main values | [Earlier catalogue address](https://www.api.org/-/media/files/publications/2010-catalog/2010-publications-catalog.pdf), HTTP 200, 2026-10-12 21:11:24; 404 page without entry; [store address](https://www.apiwebstore.org/standards/2rd), HTTP 200, 2026-10-12 21:11:25; entry requires JavaScript | not verified: source did not open |
| DNV-RP-C203 | VIV reference year | 2016 | [Publisher page](https://www.dnv.com/energy/standards-guidelines/dnv-rp-c203-fatigue-design-of-offshore-steel-structures/), HTTP 403, 2026-10-12 21:11:52 | not verified: source did not open; main reference line restored unchanged |

Table: Publisher retrieval evidence; successful HTTP responses without the required entry do not verify a label.

The separate strategies identify legacy WSD in
`src/digitalmodel/structural/analysis/wall_thickness_codes/api_rp_2rd.py:24`
and STD LRFD in `src/digitalmodel/structural/analysis/wall_thickness_codes/api_std_2rd.py:24`.
`tests/test_wall_thickness_api_rp_2rd.py:59` checks the legacy allowable factor,
but does not establish an edition. The RP edition field retains main's value;
that value is not verified against a publisher source or the implementation.
Edition uncertainty is recorded here, outside caller-visible labels.
Its lookup key remains unchanged.

| Location | Stated label / edition | Disposition |
|---|---|---|
| `src/digitalmodel/codes.py` / RP row above | RP 2RD; 2013 | Restored unchanged from main; title and edition not verified; STD row unchanged |
| `src/digitalmodel/parametric/refresh.py:34` | API-RP-2RD; 2013 | Left: fingerprint input and identifier; generator is `src/digitalmodel/parametric/refresh.py` |
| `docs/api/structural/wall-thickness-explorer.json:59` | API RP 2RD (2013) | Unchanged from main; generator is `scripts/capabilities/build_wall_thickness_explorer.py` |
| `docs/api/structural/wall-thickness-explorer.html:76` | API RP 2RD (2013) | Unchanged from main; same generator and embedded dataset |
| `tests/fixtures/test_vectors/structural/wall_thickness.yaml:197` | API RP 2RD:2013 | Left: extracted fixture; generator not identified in the worktree |
| `src/digitalmodel/structural/analysis/wall_thickness_codes/dnv_st_f201.py:24` | ST-F201; 2021 | Retained; implemented edition not established |
| `src/digitalmodel/codes.py` / ST-F201 row above | ST-F201; 2021 | Retained; same unresolved strategy |
| `src/digitalmodel/structural/analysis/wall_thickness_codes/manifest.yaml:33` | ST-F201; 2021 | Retained; same unresolved strategy |
| `src/digitalmodel/structural/fatigue/worked_examples.py:320` | DNVGL-ST-F201; 2018 | Retained; fatigue-factor edition not established |
| `tests/fixtures/test_vectors/fatigue/worked_examples.yaml:159` | DNVGL-ST-F201; 2018 | Retained; extracted worked-example citation |
| `docs/api/structural/wall-thickness-explorer.json:57` and HTML dataset | ST-F201; 2021 | Unchanged from main; same explorer generator |

Table: Riser label inventory and disposition; locators refer to the named files.

**Open for the project manager.** A reviewer holding the applicable documents
will need to compare the RP strategy's burst, collapse and hoop checks and
allowable factors against the earlier RP edition. The ST-F201 pressure
containment equation, material-strength reduction, material, dynamic and
safety-class factors in `dnv_st_f201.py:28-65` and their hand-calculation test
in `tests/test_wall_thickness_codes/test_dnv_st_f201.py:59` need an
edition-specific comparison. The fatigue factor and safety-class applicability
cited at clause 5.4.4 in `worked_examples.py:282-320` also need comparison.
The direct DNV catalogue request returned HTTP 403; published edition and
amendment months are not established by this assessment. No ST-F201 label is
upgraded to an edition/amendment pair, and no engineering value is changed.

The atlas implementation itself cites STD 2RD, second edition 2013, in
`src/digitalmodel/orcaflex/code_check_engine.py:13`; its historical RP class
names remain compatibility identifiers. Resolving the differing provenance in
`parametric/refresh.py` requires a separate fingerprint-aware regeneration.
The same code-check engine cites OS-F201 (2010) at line 15, not ST-F201;
that earlier correction is already delivered by
[the prior citation change](https://github.com/vamseeachanta/digitalmodel/pull/2164).

## Tubular products

| Standard | Edition | Module(s) | Tests |
|---|---|---|---|
| API 5L / ISO 3183 | 2018 | `materials/grades.py`, `materials/line_pipe.py` | `test_grade_matrix`, `test_line_pipe` |
| API 5CT | 2018 | `well/tubulars/casing.py` | `test_casing` |
| API 5C3 / API TR 5C3 | 2018 | `well/tubulars/casing.py`, `asset_integrity/custom/PipeCapacity.py` | `test_casing` |
| API 11B | 2018 | `well/tubulars/sucker_rod.py` | `test_sucker_rod` |

## Materials (structural / marine)

| Standard | Edition | Module(s) | Tests |
|---|---|---|---|
| IACS UR W11 | — | `materials/grades.py`, `structural_analysis/models.py` | `test_grade_matrix` |
| EN 10025-2 | 2019 | `materials/grades.py`, `structural_analysis/models.py` | `test_grade_matrix` |

## Ship longitudinal strength & scantlings

| Standard | Edition | Module(s) | Tests |
|---|---|---|---|
| IACS UR S11 | 2020 | `naval_architecture/hull_girder_strength.py` | `test_hull_girder_strength` |
| IACS UR S11A | 2021 | `naval_architecture/hull_girder_strength.py` | `test_hull_girder_strength` |
| DNV-RU-SHIP | 2021 | `naval_architecture/scantlings.py`, `compliance.py` | `test_scantlings` |
| IACS CSR | 2022 | `naval_architecture/scantlings.py` | `test_scantlings` |
