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
| DNV-RP-F101 | 2021 | `asset_integrity/dnv_rp_f101.py` | `test_dnv_rp_f101` |
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
| ASME B31.8 | 2020 | `.../wall_thickness_codes/asme_b31_8.py` | `test_wall_thickness_codes` |

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
