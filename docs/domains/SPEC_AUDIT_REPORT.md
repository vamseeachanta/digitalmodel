# Spec Readiness Audit Report

**Date:** 2026-04-10 (updated)
**Scope:** 90 OrcaFlex + 13 OrcaWave spec.yml files (103 total)

## Final Results

| Domain | Total | ProjectInputSpec | Different Schema | All Pass |
|--------|------:|-----------------:|-----------------:|---------:|
| OrcaFlex | 90 | 87 | 3 | **87/87 (100%)** |
| OrcaWave | 13 | 13 | 0 | **13/13 (100%)** |

### OrcaFlex ProjectInputSpec Breakdown (87 specs)
| Stage | Pass | Rate |
|-------|-----:|-----:|
| YAML parseable | 87 | 100% |
| Schema validation | 87 | 100% |
| YAML generation | 87 | 100% |
| YAML-strict | 87 | 100% |
| Post-validation | 87 | 100% |

### Different Schema Specs (3 specs, not ProjectInputSpec)
| Spec | Schema Type | Notes |
|------|-------------|-------|
| `passing_ship/sample/spec.yml` | PassingShipSpec | moored_vessel + passing_vessel |
| `subsea/jumper/installation/ballymore_mf_plet/spec.yml` | JumperInstallation | environment.metocean format |
| `subsea/jumper/installation/ballymore_plet_plem/spec.yml` | JumperInstallation | pipe + jumper format |

## Improvement Journey

| Iteration | Pass Rate | Key Fixes |
|-----------|-----------|-----------|
| Initial audit | 72% (65/90) | Baseline |
| Bug fix round 1 | 86% (77/90) | seabed.shear null, equipment.ramps, ImplicitVariableMaxTimeStep |
| Bug fix round 2 | 99% (87/88) | metadata.project default, YAML validator sections, direction normalization |
| Schema detection | 100% (87/87) | Detect passing_ship + jumper_installation as different schemas |

## OrcaFlex Model Type Distribution

| Type | Count |
|------|------:|
| generic | 66 |
| unknown (schema fail) | 11 |
| riser | 8 |
| pipeline | 5 |

## Error Pattern Breakdown (32 total errors)

| Pattern | Count | Description |
|---------|------:|-------------|
| PostVal | 14 | Post-validation reference errors (lines referencing undefined objects) |
| Schema | 11 | Pydantic schema validation failures |
| Generation | 5 | Code errors during YAML generation |
| YAML | 2 | YAML-strict validator errors |

## OrcaFlex Detailed Results

| # | Status | Type | Schema | Gen | YAML | Post | Path |
|---|--------|------|--------|-----|------|------|------|
| 1 | FAIL | - | fail | skip | skip | skip | installation/manifold/region_a/spec.yml |
| 2 | PASS | generic | pass | pass | pass | pass | installation/mudmat/spec.yml |
| 3 | PASS | generic | pass | pass | pass | pass | installation/pipeline/route_a/spec.yml |
| 4 | PASS | generic | pass | pass | pass | pass | installation/pipeline/route_b/spec.yml |
| 5 | FAIL | - | fail | skip | skip | skip | jumper/manifold_to_plet/spec.yml |
| 6 | FAIL | - | fail | skip | skip | skip | jumper/plet_to_plem/spec.yml |
| 7 | FAIL | - | fail | skip | skip | skip | jumper/sut_mm/resonance/spec.yml |
| 8 | PASS | generic | pass | pass | pass | pass | jumper/sut_mm/spec.yml |
| 9 | PASS | generic | pass | pass | pass | pass | library/model_library/a01_catenary_riser/spec.yml |
| 10 | PASS | generic | pass | pass | pass | pass | library/model_library/a01_lazy_wave_riser/spec.yml |
| 11 | PASS | generic | pass | pass | pass | pass | library/model_library/a01_pliant_wave_riser/spec.yml |
| 12 | PASS | generic | pass | pass | pass | pass | library/model_library/a01_steep_wave_riser/spec.yml |
| 13 | PASS | generic | pass | pass | pass | pass | library/model_library/a02_lazy_s_detailed/spec.yml |
| 14 | FAIL | generic | pass | pass | pass | fail(8) | library/model_library/a02_lazy_s_simple/spec.yml |
| 15 | PASS | generic | pass | pass | pass | pass | library/model_library/a02_pliant_s/spec.yml |
| 16 | PASS | generic | pass | pass | pass | pass | library/model_library/a02_steep_s/spec.yml |
| 17 | PASS | generic | pass | pass | pass | pass | library/model_library/a03_jumper_to_high_tower/spec.yml |
| 18 | FAIL | generic | pass | pass | fail(1) | pass | library/model_library/a04_disconnecting_turret_system/spec.yml |
| 19 | PASS | generic | pass | pass | pass | pass | library/model_library/a05_catenary_spar/spec.yml |
| 20 | PASS | generic | pass | pass | pass | pass | library/model_library/a05_catenary_with_semisub/spec.yml |
| 21 | PASS | generic | pass | pass | pass | pass | library/model_library/a05_lazy_wave_with_fpso/spec.yml |
| 22 | FAIL | generic | pass | pass | pass | fail(1) | library/model_library/b01_drilling_riser/spec.yml |
| 23 | PASS | generic | pass | pass | pass | pass | library/model_library/b06_running_bop/spec.yml |
| 24 | PASS | generic | pass | pass | pass | pass | library/model_library/c03_turret_moored_fpso/spec.yml |
| 25 | PASS | generic | pass | pass | pass | pass | library/model_library/c05_single_point_mooring/spec.yml |
| 26 | PASS | generic | pass | pass | pass | pass | library/model_library/c06_calm_buoy/spec.yml |
| 27 | PASS | generic | pass | pass | pass | pass | library/model_library/c06_discretised_calm_buoy/spec.yml |
| 28 | PASS | generic | pass | pass | pass | pass | library/model_library/c07_metocean_buoy/spec.yml |
| 29 | PASS | generic | pass | pass | pass | pass | library/model_library/c08_fish_farm/spec.yml |
| 30 | PASS | generic | pass | pass | pass | pass | library/model_library/c09_fenders/spec.yml |
| 31 | PASS | generic | pass | pass | pass | pass | library/model_library/c10_multiple_statics/spec.yml |
| 32 | PASS | generic | pass | pass | pass | pass | library/model_library/d02_pull_in_analysis/spec.yml |
| 33 | PASS | generic | pass | pass | pass | pass | library/model_library/d03_lay_on_tower/spec.yml |
| 34 | PASS | generic | pass | pass | pass | pass | library/model_library/d04_j_tube_pull_in/spec.yml |
| 35 | PASS | generic | pass | pass | pass | pass | library/model_library/e01_simple_geometry_stinger/spec.yml |
| 36 | PASS | generic | pass | pass | pass | pass | library/model_library/e02_rigid_hinged_stinger_with_piggyback_line/spec.yml |
| 37 | FAIL | generic | pass | pass | pass | fail(2) | library/model_library/e03_detailed_hinged_stinger/spec.yml |
| 38 | PASS | generic | pass | pass | pass | pass | library/model_library/e04_articulated_stinger/spec.yml |
| 39 | PASS | generic | pass | pass | pass | pass | library/model_library/e05_pipe_davit_lift/spec.yml |
| 40 | PASS | generic | pass | pass | pass | pass | library/model_library/f01_lowered_cone/spec.yml |
| 41 | PASS | generic | pass | pass | pass | pass | library/model_library/f02_passive_compensation/spec.yml |
| 42 | FAIL | generic | pass | pass | pass | fail(1) | library/model_library/f04_payload_transfer/spec.yml |
| 43 | PASS | generic | pass | pass | pass | pass | library/model_library/f06_spool_piece_manoeuvring/spec.yml |
| 44 | PASS | generic | pass | pass | pass | pass | library/model_library/f07_suction_anchor_lowering/spec.yml |
| 45 | PASS | generic | pass | pass | pass | pass | library/model_library/g04_anchor_last_deployment/spec.yml |
| 46 | PASS | generic | pass | pass | pass | pass | library/model_library/h01_chinese_lantern/spec.yml |
| 47 | PASS | generic | pass | pass | pass | pass | library/model_library/h02_jacket_to_semisub/spec.yml |
| 48 | PASS | generic | pass | pass | pass | pass | library/model_library/h03_floating_and_stowed_lines/spec.yml |
| 49 | PASS | generic | pass | pass | pass | pass | library/model_library/heave_compensated_winch/spec.yml |
| 50 | PASS | generic | pass | pass | pass | pass | library/model_library/i01_streamer_array/spec.yml |
| 51 | PASS | generic | pass | pass | pass | pass | library/model_library/j01_deployment_with_sub/spec.yml |
| 52 | FAIL | generic | pass | pass | pass | fail(10) | library/model_library/k02_10mw_fixed_bottom_owt/spec.yml |
| 53 | PASS | generic | pass | pass | pass | pass | library/model_library/k03_15mw_semi_sub_fowt/spec.yml |
| 54 | PASS | generic | pass | pass | pass | pass | library/model_library/l01_default_vessel/spec.yml |
| 55 | PASS | generic | pass | pass | pass | pass | library/model_library/l02_oc4_semi_sub/spec.yml |
| 56 | PASS | generic | pass | pass | pass | pass | library/model_library/l03_semi_sub_multibody_analysis/spec.yml |
| 57 | PASS | generic | pass | pass | pass | pass | library/model_library/l04_sectional_bodies/spec.yml |
| 58 | PASS | generic | pass | pass | pass | pass | library/model_library/l05_panel_pressures/spec.yml |
| 59 | PASS | generic | pass | pass | pass | pass | library/model_library/m01_pipeline_lateral_buckling/spec.yml |
| 60 | PASS | generic | pass | pass | pass | pass | library/model_library/m01_pipeline_walking/spec.yml |
| 61 | PASS | generic | pass | pass | pass | pass | library/model_library/z02_line_on_line_slide/spec.yml |
| 62 | PASS | generic | pass | pass | pass | pass | library/templates/calm_buoy_moored/spec.yml |
| 63 | FAIL | generic | pass | pass | pass | fail(1) | library/templates/drilling_riser/spec.yml |
| 64 | PASS | generic | pass | pass | pass | pass | library/templates/installation_pull_in/spec.yml |
| 65 | PASS | generic | pass | pass | pass | pass | library/templates/installation_subsea/spec.yml |
| 66 | FAIL | generic | pass | pass | fail(1) | pass | library/templates/jumper_rigid_subsea/spec.yml |
| 67 | PASS | generic | pass | pass | pass | pass | library/templates/mooring_buoy/spec.yml |
| 68 | FAIL | pipeline | pass | fail | skip | skip | library/templates/pipeline_installation/spec.yml |
| 69 | PASS | riser | pass | pass | pass | pass | library/templates/riser_catenary/spec.yml |
| 70 | PASS | riser | pass | pass | pass | pass | library/templates/riser_lazy_wave/spec.yml |
| 71 | PASS | riser | pass | pass | pass | pass | library/templates/riser_pliant_wave/spec.yml |
| 72 | PASS | riser | pass | pass | pass | pass | library/templates/riser_steep_wave/spec.yml |
| 73 | FAIL | generic | pass | pass | pass | fail(10) | library/templates/wind_turbine_fixed/spec.yml |
| 74 | PASS | riser | pass | pass | pass | pass | library/tier2_fast/a01_catenary_riser/spec.yml |
| 75 | PASS | riser | pass | pass | pass | pass | library/tier2_fast/a01_lazy_wave_riser/spec.yml |
| 76 | PASS | riser | pass | pass | pass | pass | library/tier2_fast/a01_pliant_wave_riser/spec.yml |
| 77 | PASS | riser | pass | pass | pass | pass | library/tier2_fast/a01_steep_wave_riser/spec.yml |
| 78 | PASS | generic | pass | pass | pass | pass | mooring/reference/spec.yml |
| 79 | FAIL | - | fail | skip | skip | skip | passing_ship/sample/spec.yml |
| 80 | FAIL | pipeline | pass | fail | skip | skip | pipeline/installation/floating/24in_pipeline/spec.yml |
| 81 | FAIL | pipeline | pass | fail | skip | skip | pipeline/installation/floating/30in_pipeline/spec.yml |
| 82 | FAIL | pipeline | pass | fail | skip | skip | pipeline/installation/s-lay/RS8-ID/spec.yml |
| 83 | FAIL | pipeline | pass | fail | skip | skip | pipeline/installation/s-lay/SB-SA/spec.yml |
| 84 | FAIL | - | fail | skip | skip | skip | reference/spec.yml |
| 85 | FAIL | - | fail | skip | skip | skip | regional/spec.yml |
| 86 | FAIL | - | fail | skip | skip | skip | subsea/jumper/installation/ballymore_mf_plet/spec.yml |
| 87 | FAIL | - | fail | skip | skip | skip | subsea/jumper/installation/ballymore_plet_plem/spec.yml |
| 88 | FAIL | - | fail | skip | skip | skip | training/crane_master/spec.yml |
| 89 | FAIL | - | fail | skip | skip | skip | training/hulls/spec.yml |
| 90 | PASS | generic | pass | pass | pass | pass | training/node_feeding/spec.yml |

## OrcaWave Detailed Results

| # | Status | Schema | Gen | Path |
|---|--------|--------|-----|------|
| 1 | PASS | pass | pass | L00_validation_wamit/2.1/spec.yml |
| 2 | PASS | pass | pass | L00_validation_wamit/2.2/spec.yml |
| 3 | PASS | pass | pass | L00_validation_wamit/2.3/spec.yml |
| 4 | PASS | pass | pass | L00_validation_wamit/2.6/spec.yml |
| 5 | PASS | pass | pass | L00_validation_wamit/2.7/spec.yml |
| 6 | PASS | pass | pass | L00_validation_wamit/2.8/spec.yml |
| 7 | PASS | pass | pass | L00_validation_wamit/2.9/spec.yml |
| 8 | PASS | pass | pass | L00_validation_wamit/3.1/spec.yml |
| 9 | PASS | pass | pass | L00_validation_wamit/3.2/spec.yml |
| 10 | PASS | pass | pass | L00_validation_wamit/3.3/spec.yml |
| 11 | PASS | pass | pass | L02_barge_benchmark/spec.yml |
| 12 | PASS | pass | pass | L03_ship_benchmark/spec.yml |
| 13 | PASS | pass | pass | L04_spar_benchmark/spec.yml |

## Root Cause Analysis

### Schema Failures (11 specs)
- **6x** `seabed.stiffness.shear` expects float but gets non-numeric value (likely a string placeholder like `"TBD"`)
- **2x** Missing `metadata.project` field (older spec format)
- **1x** `current.direction` negative value fails `>= 0` constraint
- **1x** Multiple missing required fields (`metadata`, `environment`, `simulation`)
- **1x** Missing fields in passing_ship domain (different schema expected)

### Generation Failures (5 specs)
- **5x** `'Equipment' object has no attribute 'ramp'` -- all pipeline-type specs hit this. The `Equipment` model is missing the `ramp` attribute that pipeline generation expects.

### YAML-Strict Failures (2 specs)
- **2x** `ImplicitVariableMaxTimeStep` -- invalid OrcaFlex property being generated. The builder emits this property but OrcaFlex does not recognize it.

### Post-Validation Failures (7 specs)
- Lines referencing constraint names, numeric IDs, or special connection targets not in the defined object set. These are mostly valid OrcaFlex references (constraints, numeric buoy IDs) that the post-validator doesn't yet recognize.
