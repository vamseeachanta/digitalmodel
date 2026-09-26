# Hull inventory curvature signatures

Serani & Maki (2026), *Geometry-Based Metrics for Early-Stage Hull-Form
Producibility Screening*, arXiv:2609.27544. See [screening guidance](curvature-screening.md).

Generated with HullProd through the existing curvature adapter. Repo-local sources only.
Declared length_m, Lpp, or length_bp supplies lref; otherwise HullProd chooses it automatically.
Panels are source panels (controls: assessed triangles); symmetry expansion can increase the assessed count.
Compare matched mesh densities and reference lengths. Poor reliability is not a successful quality gate.
Crease-dominated signatures describe panel junctions, not plate producibility.
Controls reproduce test_curvature_screen.py: unit sphere (subdivision 3), open cylinder
(radius 6, height 26, 48 x 26), and Wigley half hull (100 x 10 x 6.25, 160 x 40).

Regenerate: python scripts/hull_library/screen_inventory.py (exit 1 if any hull fails).

| hull_id | hull_type | panels | lref | lref_mode | I_D | I_D_plus | I_D_minus | a_flat | a_single | a_elliptic | a_saddle | reliability | crease_dominated |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| inventory_l01_control_surface_mesh_fac3cf24cd | custom | 587 | 114 | auto_principal_span | 13.0603 | 13.0603 | 1.35962e-15 | 0.488563 | 0.46762 | 0.043817 | 0 | poor | False |
| inventory_l01_vessel_mesh_40ca9362fd | ship | 385 | 103.21 | auto_principal_span | 16.2067 | 15.9513 | 0.255402 | 0.401946 | 0.209021 | 0.275708 | 0.113325 | poor | False |
| inventory_l02_oc4_semi_sub_mesh_db621a84f1 | semi_pontoon | 1069 | 73.9322 | auto_principal_span | 47.315 | 38.1582 | 9.15678 | 0.124584 | 0.315483 | 0.443709 | 0.116224 | caution | True |
| inventory_l03_centre_column_cs_8d3ba6272f | semi_pontoon | 1579 | 23 | auto_principal_span | 2.28364 | 2.28364 | 9.26015e-16 | 0.722493 | 0.253256 | 0.0242502 | 0 | caution | True |
| inventory_l03_centre_column_99082a59e2 | semi_pontoon | 336 | 20 | auto_principal_span | 4.80593 | 4.80593 | 9.10943e-16 | 0.019054 | 0.822698 | 0.158248 | 0 | caution | True |
| inventory_l03_outer_column_cs_cd85ee7101 | semi_pontoon | 4758 | 29.918 | auto_principal_span | 1.51184 | 1.51011 | 0.00173586 | 0.825446 | 0.128899 | 0.0227614 | 0.0228933 | caution | True |
| inventory_l03_outer_column_bc77718b51 | semi_pontoon | 1178 | 20 | auto_principal_span | 3.58945 | 2.43333 | 1.15612 | 0.216522 | 0.315091 | 0.364625 | 0.103762 | caution | True |
| inventory_l04_column_bb7a3d1773 | semi_pontoon | 524 | 40 | auto_principal_span | 9.6755 | 7.62419 | 2.05131 | 0.100202 | 0.449616 | 0.276222 | 0.173959 | poor | True |
| inventory_l04_keystone_3ec94751f3 | semi_pontoon | 799 | 40 | auto_principal_span | 16.2839 | 6.53102 | 9.75292 | 0.278565 | 0.33129 | 0.17583 | 0.214315 | poor | True |
| inventory_l04_pontoon_1427b19ab4 | semi_pontoon | 378 | 40 | auto_principal_span | 2.68184e-15 | 1.65354e-15 | 1.0283e-15 | 0.660256 | 0.339744 | 0 | 0 | caution | True |
| inventory_l05_column_d480465b3f | semi_pontoon | 524 | 40 | auto_principal_span | 9.6755 | 7.62419 | 2.05131 | 0.100202 | 0.449616 | 0.276222 | 0.173959 | poor | True |
| inventory_l05_keystone_20e0d43985 | semi_pontoon | 799 | 40 | auto_principal_span | 16.2839 | 6.53102 | 9.75292 | 0.278565 | 0.33129 | 0.17583 | 0.214315 | poor | True |
| inventory_l05_pontoon_6425f5eaa3 | semi_pontoon | 378 | 40 | auto_principal_span | 2.68184e-15 | 1.65354e-15 | 1.0283e-15 | 0.660256 | 0.339744 | 0 | 0 | caution | True |
| inventory_pyramidzc08_376d04a1e1 | custom | 408 | 14.1145 | auto_principal_span | 389.831 | 389.831 | 0 | 0 | 0 | 1 | 0 | caution | False |
| inventory_ellipsoid0096_eff4803e29 | ellipsoid | 96 | 3.91918 | auto_principal_span | 7.61815 | 7.61815 | 0 | 0 | 0 | 1 | 0 | caution | False |
| inventory_spherewithlid_c279d5d307 | sphere | 240 | 2.07282 | auto_principal_span | 4.42379 | 4.42379 | 0 | 0.220693 | 0 | 0.779307 | 0 | caution | False |
| inventory_cylinder_180ceb078d | cylinder | 420 | 2 | auto_principal_span | 1.76713 | 1.76713 | 6.6481e-17 | 0.279372 | 0.612725 | 0.107903 | 0 | caution | True |
| inventory_unit_box_clean_6a410d1077 | barge | 5 | 1.73205 | auto_principal_span | 2.802 | 2.802 | 0 | 0 | 0 | 1 | 0 | caution | False |
| inventory_barge_de92864375 | barge | - | - | - | - | - | - | - | - | - | - | FAILED | - |
| inventory_spar_f408d325be | spar | - | - | - | - | - | - | - | - | - | - | FAILED | - |
| control_sphere | sphere | 1280 | 2 | explicit_user | 4 | 4 | 0 | 0 | 0 | 1 | 0 | good | False |
| control_cylinder | cylinder | 2496 | 12 | explicit_user | 1.86379e-16 | 1.39888e-16 | 4.64906e-17 | 0 | 1 | 0 | 0 | caution | True |
| control_wigley | ship | 12800 | 100 | explicit_user | 4.16338 | 2.11282 | 2.05056 | 0 | 0 | 0.635968 | 0.364032 | caution | False |

## Failed screens

- inventory_barge_de92864375: missing mesh file.
- inventory_spar_f408d325be: missing mesh file.

## Inventory sources

- inventory_l01_control_surface_mesh_fac3cf24cd: docs/domains/orcawave/examples/L01_default_vessel/L01 Control surface mesh.gdf
- inventory_l01_vessel_mesh_40ca9362fd: docs/domains/orcawave/examples/L01_default_vessel/L01 Vessel mesh.gdf
- inventory_l02_oc4_semi_sub_mesh_db621a84f1: docs/domains/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub mesh.gdf
- inventory_l03_centre_column_cs_8d3ba6272f: docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/L03 Centre column CS.gdf
- inventory_l03_centre_column_99082a59e2: docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/L03 Centre column.gdf
- inventory_l03_outer_column_cs_cd85ee7101: docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/L03 Outer column CS.gdf
- inventory_l03_outer_column_bc77718b51: docs/domains/orcawave/examples/L03 Semi-sub multibody analysis/L03 Outer column.gdf
- inventory_l04_column_bb7a3d1773: docs/domains/orcawave/examples/L04 Sectional bodies/L04 Column.gdf
- inventory_l04_keystone_3ec94751f3: docs/domains/orcawave/examples/L04 Sectional bodies/L04 Keystone.gdf
- inventory_l04_pontoon_1427b19ab4: docs/domains/orcawave/examples/L04 Sectional bodies/L04 Pontoon.gdf
- inventory_l05_column_d480465b3f: docs/domains/orcawave/examples/L05 Panel pressures/L05 Column.gdf
- inventory_l05_keystone_20e0d43985: docs/domains/orcawave/examples/L05 Panel pressures/L05 Keystone.gdf
- inventory_l05_pontoon_6425f5eaa3: docs/domains/orcawave/examples/L05 Panel pressures/L05 Pontoon.gdf
- inventory_pyramidzc08_376d04a1e1: docs/domains/orcawave/L00_validation_wamit/2.7/OrcaWave v11.0 files/PyramidZC08.gdf
- inventory_ellipsoid0096_eff4803e29: docs/domains/orcawave/L00_validation_wamit/2.8/OrcaWave v11.0 files/Ellipsoid0096.gdf
- inventory_spherewithlid_c279d5d307: docs/domains/orcawave/L00_validation_wamit/3.2/OrcaWave v11.0 files/SphereWithLid.gdf
- inventory_cylinder_180ceb078d: docs/domains/orcawave/L00_validation_wamit/3.3/OrcaWave v11.0 files/Cylinder.gdf
- inventory_unit_box_clean_6a410d1077: docs/domains/orcawave/L01_aqwa_benchmark/benchmark_results/orcawave/unit_box_clean.gdf
- inventory_barge_de92864375: specs/modules/orcawave/test-configs/geometry/barge.gdf
- inventory_spar_f408d325be: specs/modules/orcawave/test-configs/geometry/spar.gdf
