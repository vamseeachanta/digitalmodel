# Hull inventory curvature signatures

## Correction 2026-09-27

The historical fixed quad diagonal is replaced by the shortest 3-D diagonal
(ties alternate). Discrete curvature depends on the triangulation and winding;
this re-baselines repo-local GDF/DAT signatures without changing source geometry
or reference lengths. These three rows compare the previous committed inventory
with this regenerated table (all use `auto_principal_span`):

| Hull | Source panels | lref | I_D before (fixed) | I_D after (shortest) | Reliability before / after |
| --- | ---: | ---: | ---: | ---: | --- |
| L01 vessel | 385 | 103.21 | 16.2067 | 16.1589 | poor / poor |
| L02 OC4 semi-sub | 1069 | 73.9322 | 47.3150 | 47.4861 | caution / caution |
| L03 outer column | 1178 | 20 | 3.58945 | 3.56101 | caution / caution |

The evaluation note retains its historical values with an explicit correction;
client-hull rows are superseded and will be recomputed outside this repository.
The original inventory and evaluation used different L01 mesh processing, so
the before value here is the inventory's 16.2067, not the evaluation's 15.1.
The inventory script regenerates the data table; retain this correction when
regenerating. See [issue 2253](https://github.com/vamseeachanta/digitalmodel/issues/2253).

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
| inventory_l01_control_surface_mesh_fac3cf24cd | custom | 587 | 114 | auto_principal_span | 13.0695 | 13.0695 | 2.29366e-15 | 0.483742 | 0.473104 | 0.0431548 | 0 | poor | False |
| inventory_l01_vessel_mesh_40ca9362fd | ship | 385 | 103.21 | auto_principal_span | 16.1589 | 15.971 | 0.187954 | 0.401676 | 0.210638 | 0.27558 | 0.112107 | poor | False |
| inventory_l02_oc4_semi_sub_mesh_db621a84f1 | semi_pontoon | 1069 | 73.9322 | auto_principal_span | 47.4861 | 38.412 | 9.0741 | 0.123831 | 0.315609 | 0.444337 | 0.116224 | caution | True |
| inventory_l03_centre_column_cs_8d3ba6272f | semi_pontoon | 1579 | 23 | auto_principal_span | 2.2666 | 2.2666 | 1.01777e-15 | 0.722493 | 0.252322 | 0.0251848 | 0 | caution | True |
| inventory_l03_centre_column_99082a59e2 | semi_pontoon | 336 | 20 | auto_principal_span | 4.87832 | 4.87832 | 7.75486e-16 | 0.019054 | 0.822698 | 0.158248 | 0 | caution | True |
| inventory_l03_outer_column_cs_cd85ee7101 | semi_pontoon | 4758 | 29.918 | auto_principal_span | 1.53854 | 1.53722 | 0.0013215 | 0.825383 | 0.133689 | 0.018534 | 0.022394 | caution | True |
| inventory_l03_outer_column_bc77718b51 | semi_pontoon | 1178 | 20 | auto_principal_span | 3.56101 | 2.43594 | 1.12506 | 0.216522 | 0.315091 | 0.364625 | 0.103762 | caution | True |
| inventory_l04_column_bb7a3d1773 | semi_pontoon | 524 | 40 | auto_principal_span | 9.66351 | 7.67276 | 1.99075 | 0.0990287 | 0.443689 | 0.267167 | 0.190115 | poor | True |
| inventory_l04_keystone_3ec94751f3 | semi_pontoon | 799 | 40 | auto_principal_span | 16.0657 | 6.58173 | 9.48397 | 0.27653 | 0.323085 | 0.188201 | 0.212184 | poor | True |
| inventory_l04_pontoon_1427b19ab4 | semi_pontoon | 378 | 40 | auto_principal_span | 2.39658e-15 | 1.62778e-15 | 7.68808e-16 | 0.660256 | 0.339744 | 0 | 0 | caution | True |
| inventory_l05_column_d480465b3f | semi_pontoon | 524 | 40 | auto_principal_span | 9.66351 | 7.67276 | 1.99075 | 0.0990287 | 0.443689 | 0.267167 | 0.190115 | poor | True |
| inventory_l05_keystone_20e0d43985 | semi_pontoon | 799 | 40 | auto_principal_span | 16.0657 | 6.58173 | 9.48397 | 0.27653 | 0.323085 | 0.188201 | 0.212184 | poor | True |
| inventory_l05_pontoon_6425f5eaa3 | semi_pontoon | 378 | 40 | auto_principal_span | 2.39658e-15 | 1.62778e-15 | 7.68808e-16 | 0.660256 | 0.339744 | 0 | 0 | caution | True |
| inventory_pyramidzc08_376d04a1e1 | custom | 408 | 14.1145 | auto_principal_span | 408.49 | 408.49 | 0 | 0 | 0 | 1 | 0 | caution | False |
| inventory_ellipsoid0096_eff4803e29 | ellipsoid | 96 | 3.91918 | auto_principal_span | 7.56899 | 7.56899 | 0 | 0 | 0 | 1 | 0 | caution | False |
| inventory_spherewithlid_c279d5d307 | sphere | 240 | 2.07282 | auto_principal_span | 4.39015 | 4.39015 | 0 | 0.220693 | 0 | 0.779307 | 0 | caution | False |
| inventory_cylinder_180ceb078d | cylinder | 420 | 2 | auto_principal_span | 1.77073 | 1.77073 | 3.99331e-17 | 0.279372 | 0.612725 | 0.107903 | 0 | caution | True |
| inventory_unit_box_clean_6a410d1077 | barge | 5 | 1.73205 | auto_principal_span | 2.83784 | 2.83784 | 0 | 0 | 0 | 1 | 0 | caution | False |
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
