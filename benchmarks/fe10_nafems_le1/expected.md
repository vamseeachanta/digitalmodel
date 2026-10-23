# Expected values: NAFEMS LE1 and constant-stress patch test, gate FE10

## LE1 elliptic membrane
| quantity | value | unit | source and location |
|---|---|---|---|
| sigma_yy at point D (2000, 0) mm | 92.7 | MPa | NAFEMS, *The Standard NAFEMS Benchmarks*, TNSB Rev. 3 (1990), test LE1 elliptic membrane (licensed; cited by number, title, edition and test name). The value is kept because the received public-source record cites the same value: axia-fem `solver/scripts/gen_nafems.py`, commit 884ec6fe7b74f0a1a9b9ae55e57ccfba625a93c1, line 8 (MIT licence); featool-multiphysics `examples/ex_planestress5.m`, commit 8207417131bc90534931a694a5d8d28ec17ca9fa, line 112; feenox `examples/nafems-le1.yaml`, commit 51bde067cc8f283f3797ee8eaba814cf42c66ce4, line 15 (only the MIT-licensed excerpt is retained in sources/). |

Table 1. Published LE1 value.

Problem definition, from the same open sources (axia-fem gen_nafems.py lines 44-106; FEATool ex_planestress5.m lines 26-27, 39-42, 85-90): quarter of an elliptic membrane between the inner ellipse with semi-axes 2000 mm (x) and 1000 mm (y) and the outer ellipse with semi-axes 3250 mm and 2750 mm; thickness 100 mm; E = 210000 MPa, nu = 0.3; plane stress; outward normal pressure 10 MPa on the outer edge; u_x = 0 on x = 0 and u_y = 0 on y = 0. Point D is the inner point on the x axis.

The fixed FE10 band is within 5 % on the fine benchmark mesh. The decks carry three meshes (8 x 24, 16 x 48, 32 x 96 CQUAD4); acceptance of the fine benchmark mesh and stress-recovery method remains with the maintainer. The proposed method in compare.py is linear extrapolation of the two innermost centroid values near y = 0.

## Constant-stress patch test
Imposed field u = 1e-3 x + 0.5e-3 y, v = 0.5e-3 x + 1e-3 y (mm) on the four corners of a 2 x 1 mm patch of five irregular CQUAD4 elements; plane stress with E = 210000 MPa, nu = 0.3. Closed form computed here (tools/build_expected.py):

| quantity | formula | value (MPa) |
|---|---|---|
| sigma_xx | E/(1 - nu^2) (eps_xx + nu eps_yy) | 300.000 |
| sigma_yy | E/(1 - nu^2) (eps_yy + nu eps_xx) | 300.000 |
| tau_xy | E/(2 (1 + nu)) gamma_xy | 80.769 |

Table 2. Exact uniform stress of the patch test; every element is graded.

The fixed FE10 patch band is exact within 0.1 %.
