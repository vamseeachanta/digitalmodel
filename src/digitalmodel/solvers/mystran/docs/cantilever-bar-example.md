# Rectangular CBAR cantilever example

## Summary

The generated PBAR and analytical reference use the same bending plane for
`MystranChain.run_cantilever_bar_validation`: the span follows global x, the
orientation vector follows global y, and the tip force follows global z.
The reference displacement uses **I2**, the second moment about local y.
Native MYSTRAN 19.0.0 runs confirm both bending planes for the unequal section
below. Each signed tip displacement agrees with its independent
Euler-Bernoulli target within 0.1 %, against a 1.0 % regression criterion.

## Revision history

| Revision | Date | What changed and why |
|---|---|---|
| 0 | 2026-10-21 | Rectangular-section inputs, hand calculation and deck checks are documented to expose an axis swap hidden by a square section. |
| 1 | 2026-10-22 | Native two-plane results, input decks and parsed excerpts are recorded to confirm the unequal-section bending axes. |

Table 1. Revision history.

## Inputs and assumptions

All example inputs are **assumed, based on previous experience** and selected
for an arithmetic regression. They are not measured geometry, certified material
properties or engineering acceptance limits. Use a consistent SI system.

| Input | Symbol | Value | Unit |
|---|---|---|---|
| Span | L | 1 | m |
| Width along global y | b | 0.05 = 1/20 | m |
| Height along global z | h | 0.1 = 1/10 | m |
| Young's modulus | E | 210 | GPa |
| Poisson's ratio | nu | 0.3 | 1 |
| Tip force along global z | P | 1000 | N |
| Bar subdivisions | n_elems | 4 | 1 |

Table 2. Illustrative inputs; no physical qualification is implied.

## Method and hand calculation

The [MYSTRAN User Manual, 2026-07-01 edition, section 6.2.2.4 and
section 11.51, PBAR](https://github.com/MYSTRANsolver/MYSTRAN_Documentation/blob/99031c472ea669f36db978ae0924661ea7dfd3a1/2_User_Manual/MYSTRAN_User_Manual_2026_07_01.pdf)
defines I1 about element z and I2 about element y. Its CBAR orientation convention
places the x axis and the orientation vector in plane 1. Here local and global
axes coincide, so the z force bends in plane 2.

The rectangular area integrals are evaluated independently:

```text
A  = b h                                      = 1/200 m²
I1 = integral(y² dA) = h [y³/3] from -b/2 to b/2 = h b³/12 = 1/960000 m⁴
I2 = integral(z² dA) = b [z³/3] from -h/2 to h/2 = b h³/12 = 1/240000 m⁴
```

These are centroidal second moments of **area**, consistent with the
[University of Illinois Dynamics Reference, “Moments of inertia”,
area-moment warning](https://dynref.engr.illinois.edu/rem.html)
(undated web edition, accessed 2026-10-21).

The [MIT 2.080J Structural Mechanics solutions (2013), Problem 12-2,
printed page 8](https://ocw.mit.edu/courses/2-080j-structural-mechanics-fall-2013/d822a94a5e00663ee03a4909086181bb_MIT2_080JF13_ProbSet_10_Sol.pdf)
gives the Euler-Bernoulli tip-load result. With the z-bending second moment:

```text
delta_z = P L³ / (3 E I2)
        = 1000 / (3 × 210 × 10⁹ × (1/240000))
        = 1/2625 m
        ≈ 0.381 mm
```

Swapping width and height gives I1 = 1/240000 m⁴, I2 = 1/960000 m⁴
and delta_z = 4/2625 m, approximately 1.524 mm. The ratio is 4.0 and
the difference relative to the tall-section result is 300.0 %.
For the square regression, b = h = 0.1 m gives I1 = I2 = 1/120000 m⁴
and delta_z = 1/5250 m, approximately 0.190 mm.

## Offline reproduction

The following calculation runs without starting or discovering a solver:

```python
from digitalmodel.solvers.mystran import MystranChain

# Assumed inputs from Table 2; the divisor follows the area integral above.
length, width, height = 1.0, 0.05, 0.1
youngs_modulus, tip_force = 210e9, 1000.0
i2 = width * height**3 / 12.0
answer = MystranChain.cantilever_tip_deflection(
    tip_force, length, youngs_modulus, i2,
)
print(f"Analytical z displacement: {answer * 1000:.3f} mm")
# Analytical z displacement: 0.381 mm
```

The offline regressions in
`tests/solvers/mystran/test_bar_validation.py` call the validation helper with
a synthetic F06 writer substituted at `solve`. They inspect the real generated
PBAR area and both second moments, every CBAR orientation vector, the tip grid
and FORCE direction. The synthetic displacement is supplied from the hand
calculation above; it is not native evidence. A response from the old, incorrect
axis is also injected to check that the comparison reports 300.0 % error.

The BDF writer emits eight significant digits. The deck comparison allows
relative rounding error of 5 × 10⁻⁸ with no absolute tolerance: half a unit in
the last significant place divided by the smallest leading digit. This is a
serialization criterion, not an engineering acceptance limit.

## Native run

Two separate linear-static runs were completed on 2026-10-22 using the Table 2
section, first with the 1000 N force along +y, then along +z. The z run calls
`run_cantilever_bar_validation` itself; the y run uses `setup_analysis` with
the same PBAR and orientation. The additional independent y target is:

```text
delta_y = P L³ / (3 E I1)
        = 1000 / (3 × 210 × 10⁹ × (1/960000))
        = 4/2625 m
        ≈ 1.524 mm
```

The executable reports `MYSTRAN Version 19.0.0   JUN 28 2026`. It was built
from the [public 19.0.0 source](https://github.com/MystranSolver/MYSTRANSolver/tree/19.0.0),
commit `3a5913a904ee1adbda9fbcaf9c0a6af8e10f0f9f`, using GNU Fortran 13.3.0,
CMake Release, SuperLU, METIS and system OpenBLAS. The release provides only
a Windows executable. The build used four workers; the runs used one BLAS
and OpenMP thread. No solver source or production module was changed.

| Force plane | PBAR inertia | Hand target (mm) | Native tip (mm) | Relative error (%) | Criterion (%) |
|---|---|---|---|---|---|
| +y | I1 | 1.524 | 1.524 | 0.0 | < 1.0 |
| +z | I2 | 0.381 | 0.381 | 0.0 | < 1.0 |

Table 3. Signed tip agreement; errors use unrounded metre values and are displayed
to one decimal place. Small nonzero errors remain in the machine-readable record.

The y-to-z displacement ratio is 4.0. Each unloaded transverse displacement is
zero in the F06 output. The constrained-root force is -1000 N in the loaded
direction for each case. The epsilon estimates are below the existing test
criterion of 10⁻⁸. Both full outputs contain the `END OF JOB` marker and no
error or fatal diagnostics.

The recorded input decks are
[`y.bdf`](../../../../../tests/solvers/mystran/fixtures/native_rectangular/y.bdf)
and [`z.bdf`](../../../../../tests/solvers/mystran/fixtures/native_rectangular/z.bdf).
The corresponding compact
[`y.F06`](../../../../../tests/solvers/mystran/fixtures/native_rectangular/y.F06)
and [`z.F06`](../../../../../tests/solvers/mystran/fixtures/native_rectangular/z.F06)
excerpts retain the version line, epsilon, all five displacement rows, root
reaction and completion marker. A script selected these lines and removed
trailing spaces; numeric tokens are unchanged. Other headers are omitted.

The [manifest](../../../../../tests/solvers/mystran/fixtures/native_rectangular/manifest.json)
records the source revision, executable digest, build configuration, assumed
inputs and units, UTC completion times, unrounded agreement, original F06
digests, selected line numbers and digests of the committed decks and excerpts.
Full outputs and build logs are retained locally outside the repository;
that retention does not establish backup or public availability.

`tests/solvers/mystran/test_native_run.py` runs this same pair when `mystran`
is on PATH or `MYSTRAN_EXE` names its executable. Otherwise the two live cases
skip with an explicit reason. Three fixture-only cases require no solver:
they regenerate both decks byte for byte, check artifact digests, parse the
recorded F06 excerpts and recompute the stored agreement from the hand targets.

## Native validation limits

The parameterized native test
`tests/solvers/mystran/test_fem_chain.py::TestIntegration::test_cbar_cantilever_matches_analytical`
covers square, tall and wide sections. The existing square-case regression
criteria are retained: tip-displacement error below 0.1 %, reaction-force
relative error within 10⁻⁶, and absolute epsilon below 10⁻⁸. They are test
criteria, not published engineering allowables. The new two-plane runs satisfy
the requested 1.0 % native rectangular confirmation. The older square, reversed
section and solid-mesh native cases were not executed for this record.

The PBAR shear factors are omitted. In MYSTRAN this gives zero transverse
shear flexibility (User Manual section 11.51, remark 6), matching the
Euler-Bernoulli comparison. The result does not assess shear deformation,
geometric nonlinearity, stress recovery or general section orientation.

The helper retains its square-section torsion estimate with coefficient 0.141.
The coefficient is tabulated for a square in
[NPTEL Solid Mechanics, chapter 9, table 9.1](https://archive.nptel.ac.in/content/storage2/courses/105106049/lecnotes/mainch9.html)
(undated web edition, accessed 2026-10-21). It does not establish rectangular
torsional stiffness. This centered tip load produces no torsional moment;
torsion remains NOT_EVALUATED. A torsional load requires a separately justified
section property and validation case.
