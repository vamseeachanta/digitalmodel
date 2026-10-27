# B32 rectangular cantilever

The saved example builds a synthetic quadratic cantilever through `FEMChain`
and `INPWriter`. Native acceptance requires an installed open-source `ccx`;
writing the deck alone establishes no displacement or reaction result.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-27 | Add a reproducible quadratic beam case to extend type-mapping coverage. |

## Assumptions and independent reference

The assumed inputs use N and mm: length `L=100`, rectangular width along z
`b=2`, height along y `h=4`, modulus `E=100000`, Poisson ratio `nu=0.3`,
and tip force `P=+1` in global y. These are synthetic test inputs, not a
material specification. Twenty equal B32 elements have a perfectly fixed root
with all translations and rotations restrained. The analysis is linear static.

Integrating `E*I*v''(x)=P*(L-x)` with `v(0)=v'(0)=0` gives
`v(L)=P*L^3/(3*E*I)`, where `I=b*h^3/12`. Thus the independent expected tip
displacement is `0.3125 mm`. Global force balance gives root reaction
`Fy=-1 N`, with zero x and z totals. The reference neglects shear flexibility.

Before solving, the tests allow 1% relative deflection error for discretization
and `1e-5 N` absolute force residual for text output rounding. These numerical
tolerances are specific to this test and are not design criteria.

## Run

From the repository root, with the test dependencies available, write outputs
to an external scratch directory selected by the caller:

```sh
PYTHONPATH=src uv run python examples/calculix/b32_cantilever.py --work-dir "$TMPDIR/b32-deck"
PYTHONPATH=src uv run python examples/calculix/b32_cantilever.py --work-dir "$TMPDIR/b32-native" --solve
PYTHONPATH=src uv run python -m pytest tests/solvers/calculix/test_fem_chain.py tests/solvers/calculix/test_b32_native.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/b32-tests"
```

The example reports solver availability before building or solving. `--solve`
refuses to proceed without `ccx`. The native pytest case skips when `ccx` is
absent; acceptance requires that case to pass without a skip.

## Connectivity and readback

[Gmsh node ordering](https://gmsh.info/doc/texinfo/gmsh.html#Node-ordering)
places the two Line3 endpoints before the midpoint. The writer converts this
to endpoint, midpoint, endpoint for B32 as shown in the
[CalculiX 2.22 manual](https://www.dhondt.de/ccx_2.22.pdf), section 6.2.33,
Figure 73. The independent deck test checks both the keyword and explicit node
ordering before native execution.

The example supplements the writer's default output cards with
`*NODE FILE, OUTPUT=2D` for original-node displacement IDs and
`*NODE PRINT, NSET=ROOT, TOTALS=ONLY` with `RF` for root force totals.
These cards follow manual sections 7.96 and 7.98. The root has no applied
concentrated or distributed load, so its external force is the reaction.
The existing result parser reads the FRD displacement and DAT force totals;
expanded corner-node numbering is not used to identify the tip.

## Limits and remaining work

This is partial scope for issue #1673. Native results are unverified until
the solver test passes. The manual's section 5.14 cautions about B32 section
forces; this example checks support equilibrium rather than section stresses
or moments. Mesh convergence, other bending directions, torsion, other cross
sections and nonlinear response remain outside this test's scope. No licensed
solver is needed.
