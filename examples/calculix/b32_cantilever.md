# B32 rectangular cantilever

The saved example builds a synthetic quadratic cantilever through `FEMChain`
and `INPWriter`. This case supports native acceptance on CalculiX **2.23**
only. Native acceptance requires an installed open-source `ccx` of that release;
writing the deck alone establishes no displacement or reaction result.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| 1 | 2026-10-27 | Add a reproducible quadratic beam case to extend type-mapping coverage. |
| 2 | 2026-10-27 | Qualify native support forces by solver release after an equilibrium failure. |
| 3 | 2026-11-02 | Require the qualified release before native execution to reject incompatible force recovery. |
| 4 | 2026-11-02 | Accept the documented version-command status so the qualified executable can reach native acceptance. |

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

For native execution, provide a CalculiX 2.23 `ccx` executable and its runtime
libraries through the environment. Confirm that `ccx -v` reports
`This is Version 2.23`. The example and native test both probe that same
executable before solving. The official version command exits with status 201:
`ccx_2.23.c` calls `stop.f`, which calls `exit(201)`. Statuses 0 and 201 are
accepted only with the exact qualified banner. A different or unidentified
release, another failure status or a probe exceeding ten seconds raises an
error; it does not skip force checks.
The release requirement is limited to this B32 case and does not change the
general `FEMChain` interface or claim qualification of other analyses.

```sh
ccx -v
PYTHONPATH=src uv run python examples/calculix/b32_cantilever.py --work-dir "$TMPDIR/b32-deck"
PYTHONPATH=src uv run python examples/calculix/b32_cantilever.py --work-dir "$TMPDIR/b32-native" --solve
PYTHONPATH=src uv run python -m pytest tests/solvers/calculix/test_fem_chain.py tests/solvers/calculix/test_b32_native.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/b32-tests"
```

The example reports solver availability and the required release before building
or solving. Deck generation alone works without `ccx`. `--solve` refuses to
build a deck or solve without the qualified release. The native pytest case
retains its existing skip when `ccx` is absent; that skip does not establish
acceptance. Acceptance requires both an unskipped native test pass and a
successful saved-example `--solve` using CalculiX 2.23, with all displacement
and force checks retained at the declared tolerances. Release-probe regression
tests simulate version output only and provide no native acceptance evidence.

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

The force mapping differs from displacement averaging. In the official
[CalculiX 2.21 source](https://www.dhondt.de/ccx_2.21.src.tar.bz2),
`map3dto1d2d.f` sums the eight expanded end-face nodes once when `iforce=1`.
The DAT total therefore already accounts for beam expansion; multiplying an
original-node RF value by an averaging factor would be incorrect.

## Native qualification

The unchanged native test and saved example were run with CalculiX 2.21 and
2.23 on 2026-10-27. With 2.21, root Fx is approximately `-4.0 N`, exceeding
the `1e-5 N` absolute residual limit against zero. The eight expanded support
nodes also give an incorrect resultant before original-node mapping. This
failure cannot be corrected by changing the parser's summation.

The [CalculiX 2.23 source](https://www.dhondt.de/ccx_2.23.src.tar.bz2)
changes MPC force recovery in `resultsforc.c`. With that release, the existing
native test passes without a skip and the saved example's `--solve` exits
successfully. Tip displacement is `0.311 mm` against the independent
`0.3125 mm` reference. Relative error is calculated from the unrounded
readback and rounded upward to `0.4%`, below the predeclared `1%` limit.
All three DAT root force components satisfy the
unchanged `1e-5 N` residual limit against `(0, -1, 0) N`.

These earlier results qualify this case on 2.23 only. CalculiX 2.21 does not
satisfy its native acceptance criteria; 2.22 native behavior has not been
validated. The shared release guard now rejects every release other than 2.23
before native execution. Its expected version banner follows the `-v` handler
in `ccx_2.23.c` from the official source linked above. No parser multiplier,
reaction assertion change or tolerance relaxation is applied.

The version-status correction was checked on 2026-11-02 with the official 2.23
executable. Its version probe returned 201; the native test passed without a
skip and the saved example's `--solve` succeeded at the unchanged tolerances.
All 29 B32 tests passed, covering both accepted statuses, exact release matching,
other failure statuses, missing executables, probe exceptions and timeouts.

## Limits and remaining work

This is partial scope for issue #1673. Native qualification is limited to the
case and solver release above. The manual's section 5.14 cautions about B32 section
forces; this example checks support equilibrium rather than section stresses
or moments. Mesh convergence, other bending directions, torsion, other cross
sections and nonlinear response remain outside this test's scope. No licensed
solver is needed.
