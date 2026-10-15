# Closed-form catenary statics inputs

C1 is a chain-like line for which the plain catenary governs. C2 is a light,
compliant line for which the elastic catenary governs. Both OrcaFlex inputs
contain one homogeneous line, with 600 m unstretched length and 1 m target
segments (600 segments). This benchmark calibrates the static hand-calculation
check against published equations. It does not contain program results.

The acceptance band is 5.0% relative difference in top tension, 1.0 degree
absolute difference in top angle, and 5.0% relative difference in horizontal
distance from the top to touchdown. Each quantity must satisfy its own band.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 1 | 2026-10-15 | Initial closed-form references and generated statics inputs. |
| 2 | 2026-10-15 | Added seabed support and penetration bounds; clarified angle exports and the finite-EA offset. |

## Inputs and units

The model uses SI units: m, kN, te and s; gravity is 9.80665 m/s2 and water
density is 1.025 te/m3. The sea surface is z = 0 and the flat seabed is
z = -150 m. There is no current, wave or wind loading, vessel or other object.
Both ends use the generator's `Fixed` connection, with zero connection bending
stiffness so that they can rotate. End A is at (0, 0, -150) m; End B is at
(X, 0, 0) m. Fixed connections avoid the seabed-relative elevation convention
of `Anchored`, which the generator also offers. EI is zero and torsion is off.
Both seabed friction coefficients are zero. Hydrodynamic coefficients are left
at the generic route's defaults; there is no hydrodynamic loading in this check.

| Quantity | C1 | C2 |
| --- | --- | --- |
| X (m) | 520 | 570 |
| Outer diameter (m) | 0.10 | 0.05 |
| Mass per unstretched length (te/m) | 0.1100219 | 0.0224069 |
| EA (kN) | 1000000 | 20000 |
| Nominal submerged weight (kN/m) | 1.000 | 0.200 |
| Governing form | plain | elastic |
| Top tension (kN) | 224.788 | 150.886 |
| Top angle from vertical (degrees) | 19.433 | 53.371 |
| Horizontal distance from End B to touchdown (m) | 131.982 | 419.207 |

The mass-derived submerged weight is `(mass - density*pi*OD**2/4)*g` in kN/m.
The test checks the supplied rounded masses against nominal weights at
1e-4 relative tolerance.
The hand calculation uses nominal weights of 1000 and 200 N/m and EA in N.

The plain form governs when top tension divided by EA is at most 0.5%.
C1 is below this threshold and C2 is above it; `benchmark.json` records the
computed top strain percentages. C2's plain-form top tension is 187.737 kN,
24.4% above its elastic value, computed before rounding the tensions.

C1's model retains finite EA, so its elastic closed-form reference is
224.592 kN, 19.404 degrees and 131.807 m. These values are recorded separately
as `elastic_form_reference` in `benchmark.json`; the plain `expected` values
still govern. Computed before rounding, the elastic offsets from the plain
form are -0.1% in tension, -29.3 millidegrees in angle and -0.1% in distance.
These offsets are within the 5.0% / 1.0 degree / 5.0% band and arise from the
plain-form selection rule, rather than a program error.

## Files and rebuilding

| Path relative to this folder | Purpose |
| --- | --- |
| `benchmark.json` | Inputs, hand values, acceptance band, exports and sources |
| `catenary_hand_values.py` | Independent standard-library calculation and self-checks |
| `inputs/catenary_c1_plain.yml` | Self-contained generated C1 model |
| `inputs/catenary_c2_elastic.yml` | Self-contained generated C2 model |
| `inputs/MANIFEST.json` | Repository-relative model paths, byte counts, SHA-256 and rebuild command |

From the repository root, set `TMPDIR` to writable scratch storage, then run:

```sh
python3 docs/benchmarks/catenary_closed_form/catenary_hand_values.py --benchmark > docs/benchmarks/catenary_closed_form/benchmark.json
PYTHONPATH=src uv run --no-sources python scripts/benchmark/build_catenary_closed_form_inputs.py
PYTHONPATH=src uv run --no-sources python -m pytest tests/benchmarks/test_catenary_closed_form_inputs.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/catenary-tests"
```

To print the unrounded hand values and self-check evidence instead of the
benchmark document:

```sh
python3 docs/benchmarks/catenary_closed_form/catenary_hand_values.py
```

The builder creates `ProjectInputSpec` with `generic=GenericModel`, one
`GenericLineType` and one `GenericLine`. Typed fields express OD, mass, EA and
EI; property dictionaries express connections, segment table, torsion, friction
and statics methods. `ModularModelGenerator.from_spec(...).generate(...)` writes
the includes into scratch. `ModularToSingleConverter.convert(...)` merges them
through the repository's format-conversion route and canonical YAML writer.
The generator's `merge.py` handles named-list replacement for variations; it
does not resolve includes. No variation is used here. No model YAML is written
by hand and no intermediate specification or include is committed.

Both modular and merged files pass `PostGenerationValidator` reference checks.
That validator has no selector-order checker; the offline test additionally
walks ordered YAML mappings to check the relevant selector/dependent pairs.
Empty defaults retain `~`. The generator's quoted empty script text remains
quoted, distinct from a blank scalar or numeric default.

## Hand calculation and sources

For submerged weight w, unstretched length L, height h and horizontal span X,
the plain calculation solves
`s = sqrt(h**2 + 2*h*H/w)` and
`X = L - s + (H/w)*asinh(w*s/H)` for H, with `0 < s < L`.
Then `V = w*s`, `T = hypot(H,V)`, `theta = atan2(H,V)` from the vertical,
and suspended horizontal distance is `(H/w)*asinh(w*s/H)`.

The elastic calculation integrates the published equations with vertical force
zero at touchdown. It solves
`x(s) = H*s/EA + (H/w)*asinh(w*s/H)` and
`z(s) = w*s**2/(2*EA) + (hypot(H,w*s)-H)/w`, with
`z(s) = h` and `X = (L-s)*(1+H/EA) + x(s)`.
Nested bracketed bisection uses relative bracket tolerance below 1e-12.
The height expression is rationalized in code to avoid cancellation.

The script checks the plain limit at EA = 1e26 N to below 1e-9 relative in H.
An independent shooting solve integrates the elastic differential equations
with classical RK4 and 20000 steps. It checks relative top tension below 1e-9
and angle difference below 1e-9 degree against the elastic closed form for
both cases. Its initial guesses depend only on the inputs.

The equation references are:

- Faltinsen, *Sea Loads on Ships and Offshore Structures* (1990), textbook
  catenary; equation number not checked.
- Irvine, *Cable Structures* (1981), elastic cable equations; equation number
  not checked. The two differential equations were confirmed in lines 7 to 17
  of the [public equation reference](https://github.com/xabi80/HSP/blob/03143504bb05ed7ad4a71a3288b3826b8a521046/tests/validation/test_m4_catenary_irvine.py#L7-L17),
  at commit `03143504bb05ed7ad4a71a3288b3826b8a521046`. That file has no
  stated licence; its source text is not reproduced here. Its general vertical
  force expression reduces to `V = w*s` when force at touchdown is zero.

The script imports nothing from `src/digitalmodel`. Issue #2050 records an
elastic-stretch defect in the repository's catenary implementation; the hand
calculation therefore remains standalone.

## Reading an OrcaFlex result

Load one model file and calculate statics only. Do not start dynamics; the data
files retain the generator's default dynamics control fields, but the build
script requests no analysis. In the static results for the single line, export:

1. Effective tension at End B in kN.
2. `Declination` at End B in degrees, the actual line tangent's angle from
   global +Z in the range 0 to 180 degrees, directed from End A towards End B.
   Export the acute angle from vertical as
   `min(Declination, 180 - Declination)`. End A is below End B here, so the
   upward tangent's declination already equals the expected angle. A downward
   tangent uses the supplement. Do not export `No moment declination` or the
   fixed connection orientation. See the manufacturer's
   [line angle results](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Lineresults,Angles.htm)
   and [direction conventions](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Directionconventions.htm#DirectionConventionsAzmDec).
3. Horizontal distance from End B to touchdown in m: subtract the touchdown
   point's global x coordinate from X. Use the suspended/grounded transition
   in the static line geometry.

Compare to that case's `expected` object. Relative difference is
`100*abs(program-expected)/abs(expected)` for tension and distance. For angle
use `abs(program-expected)` in degrees. Record the program version, the three
exported results and each difference before applying the acceptance band.

## Limits

The hand values assume a rigid horizontal seabed and a homogeneous line with
zero bending stiffness, no friction and a positive grounded length. They do
not cover lifted anchors, fully suspended lines or dynamic response.

Both inputs explicitly set `SeabedNormalStiffness` to **10000 kN/m/m2** with
`SeabedModel: Elastic`. Taking contact width as OD, the nominal penetration
estimate `delta = w/(K*OD)` is 1.0 mm for C1 and 0.4 mm for C2. The offline
tests require penetration below 10.0 mm for each case, using both nominal and
mass-derived submerged weights. These are contact estimates, not program
results.

As a height-sensitivity check, increasing the hand calculation's 150 m height
by each estimated penetration changes tension and touchdown distance by less
than 0.1% and angle by less than 0.1 degree in both governing forms. These
bounds are rounded upward from the unrounded differences and are negligible
against the 5.0% / 1.0 degree / 5.0% band. They do not cover discretisation or
contact convergence. A licensed-program load and statics check is still
needed to confirm contact and extract the three comparison results. No
licensed program or programming interface is imported or executed by the
builder or offline tests.
