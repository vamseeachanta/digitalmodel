# Lay-azimuth warnings: geometry and clearance

Decision: retain `LayAzimuth: 180` for now. It is compatible with the
vertical reset geometry but is not a uniquely determined bearing. The twelve
warnings occur with positive native seabed clearance at every warning event.
Acceptance by the maintainer remains open;
do not treat convergence as warning clearance or this note as model acceptance.

This solver-free assessment covers the named lines in o0-c1/c2/c3 in
[PR 2469](https://github.com/vamseeachanta/digitalmodel/pull/2469).
No criterion,
geometry, damping, tolerance, friction policy, or model input is changed.
The minimum-tension decision remains with the maintainer.

## Native evidence boundary

The pre-flight QA dated 2026-10-17 reports 24/24 staged-statics
convergence (96 of 96 current steps) and twelve warnings:

| Cases | Line | Current stage | Count |
| --- | --- | --- | --- |
| o0-c1/c2/c3 | Flex Joint Rigid Piece | 25% | 3 |
| o0-c1/c2/c3 | Drill String | 50%, 75%, 100% | 9 |

The native minimum seabed clearance at all twelve warning events is 19.425 m
for Flex Joint Rigid Piece and 19.58–21.83 m for Drill String. These values
establish separation from the seabed at the warning events; acceptance of the
warnings remains a decision for the maintainer. Counts and clearances are
the supplied pre-flight QA summary; the native solution is not rerun here.

## Vendor basis

Orcina, [Line data: Statics](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Linedata%2CStatics.htm),
accessed 2026-10-17 UTC: lay azimuth defines the seabed-friction reference
position when friction is included and step 1 is not prescribed. The menu's
suggested bearing runs from bottom to top using current endpoint positions.
Misalignment with the vertical endpoint plane can hinder convergence. Finite
element and analytic catenary representations must be distinguished; the
latter also uses the bearing for effective seabed slope.

Orcina, [Line data, Top end](https://www.orcina.com/webhelp/OrcaFlex/Content/html/Linedata.htm),
accessed 2026-10-17 UTC: the configured top end determines the bottom-to-top
sense. End connection orientation is not a substitute for that sense.
Neither source establishes that convergence alone makes a warning benign.

## Geometry evidence and decision

Paths below are relative to this campaign directory. For each of o0-c1/c2/c3,
`models/<case>/includes/20_generic_objects.yml` gives:

| Input | Drill String | Flex Joint Rigid Piece |
| --- | --- | --- |
| TopEnd | End A | End A |
| End A reset position, m | Free, (0, 0, 34) | Flex Joint local (0, 0, 0); buoy initially (0, 0, -1000) |
| End B reset position, m | Drill String Connection local (0, 0, 0); buoy initially (0, 0, -1000) | Fixed (0, 0, -1000.25) |
| Length, m | 1035 | 0.25 |
| Mid-line support | arc length 10 from A, Rotary Table local (0, 0, -1) | none |
| Representation | Finite element | Finite element |
| Statics | Catenary, Full statics, As laid | Catenary, Full statics, As laid |

The vessel starts at (0, 0, 0). The flat seabed is at z=-1020 m with zero
slope (`models/<case>/includes/03_environment.yml`). Both endpoint horizontal
projections coincide, so atan2(0, 0) cannot supply a physical lay bearing.
180 degrees is therefore compatible at reset; replacing it with 0, a current
heading, or an end orientation has no uniquely supported reset-geometry basis.

The lowest reset endpoint is 20 m above seabed for Drill String and 19.75 m
above it for Flex Joint Rigid Piece. Even an unstretched path directed straight
down from the Drill String reset top cannot reach lower than 34-1035=-1001 m
(19 m clearance). For the rigid piece, the analogous conservative bound from
its fixed end is -1000.25-0.25=-1000.5 m (19.5 m clearance). These are geometry
screens, not bounds on stretched native solutions or on free-end motion.
They support the absence of intended seabed contact, not proof of native
clearance. Seabed friction coefficients on the line types are nonzero; do not
infer irrelevance from the connected buoys' zero friction coefficients.

Current can move the free connections and the Drill String free end. The
bottom-to-top horizontal bearing at a warning stage may therefore differ from
reset and from other stages. Its value cannot be recovered from warning text
or current direction alone. There is insufficient native position evidence to
say 180 is correct at all twelve warning events, or to justify a replacement
number. The native clearances of 19.425 m and 19.58–21.83 m establish no seabed
contact at the twelve warning events. Disabling friction simply to remove the
warning is not justified.

## Native confirmation and explicit disposition still required

The pre-flight QA should retain stage-specific warnings
and the two lines' global A/B positions, designated top ends, minimum
node z/seabed clearance (including line radius), and seabed contact/reaction
readback at zero current and every current stage for all three cases. Record
whether step 1 is run or calculated positions are reused at each stage. Keep
criteria and all solver settings fixed.

If there is no seabed contact throughout, the maintainer can explicitly accept
the warnings as benign for these finite-element, suspended lines on the cited
vendor basis. Record that decision with the native evidence. If contact is
present, review the physical as-laid history and stage endpoint bearings before
changing the owning template and regenerating affected inputs. Re-run the
24-case convergence check after any input change; the twelve warning events
alone do not define the regression scope. The minimum-tension criterion
requires a separate decision from the maintainer.

## Known limits and retained native read-back

The retained o0-c0 and o5-c3 results were read back on 2026-10-17. The
read-back applies to the unchanged models and driver at revision `6440cd33`.

- **Shielding:** both cases record one warning that the drill string inside
  the riser is treated as shielded because the outer line is not free-flooding.
  The o5-c3 warning also names Drill String Below BOP inside Casing. The upper
  free end is at z = 34.9996 m, 14.60 m above the ring at o0-c0 and 18.86 m
  above it at o5-c3; these elevations are above water. For relationship 4,
  Drill String Connection is 0.356 m below the casing top at o0-c0 and
  3.836 m above it at o5-c3. The casing top is at z = -1000 m, so the latter
  protrusion is submerged but shielded, omitting about 0.74 kN of drag at
  1.5 m/s. The above-water observation does not apply to this protrusion.
  Warning disposition remains with the maintainer.
- **Stress:** at o5-c3, maximum von Mises stress is 307.25 MPa at riser End B,
  arc length 1020 m and z = -1000.005 m. The coincident bending moment is
  1858.2 kN m and wall tension is 4431.0 kN. The hand calculation gives
  307.25 MPa at that section; the location and section-load reconciliation
  are established, but acceptance still requires the external stress criterion.
- **Minimum tension:** pre-flight statics give 6,923.1–8,261.7 kN against the
  stored assumed minimum of 8,345.7 kN. No operating window is established;
  the criterion decision remains with the maintainer.
- **Path dependence:** no comparison tolerance is set for the reported
  difference of at most 1.5 kN between the two statics methods.

Solver-free evidence: `tests/drilling_riser/test_generic_lay_azimuth_geometry.py`
checks the reset positions, top-end sense, seabed and unstretched clearance
screens directly from all three committed models. It cannot certify native
warning disposition.
