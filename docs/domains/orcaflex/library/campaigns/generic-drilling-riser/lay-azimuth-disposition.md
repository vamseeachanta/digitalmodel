# Lay-azimuth review (pmxp2)

Decision: retain `LayAzimuth: 180` for now. It is compatible with the
vertical reset geometry but is not a uniquely determined bearing. The twelve
warnings have a plausible benign explanation (no intended seabed contact),
not a demonstrated native disposition. Maintainer acceptance remains open;
do not treat convergence as warning clearance or this note as model acceptance.

Base and scope follow the CODE-JOB: PR 2469 branch `codex/cxl-rg1b`, at
`caecc66db3d44ccf513349286c130ff7fad6f6bd` (merged PR 2648). This is a consolidated
solver-free investigation of the named lines in o0-c1/c2/c3. No criterion,
geometry, damping, tolerance, friction policy, or model input is changed.
R23 remains with the product manager. Git LFS content is frozen.

## Native evidence boundary

The supplied CODE-JOB dated 2026-10-17 05:25:04Z reports 24/24 statics
convergence and twelve warnings:

| Cases | Line | Current stage | Count |
| --- | --- | --- | --- |
| o0-c1/c2/c3 | Flex Joint Rigid Piece | 25% | 3 |
| o0-c1/c2/c3 | Drill String | 50%, 75%, 100% | 9 |

The referenced solver-loop QA appendix was not available in this checkout.
These counts are the supplied native summary, not independently inspected
native results. No native run was attempted here. No vendor binary was
inspected or reverse-engineered.

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
number. Disabling friction simply to remove the warning is not justified.

## Native confirmation and explicit disposition still required

At the draft PR head, the solver loop should retain stage-specific warnings
and collect the two lines' global A/B positions, designated top ends, minimum
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
alone do not define the regression scope. R23 requires its separate decision.

Solver-free evidence: `tests/drilling_riser/test_generic_lay_azimuth_geometry.py`
checks the reset positions, top-end sense, seabed and unstretched clearance
screens directly from all three committed models. It cannot certify native
warning disposition.
