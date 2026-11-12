# Parachute calculation follow-up drafts

These five local draft numbers resolve the pack's pending references. They are
not published issue numbers. Publication and replacement of the markers in
[agent-pack.md](agent-pack.md) remain maintainer actions. Source locations below
resolve at code revision `21942e8cfc7dfb985c5bfa693245df7d197cf76c`.

## Draft 1: Validate invalid inputs and make viewer text encoding explicit

The section-6 invalid-input and fallback entries include silent zero stresses,
out-of-table sizing results, missing load nodes and signed-stress cancellation.
The viewer also writes Unicode HTML using the platform default encoding; the
reviewed cp1252 run fails all five `TestGenerateViewerHtml` cases.
[src/digitalmodel/structural/parachute/member_check.py:L22-L71]
[src/digitalmodel/structural/parachute/stroud_sizing.py:L40-L73]
[src/digitalmodel/structural/parachute/frame_viewer_3d.py:L124-L138]

Calculation maintainer closure criteria: define finite/domain and missing-data
policies for the branches assigned to draft 1; add rejection or explicit-status
tests without masking invalid inputs. Use `write_text(html, encoding="utf-8")`
and explicit UTF-8 reads in viewer tests. Run all five nodes listed under SF76
with UTF-8 mode disabled on an interpreter whose default encoding is cp1252,
as well as in UTF-8 mode; require five passes in each configuration. Until then,
the section-4 UTF-8 precondition remains necessary.

## Draft 2: Qualify CAD units, bend fallbacks and viewer geometry

CAD coordinates pass through without an exchange-unit round trip; arc creation
can fall back to a straight tube, and the viewer's quadratic curve is not a
circular arc. Seven CAD tests currently lack the FreeCAD program.
[src/digitalmodel/structural/parachute/freecad_frame_builder.py:L41-L123]
[src/digitalmodel/structural/parachute/frame_viewer_3d.py:L286-L291]
[tests/structural/parachute/test_freecad_frame_builder.py:L15-L24]

CAD maintainer closure criteria: measure exported/reimported dimensions in a
declared unit system, exercise every fallback assigned to draft 2, compare bend
geometry against the intended circle, and execute all seven CAD cases with the
program available. Record output containment and rendered completeness checks.

## Draft 3: Qualify physical inputs and distinguish opening peak from steady drag

Empirical coefficients, sizing limits, geometry presets, material properties and
allowable-stress provenance lack independent physical qualification. The reduced
transient multiplies Cx-scaled drag by a monotone inflation ramp and retains Cx
throughout the history. The aerodynamics reference instead defines Cx as the
opening peak multiplier, with 1.0 for steady state. For Cx greater than 1,
post-inflation drag and deceleration at a given speed are overstated relative to
steady drag; impulse and speed decay are biased. Monotone-speed regression
does not detect this discrepancy or qualify `peak_force_lbs`, `impulse_lbf_s`
and the speed history.
[src/digitalmodel/structural/parachute/chute_assessment.py:L232-L242]
[docs/domains/structural/parachute/parachute-aerodynamics.md:L19-L19]
[docs/domains/structural/parachute/parachute-aerodynamics.md:L49-L51]

Calculation maintainer closure criteria: establish a source-qualified opening
and steady force law; independently check peak-to-steady recovery, impulse and
speed histories for Cx equal to 1 and greater than 1, with time-step refinement.
Resolve the remaining U1/U3 entries, reconcile material sources and governing
editions/clauses, and add calculation Citation sidecars. Report unqualified
outputs explicitly until independent evidence meets the stated criteria.

## Draft 4: Extend and classify implementation-map coverage

The map contains five package rows in one family. Lift is listed in section 3
but lacks a map row; tube sections, stiffness, stress/unity, connections, sizing,
traction and CAD arc geometry lack counted family coverage. Repository-wide
name matches remain discovery candidates, not confirmed independent methods.
[docs/registry/implementation-map.yaml:L16246-L16301]
[src/digitalmodel/structural/parachute/chute_assessment.py:L103-L128]

Map maintainer closure criteria: classify the discovery candidates, add the
missing lift row and method families with package rows, then repeat the
case-sensitive definition-level search using that map revision. Preserve the
distinction between independent computations, delegates and data holders.

## Draft 5: Codename discovery material and separate identifying provenance

One discovery file has an identifying name; preset naming and design-basis
provenance also need codenaming and separate provenance records. The pack omits
the identifying name and makes no claim that private provenance is retained.
[src/digitalmodel/structural/parachute/freecad_frame_builder.py:L137-L143]

Documentation maintainer closure criteria: codename the discovery material and
presets, retain identifying provenance only in an appropriate private record,
and give public readers a non-identifying basis and a usable closure reference.
Publish no private values or identifying discovery names in the pack.
