# Generic jack-up preload sequence analysis

The comparison calculates three-leg reaction demands for sequential, simultaneous
and staged in-water then elevated preload. It exposes ballast limits, signed
soil-capacity margins, tide sensitivity and the static response to a prescribed
single-leg settlement. The assumed cases do not establish installation acceptance.
The way forward is to qualify the soil, hull, ballast layout, tide prediction,
procedure and structural response before using the comparison operationally.

## Revision history

| Revision | Date | What changed and why |
| --- | --- | --- |
| 0 | 2026-10-28 | Added three preload methods, six-stage water-contact sensitivity and independent equilibrium checks to expose capacity and load-transfer limits. |

## Public basis and assumptions

[Looking back at successful jack-up preloading operations: Managing and applying key data (2021)](https://jack-up.citystgeorges.ac.uk/wp-content/uploads/2025/09/Jackup-2021-13.pdf),
Example Rig Move 1, page 5, describes a 2006 operation using six leg-by-leg stages,
with the first four performed with the hull in the water. Only that procedural
outline is adopted. The source does not publish a complete stage-by-stage load
schedule. The implemented schedule assumes a three-leg sequential sweep within
each of six stages; it does not reproduce unpublished case records. Stages 1
and 2 command fractions `f` and `(1+f)/2` of target; stages 3 through 6 command
the target. Here `f=0.6`, with range 0.4 to 0.8. Stages 1 through 4 have a
positive reference draft; stages 5 and 6 use an assumed elevated air gap.

The [regional coastal wetland Ramsar information sheet (2024)](https://rsis.ramsar.org/RISapp/files/RISrep/GA1652RIS_2412_fr.pdf),
section 4.4.4, page 12, reports tidal excursions of about 1.0 m during neaps and
2.0 m during springs. The default sensitivity interprets 2.0 m as low-to-high
range and assumes symmetric offsets of -1.0, 0.0 and +1.0 m. A separate neap
comparison can use offsets of -0.5 to +0.5 m. This coastal regional context is
not an offshore site prediction, tidal datum, time series or design extreme.
Both public documents were retrieved successfully on 2026-10-28.

A 2026 public industry article on preload draft is contextual information
identified in the study brief; its text was not supplied or used numerically.
Air gap, draft, sea state, tide, ballast and choice of preload method remain
unpublished inputs. They are represented by explicit ranges and alternatives,
without treating any as known event conditions.

Every other numerical scenario input and its range is **assumed, based on
previous experience**. The three existing
[layered soil profiles](../../../examples/data/jackup_layered_penetration/)
retain their input ranges and public equation citations. They illustrate
possible stratigraphies; no regional site investigation is asserted. The example
uses the sand load-spread branch, stiff-over-soft clay and uniform soft clay.

| Input | Default | Independent sensitivity range |
| --- | --- | --- |
| Rectangular hull length, breadth, depth | 40.0, 36.0, 6.0 m | 30.0–70.0, 30.0–66.0, 5.0–10.0 m |
| Longitudinal and transverse leg spacing | 40.0, 44.0 m | 30.0–50.0, 36.0–54.0 m |
| Water weight density | 10.0 kN/m³ | 9.8–10.2 kN/m³ |
| Dry reserve per leg | 12.0 MN | 6.0–18.0 MN |
| Available ballast weight | 36.0 MN | 18.0–72.0 MN |
| Maximum total weight | 72.0 MN | 54.0–108.0 MN |
| Reference minimum draft for in-water stages | 1.0 m | 0.5–1.5 m |
| Reference elevated air gap | 1.0 m | 0.3–1.5 m |
| Prescribed single-leg settlement | 1.0 m | 0.3–2.0 m, rounded for display |
| Significant wave height, context only | 0.0 m | 0.0–2.0 m; wave forces excluded |

The exact settlement input lower bound is one quarter metre. Machine-readable
assumptions preserve this bound. Ranges are independent sensitivity bounds;
the defaults alone are evaluated apart from the explicit tide sweep. Changing
several bounds together requires checking geometry, contact and ballast feasibility.
Maximum weight equals three dry reserves plus available ballast at the default.
No actual ballast compartment layout, pump sequence or equipment rating is supplied.

## Methods and standards clauses

| Method | Implemented states | Clause basis for the method |
| --- | --- | --- |
| One-leg-at-a-time | Three elevated states, each loading one leg and retaining dry reserve on the other two | SNAME T&RB 5-5A, Rev 3 (2008), clause: TO CONFIRM; ISO 19905-1, 2012 edition, clause: TO CONFIRM |
| Simultaneous all-legs | One elevated state, with available extra weight shared equally | SNAME T&RB 5-5A, Rev 3 (2008), clause: TO CONFIRM; ISO 19905-1, 2012 edition, clause: TO CONFIRM |
| Staged leg-by-leg | Six stages with three sequential substeps per stage; first four in water, final two elevated | SNAME T&RB 5-5A, Rev 3 (2008), clause: TO CONFIRM; ISO 19905-1, 2012 edition, clause: TO CONFIRM |

Clauses have not been confirmed against a standard copy. These citations identify
an intended engineering context, without claiming conformity or establishing the
required current edition. No licensed table, complete section or numerical
standard criterion is reproduced.

## Equilibrium, tide and margin conventions

The [extended preload module](../../../src/digitalmodel/marine_ops/preload_sequence.py)
uses the existing `penetration_curve()` result for each leg. Equivalent dry leg
loads encode total hull weight and its two horizontal centre-of-gravity moments.
The weight cap limits the extra load available above the three reserve loads.
For simultaneous loading the default extra demand is 54.0 MN, while only 36.0 MN
is available: each leg therefore receives 24.0 MN. One active leg can reach
30.0 MN while the other two retain 12.0 MN dry load.

Hull draft is positive downward, air gap is negative draft, and positive tide
raises the water at a fixed hull position. Ballast is held constant during each
tide sensitivity. Prescribed leg settlements define a rigid hull plane
`w(x,y)=a+b*x+c*y`. Trim is `atan(c)` and heel is `atan(b)`. The rectangular
bottom is clipped at zero immersion. Integrating the remaining linear draft
and its two first moments gives buoyancy and its centre of action, including
partial water contact. Barycentric support functions transfer that buoyancy into
three leg reactions while preserving vertical force and both moments.

Each fault trial starts from the corresponding nominal stage, holding the other
two support levels fixed. Sequential states trial their active leg; the
simultaneous state independently trials each leg. Trials do not accumulate into
later stages. The imposed 1.0 m settlement is a sensitivity, not a predicted
plunge, transient response, penetration increment or arrest depth. Without water
contact, this prescribed-displacement model produces rotation but no reaction
redistribution. Once water contact occurs, buoyancy changes reactions and can
increase a neighbouring reaction. Deck immersion is rejected. Negative reactions
are retained and flagged as invalid three-support contact; no silent rebalancing
or tensile soil support is introduced.

Output reactions are the static forces **required** by equilibrium. The term
nominal achieved preload refers to the imposed ballast/hydrostatic demand and
does not prove soil support or completed preload. Reactions are not capped at
soil capacity. Signed margins are:

- Target margin: reaction minus target preload; negative means target shortfall.
- Peak headroom: first descending peak minus reaction, or interval maximum minus
  reaction when there is no descending branch. Reaching a descending peak at
  equality is flagged, accounting for shielding by earlier higher resistance.
- Post-peak reserve: first trough minus reaction; negative means demand exceeds
  that lower support. It is null for a monotonic profile.
- Capacity drop: first peak minus first trough, separately from those margins.

The evaluated-interval capacity flag only compares demand with the maximum
available reaction. It does not rule out an intervening capacity loss. Static
recovery depth is not interpreted as dynamic arrest. The inherited per-leg
history screen remains responsible for supplied hold and settlement evidence;
these load snapshots supply no observed time-history or holding qualification.

## Example results and outputs

```sh
PYTHONPATH=src python examples/domains/geotechnical/jackup_preload_methods.py OUTPUT
PYTHONPATH=src python examples/domains/geotechnical/jackup_preload_methods.py OUTPUT --tide-range -0.5 0.5
```

The script writes `comparison.csv`, `summary.json`, `assumptions.json` and, for
each profile, an input JSON and adjacent Plotly figure JSON. The default table
contains 1,242 per-leg rows, retaining stage, substep, nominal/trial condition,
trial leg, tide, draft, reactions, redistribution, trim, heel and signed margins.
Leg indices start at zero. Figures show nominal reaction, peak and post-peak
margins, trim and heel, and redistribution against tide. Separate in-water traces
expose sensitivity that a later elevated maximum would otherwise hide.
Generation requires only the standard library and existing source modules;
there are no runtime network requests. A report can supply a local Plotly renderer.
Machine-readable values retain calculation precision; displayed results use one
decimal place.

At reference tide, sequential and staged elevated loading demand 30.0 MN per
leg; simultaneous loading demands 24.0 MN. The signed margins below include
nominal states and the independent settlement trials at reference tide.

| Profile | Sequential peak headroom (MN) | Simultaneous peak headroom (MN) | Staged peak headroom (MN) | Sequential/staged post-peak reserve (MN) | Simultaneous post-peak reserve (MN) |
| --- | --- | --- | --- | --- | --- |
| Stiff over soft clay | -12.5 | -6.5 | -12.5 | -16.9 | -10.9 |
| Sand over soft clay | -8.6 | -2.6 | -8.6 | -11.1 | -5.1 |
| Uniform soft clay | -4.0 | 2.0 | -4.0 | Not applicable | Not applicable |

The uniform control supplies no descending branch, yet the 30.0 MN demand exceeds
its evaluated maximum. The lower simultaneous demand has more capacity headroom
and a 6.0 MN target shortfall. Neither observation establishes a preferred method.

During in-water full-target substeps, active-leg nominal reactions at tides
-1.0, 0.0 and +1.0 m are 30.0, 25.2 and 20.4 MN. For sand over soft clay,
peak headroom correspondingly changes from -8.6 through -3.8 to +1.0 MN, while
target shortfall grows to 9.6 MN. Additional headroom from buoyancy therefore
comes with reduced preload, rather than increased proven soil capacity. No
active draft controller or compensating ballast is assumed.

With the same imposed settlement, fore-leg loss gives 1.4 degrees trim; an aft
leg loss gives 1.3 degrees heel. These rotations follow geometry, rather than
an inferred difference in method performance. At reference tide the staged
in-water trials produce up to 2.8 MN absolute reaction change, while elevated
trials retain water clearance and have zero buoyancy transfer. Higher tide can
bring an elevated hull into water contact. The CSV retains each resulting load
transfer and the capacity margins of all three legs.

## Evidence, limitations and next steps

The starting targeted suite had 209 passing tests. New expectations are written
as independent hand calculations in the test file: displaced volume, triangular
integration, partial-immersion wedges, both moments, both rotations, weight-cap
allocation and layered peak/trough equations. Additional checks cover invalid
geometry, deck immersion, loss of contact and portable offline output. The
six-stage outline is source-backed; stage fractions, sweeps and all other
procedure particulars remain assumptions.

This vertical static screen excludes soil/structure dynamic coupling, horizontal
centre-of-gravity motion, structural flexibility, jacking controls, wave forces,
soil transport, remoulding, rebound and accumulated penetration between stages.
No ballast reachability or hull structural capacity is qualified. Independent
support histories and the frozen soil profile may differ materially from an
installation. Obtain site investigation and tide data, qualify all ballast and
hull particulars, confirm method clauses, and assess transient response before
selecting or changing an actual procedure.
