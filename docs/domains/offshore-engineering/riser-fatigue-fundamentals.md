# Fatigue Fundamentals & Hotspot Mitigation for Offshore Pipe and Risers

## Scope

The fundamentals of **metal fatigue** for offshore oil-and-gas pipe and risers:
what fatigue is, why and how metal fatigues, why certain sections fatigue first
("hotspots"), the two assessment approaches (S-N / Palmgren-Miner vs
fracture-mechanics / Paris), and the design measures used to raise fatigue life at
hotspots (taper / variable-wall-thickness joints, flex joints). Companion to
`scf-to-dnv-c203-fatigue-procedure.md`, which produces the local stress
amplification that feeds the S-N assessment.

## What is fatigue design?

Fatigue occurs under **cyclic loading** — repeated loading and unloading. It is a
gradual process: after a finite number of cycles (the **fatigue life**) the
structure may crack, leak or break. The governing design rule is:

```
Fatigue life  ≥  Service life × Safety factor
```

The safety factor (Design Fatigue Factor) is `> 1.0`; use the value from the
current applicable code for the component class and inspectability.

## Why and how metal fatigues

When cyclic loads exceed a threshold, a crack initiates, grows and the structure
eventually fractures. The phases are:

1. **Initiation** — cyclic slip, crack nucleation.
2. **Crack growth / propagation** — micro-crack growth, then macro-crack growth.
3. **Fracture** — final failure.

## Sources of cyclic loading offshore

A riser or conductor section sees dynamic stress from:
- **Wave and/or vessel motions** — first-order and low-frequency.
- **Vortex-induced vibration (VIV)** under currents.
- **Operational cycling** — changes in temperature, pressure and internal-fluid
  content/density.

## Why fatigue concentrates at hotspots

At certain locations the fatigue damage is an order of magnitude higher than
elsewhere — these are **fatigue hotspots**. Two mechanisms (and their
combination) cause them:

1. **Global load intensification from a boundary-condition change.** Examples:
   - **Soil interface / foundation** for vertical structures — TLP tendons,
     platform legs, wellhead-conductor foundation.
   - **Vessel interface** — TLP tendons, riser tensioners, drill-floor / topside
     interface.
2. **Local geometry discontinuity** — global load is unchanged but local stress is
   amplified (stress concentration). Examples:
   - Changing wall thickness or fillet radius.
   - Weld connections (quality and geometry).
   - Mechanical connectors.
3. **Combination** of both.

The local amplification is quantified as a stress concentration factor (SCF); the
FE procedure to derive it is in `scf-to-dnv-c203-fatigue-procedure.md`.

## Quantifying and evaluating fatigue

### S-N (Palmgren-Miner linear damage)
- Uses **Wöhler S-N (stress-life)** or strain-life curves.
- Inputs: the expected loading (stress-range histogram / spectrum) and the
  acceptable operating duration (fatigue life).
- The chosen S-N detail category dictates the required **weld quality** and
  **thread quality/geometry**.
- Damage accumulates linearly (Miner's rule): `D = Σ (ni / Ni) ≤ 1/DFF`, where
  `ni` are applied cycles in a stress-range bin and `Ni` the allowable cycles from
  the S-N curve.

### Fracture mechanics (Paris)
- Uses **Paris-law** crack-growth curves.
- Inputs: the expected loading and the acceptable operating duration considering
  unstable crack growth.
- The acceptable fatigue life sets the **allowable starting crack size** in the
  component or weld (links to NDE acceptance and inspection planning).

## Managing fatigue at hotspots

Mechanisms to raise fatigue life at hotspots to acceptable levels:

### 1. Variable / tapered wall-thickness joints
A joint whose wall thickness varies (a **taper joint**) distributes the bending
moment over a longer length, lowering peak local stress. Typical applications:
- Tension joint for drilling and completion risers.
- Thicker-wall upper transition joint for a completion riser.
- Thicker telescopic joint for a drilling riser.
- Transition and cross-over joints for risers.
- Vessel interface at the top of a steel catenary riser (SCR).

Use taper joints at the **seabed interface** and the **tensioner / vessel
interface** — the two boundary-condition hotspots.

### 2. Flexible (flex) joints
A flexible elastomeric joint turns a very-high-rotational-stiffness pipe-to-pipe
connection into a low-finite-stiffness ball-and-socket, sharply cutting the local
bending moment. Typical applications:
- Tendon bearings at top and bottom of a **Tension Leg Platform (TLP)**.
- Vessel interface at the top of a **steel catenary riser**.
- Top and bottom of **drilling risers**.
- Bottom of **free-standing risers (FSRs)** and **single-line offset risers
  (SLORs)**.

### 3. Reduce dynamic loads / improve weld quality
Lower the driving stress range (configuration, tensioning, VIV suppression) and
specify higher-quality welds to move to a better S-N detail category.

## Codes and regulations

The prevailing codes prescribe the safety factor for `Design fatigue life =
Service life × Safety factor`, with values `> 1.0` that depend on consequence and
inspectability. Relevant references:

- **DNV-RP-C203** — Fatigue Design of Offshore Steel Structures (S-N curves).
- **API-RP-2A (WSD)** — fixed offshore platforms (general fatigue framework).
- **API-RP-2SK** — station-keeping systems for floating structures.
- **API-RP-2RD** — risers for floating production systems and TLPs.
- Class/regulatory fatigue assessment guidance for offshore structures.

Always confirm the safety factor and detail category against the latest issue of
the applicable code.

## Provenance

Derived and de-identified from internal AceEngineer offshore project work
(~2013–2016).
