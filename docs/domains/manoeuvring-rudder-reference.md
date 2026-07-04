# Rudder & low-speed manoeuvring reference — B1528 SIROCCO

Screening-level reference for rudder selection and low-speed ship controllability,
worked on the SIROCCO (B1528) **Panamax/LR1 single-screw tanker** (LBP 225.5 m,
B 32.26 m, T 12.2 m laden, Cb ≈ 0.82, rudder area 44.94 m², span 9.0 m).

> All numbers below were produced by an adversarially-verified research sweep and
> reproduced by unit tests in `tests/test_maneuvering_envelope.py`. This is **not**
> a class/IMO compliance proof or a full MMG/6-DOF manoeuvring model.

Live tool: [`docs/api/hydro/rudder-maneuvering-explorer.html`](../api/hydro/rudder-maneuvering-explorer.html) ·
Code: [`naval_architecture/maneuvering_envelope.py`](../../src/digitalmodel/naval_architecture/maneuvering_envelope.py) ·
Data: [`data/rudder_database.yml`](../../src/digitalmodel/naval_architecture/data/rudder_database.yml)

---

## 0 · Review of the existing rudder calculation

The prior B1528 work is **sound but narrow**:

- `hydrodynamics/propeller_rudder.py` — Söding/Brix + actuator-disk slipstream rudder
  forces (engine-on capable; correctly guarded for engine-off and braking quadrants).
- `naval_architecture/b1528_sirocco_current_heading_rudder*` — the ACMA deliverable:
  the rudder-induced **current-heading force component** for a **moored** vessel
  (SOG = 0, engine off). Explicitly excludes hull current loads, turning, propeller race.
- Reusable building blocks already present: `maneuverability.py` (Whicker-Fehlner lift,
  Nomoto K/T, steady radius, directional stability), `turning_circle.py` (Nomoto
  turning-circle simulation → advance/transfer/tactical diameter), `yaw_moment.py`,
  `rudder_stock_torque.py`.

**Gap filled here:** a rudder-type database, a low-speed turning/threshold-speed layer,
and an engine-on current-balance ("hold heading/position") layer — all composing the
existing physics, parameterised by loading condition.

---

## 1 · Rudder database — objective: selection & sizing

Nine rudder types catalogued (spade, semi-balanced horn, full/unbalanced, flap/Becker,
Schilling, fishtail, Kort nozzle, twisted leading-edge, gate) with area ratio
A_R/(L·T), geometric aspect ratio, max normal-force coefficient, stall behaviour, and
applications. See `rudder_database.yml`.

**SIROCCO classification:** conventional semi-balanced horn / balanced spade. A_R/(L·T) =
**1.63 %** laden — upper-normal for a full-form tanker (1.5–2.0 %). DNV minimum area
A = (L·T/100)(1+25(B/L)²) = **41.6 m²**, so 44.94 m² is compliant with ~8 % margin.
Geometric AR 1.80, effective AR ≈ 3.60 → lift slope **a = 3.77 / rad**.

---

## 2 · Turning circle — objective: turn within the channel

Steady radius **R/L = 1/(K′·δ)**, which is **speed-independent in the linear regime** —
the circle (in ship-lengths) is the same from sea speed down to the steerage threshold;
lower speed only makes the turn *slower*, not *larger*. Tactical diameter TD/L ≈ 2·R/L,
with K′ calibrated to the Lyster & Knights (1979) sea-trial regression.

| Quantity | SIROCCO (35° helm) | IMO MSC.137(76) limit | Verdict |
|---|---:|---:|:--:|
| Tactical diameter | 3.2·L ≈ 720 m | ≤ 5·L = 1128 m | PASS |
| Advance | ≈ 3.2·L ≈ 720 m | ≤ 4.5·L = 1015 m | PASS |

Full-form tankers are marginally course-**unstable** (Clarke discriminant C ≈ −9.3×10⁻⁶):
they need continuous corrective helm to hold a straight course but turn readily once committed.
**Loading:** laden = tighter circle, best rudder bite; ballast ≈ 10–20 % larger circle.

---

## 3 · Threshold speed — objective: keep steerage

Minimum steerage speed from a rudder-vs-beam-wind balance:

> **U_min = (V_w / c) · √( ρ_air·A_L·C_Y / (ρ_w·A_Re·a·sinδ) )**

`c` = inflow factor (≈ 1 coasting; 1.3–1.6 with a kick-ahead feeding the rudder race).

| Condition | A_L | A_Re | U_min @ 20 kn beam wind |
|---|---:|---:|---:|
| Laden, coasting | 2200 m² | 44.94 m² | ≈ **2.9 kn** |
| Ballast, coasting | 3500 m² | 35 m² | ≈ **4.1 kn** |
| Ballast, kick-ahead | 3500 m² | 35 m² | ≈ **2.7 kn** |

Below U_min the ship "won't answer the helm" — tug/thruster assist required. Ballast is the
governing low-speed case (reduced rudder immersion + higher windage).

---

## 4 · Heading-hold under current with engine on — objective: maintain position

To hold heading the rudder yaw moment must cancel the hull current yaw moment
(OCIMF form **N = C_XYc·½ρV_c²·L²·T**). Engine-off the rudder force is tiny and
current-limited; **engine-on** the Söding/Brix slipstream scales the rudder side force by
**propeller thrust**, so:

> **δ_required = asin( |N_current| / N_authority · sinδ_max )**, with
> N_authority = C_rp·F_prop·[1+1/√(1+C_th)]·sinδ_max · x_R

The slipstream bracket runs from ~2 (light loading) to ~1 (bollard); the large near-bollard
force comes from large F_prop, not the bracket. Worked: a near-head 3 kn current
(N ≈ 9 MN·m) is held with ≈ 8° of carried helm at modest thrust. Beam-on, N climbs toward
~38 MN·m (3 kn) / ~105 MN·m (5 kn) and exceeds rudder authority — a tug is then required,
because the rudder adds yaw but not bollard-grade sway.

**Loading:** current load scales with L·T, so it is ~43 % smaller in ballast — but rudder
authority is also degraded by emergence/ventilation, so ballast is not automatically easier.

---

## Citations

Molland & Turnock (2007); Bertram (2012); Brix (1993); Clarke, Gedling & Hine (1983);
Nomoto et al. (1957); Whicker & Fehlner (1958); Soeding (1982); Lyster & Knights (1979);
OCIMF *Prediction of Wind and Current Loads on VLCCs* (1994) / MEG4; IMO Res. MSC.137(76) (2002).
