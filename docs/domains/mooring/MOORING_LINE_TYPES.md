category: asset

# Mooring Line Types (vessel at a berth)

Reference for the standard mooring-line arrangement that holds a vessel alongside
a berth or jetty during cargo operations, loading/unloading, and adverse weather.
Proper mooring restrains the surge, sway, and yaw a vessel would otherwise take up
under wind, waves, tide, and current.

This is the *station-keeping-at-berth* view. For the engineering analysis of
mooring **systems** (line geometry, material, pretension, restoring curves,
fatigue) see [`mooring.md`](mooring.md) and the
[`mooring_analysis`](../../../src/digitalmodel/marine_ops/marine_engineering/mooring_analysis/)
module; for permanent-floater spreads see the CALM-buoy notes in this directory.

## Line types by function

| Line | Led from | Restrains | Purpose |
|---|---|---|---|
| **Head line** | Forward end, leading ahead | Astern motion | Stops the vessel from moving back (aft) along the berth |
| **Stern line** | Aft end, leading astern | Ahead motion | Stops the vessel from moving forward along the berth |
| **Breast line** | Roughly perpendicular to the hull (fore and aft breasts) | Off-berth (sway) motion | Holds the vessel close in to the berth face |
| **Spring line** | Diagonally (fore spring leads aft, aft spring leads forward) | Surge (fore–aft) motion | Controls forward/backward drift; gives extra longitudinal stability during operations |

Head/stern lines and breast lines together resist the off-berth and along-berth
drift; springs take up the longitudinal surge that head/stern lines alone control
poorly. A symmetric arrangement (forward and aft breasts, fore and aft springs)
balances the restraint about the vessel's midship.

## Good practice

- **Right line, right place, right tension.** Balanced tension shares the load
  across the pattern; a slack or over-tight line concentrates load and can part.
- Lines of similar **elasticity and length** should share a load group — mixing
  stiff and soft lines in one group lets the stiff line take the whole load.
- Watch the **vertical and horizontal lead angles**: shallow leads work the
  winches and fairleads efficiently; steep leads waste restraint and chafe.

## Safety and inspection

Correct arrangement and tension matter because poor mooring can damage the
vessel, the berth, cargo equipment, or cause injury. Routinely inspect the load
path end to end — **ropes/wires, winches and brakes, bollards/bitts, and
fairleads/rollers** — for wear, chafe, and corrosion. A well-secured vessel
protects the crew, the cargo, and keeps port operations running smoothly.

## See also

- [`mooring.md`](mooring.md) — mooring module overview and OrcaFlex line setup
- [`../ship-design/berthing/lngc.md`](../ship-design/berthing/lngc.md) — LNGC berthing/mooring context
- OCIMF MEG4 — *Mooring Equipment Guidelines* (industry standard for line
  selection, arrangement, and tending); see [`ocimf_module_readme.md`](ocimf_module_readme.md)
