# Installation demos — crane Go/No-Go on real vessel data

## `go_no_go_crane_curves.py`

Go/No-Go crane-lift screen driven by **real crane capacity curves** from the
vessel database (`data/vessels/`). For a lift weight and crane working radius it
reads each installation vessel's published crane curve
(`marine_ops.vessel_db.installation_vessels()` → `CraneCurve.capacity_at_radius`)
and evaluates DNV-RP-H103 crane utilisation (static + dynamic with DAF), then
ranks the fleet GO / MARGINAL / NO_GO.

```
cd digitalmodel
uv run python examples/demos/installation/go_no_go_crane_curves.py
uv run python examples/demos/installation/go_no_go_crane_curves.py --lift-te 5000 --radius-m 35
uv run python examples/demos/installation/go_no_go_crane_curves.py --json   # writes output/
```

Example (`--lift-te 5000 --radius-m 35`):

```
  decision   vessel                        SWL@R te  stat UC  dyn UC
  [GO]       Pioneering Spirit                25000    0.200   0.260
  [GO]       Saipem 7000                      14000    0.357   0.464
  [GO]       SSCV Sleipnir                    10000    0.500   0.650
  [NO_GO]    SSCV Thialf                       7100    0.704   0.915
  [NO_GO]    Seven Borealis                    5000    1.000   1.300
```

The crane fleet is sourced primarily from **worldenergydata** (the source of
truth — real IMO numbers + published crane reach), merged with the in-repo
web-research curated records and deduped by normalized name (worldenergydata
wins). worldenergydata vessels (e.g. Sleipnir 10,000 t @ 48 m, Saipem 7000 @
40/48 m) carry a real reach, so the radius is applied; vessels with only a
headline SWL are flagged `*`. If worldenergydata is not checked out, the demo
falls back to the curated records alone. This honours the database "flag, don't
fake" contract — see `data/vessels/README.md`.
