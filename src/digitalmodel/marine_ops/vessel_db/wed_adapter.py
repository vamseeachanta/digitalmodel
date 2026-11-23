"""
ABOUTME: Adapter that lets digitalmodel CONSUME the worldenergydata vessel fleet
as the single source of truth (it owns the collection/PDF/scrape/dedup pipeline).

worldenergydata curates the real fleet as
``curated/{drilling_rigs,construction_vessels}.csv`` (~2,268 drilling rigs +
165 construction vessels, deduped by IMO). digitalmodel needs the engineering
view of that fleet (crane curves for installation, hull particulars for
diffraction) WITHOUT re-collecting it.

Path resolution (mirrors the OCIMF/LLM_WIKI_PATH precedence used elsewhere):
  1. WED_VESSEL_FLEET_PATH  — points at the curated dir (or its parent)
  2. WORLDENERGYDATA_ROOT   — repo root; curated dir derived from it
  3. local clone fallbacks  — a local worldenergydata checkout (analysis root,
                              ~/workspace-hub, or a sibling of the digitalmodel repo)
Under each candidate root the curated dir is probed at the legacy location
(``data/modules/vessel_fleet/curated/``) first, then at the packaged location
(``packages/worldenergydata-vessel_fleet/src/worldenergydata/vessel_fleet/
_data/curated/``) where the WED monorepo split moved it.
Resolves to None (not an exception) when absent, so consumers degrade gracefully
in environments where worldenergydata is not checked out (e.g. external installs).
"""

from __future__ import annotations

import csv
import logging
import os
from pathlib import Path
from typing import Optional

from digitalmodel.marine_ops.vessel_db.loader import Record, parse_value

logger = logging.getLogger(__name__)

_CURATED_REL = Path("data") / "modules" / "vessel_fleet" / "curated"
_CURATED_REL_PACKAGE = (
    Path("packages") / "worldenergydata-vessel_fleet" / "src"
    / "worldenergydata" / "vessel_fleet" / "_data" / "curated"
)
# Probe order under each candidate WED root: legacy first, then package layout.
_CURATED_RELS = (_CURATED_REL, _CURATED_REL_PACKAGE)


def _local_wed_roots() -> list[Path]:
    """Candidate local worldenergydata checkouts (used when no env var is set)."""
    roots = [
        Path("/mnt/local-analysis/worldenergydata"),  # abs-path-allowed
        Path.home() / "workspace-hub" / "worldenergydata",
    ]
    # Sibling of the digitalmodel repo (…/<parent>/worldenergydata).
    here = Path(__file__).resolve()
    for parent in here.parents:
        if (parent / "digitalmodel").is_dir() or parent.name == "digitalmodel":
            roots.append(parent.parent / "worldenergydata")
            break
    return roots


def resolve_wed_curated_dir(require: bool = False) -> Optional[Path]:
    """Locate the worldenergydata curated vessel-fleet directory.

    Returns the directory holding ``drilling_rigs.csv`` /
    ``construction_vessels.csv``, or None if it cannot be found (unless
    ``require`` is True, in which case an actionable error is raised).
    """
    candidates: list[Path] = []

    env_fleet = os.environ.get("WED_VESSEL_FLEET_PATH")
    if env_fleet:
        p = Path(env_fleet)
        candidates += [p, p / "curated", *(p / rel for rel in _CURATED_RELS)]

    env_root = os.environ.get("WORLDENERGYDATA_ROOT")
    if env_root:
        candidates += [Path(env_root) / rel for rel in _CURATED_RELS]

    # Local-clone fallbacks (legacy layout first, then package layout, per root).
    for root in _local_wed_roots():
        candidates += [root / rel for rel in _CURATED_RELS]

    for c in candidates:
        if (c / "construction_vessels.csv").is_file() or (c / "drilling_rigs.csv").is_file():
            return c

    if require:
        raise FileNotFoundError(
            "worldenergydata curated vessel fleet not found. Set WED_VESSEL_FLEET_PATH "
            "to the curated dir (data/modules/vessel_fleet/curated or packages/"
            "worldenergydata-vessel_fleet/src/worldenergydata/vessel_fleet/_data/curated "
            "under <worldenergydata>), or WORLDENERGYDATA_ROOT to the repo root. "
            "worldenergydata is the source of truth for vessel collection "
            "(https://github.com/vamseeachanta/worldenergydata)."
        )
    logger.warning(
        "worldenergydata curated vessel fleet not found (probed WED_VESSEL_FLEET_PATH, "
        "WORLDENERGYDATA_ROOT and local-clone fallbacks for %s); WED-backed consumers "
        "(construction_vessels/drilling_rigs/installation_vessels) will return 0 WED rows. "
        "Set WED_VESSEL_FLEET_PATH to the curated dir to fix.",
        " or ".join(str(rel) for rel in _CURATED_RELS),
    )
    return None


def _read_csv(path: Path) -> list[dict]:
    with open(path, newline="") as f:
        return list(csv.DictReader(f))


def _record_from_row(row: dict, scope: str, name_col: str) -> Record:
    """Wrap a WED CSV row as a vessel_db Record (citation = DATA_SOURCE)."""
    name = (row.get(name_col) or row.get("VESSEL_NAME") or "?").strip()
    src = row.get("DATA_SOURCE", "worldenergydata")
    url = row.get("DATA_SOURCE_URL", "")
    fields = {k.lower(): v for k, v in row.items() if v not in (None, "")}
    return Record(
        name=name,
        scope=scope,
        layer="particulars",
        vessel_type=row.get("VESSEL_TYPE") or row.get("RIG_TYPE") or "",
        owner_operator=row.get("OWNER") or row.get("OPERATOR") or "",
        year_built=str(row.get("YEAR_BUILT", "")),
        raw_fields=fields,
        citations=[{
            "fields": ["all"],
            "source": f"worldenergydata vessel_fleet ({src})",
            "url": url,
            "access": "public",
        }],
        gaps=[],
    )


def construction_vessels() -> list[Record]:
    """WED construction vessels (crane/pipelay/heavy-lift) as Records."""
    d = resolve_wed_curated_dir()
    if d is None or not (d / "construction_vessels.csv").is_file():
        return []
    return [_record_from_row(r, "install", "VESSEL_NAME")
            for r in _read_csv(d / "construction_vessels.csv")]


def drilling_rigs(offshore_only: bool = True, dimensioned_only: bool = False) -> list[Record]:
    """WED drilling rigs as Records.

    offshore_only filters IS_OFFSHORE; dimensioned_only keeps rows with LOA+beam.
    """
    d = resolve_wed_curated_dir()
    if d is None or not (d / "drilling_rigs.csv").is_file():
        return []
    out = []
    for r in _read_csv(d / "drilling_rigs.csv"):
        if offshore_only and str(r.get("IS_OFFSHORE", "")).lower() not in ("true", "1", "yes"):
            continue
        if dimensioned_only and not (r.get("LOA_M") and r.get("BEAM_M")):
            continue
        out.append(_record_from_row(r, "floating", "RIG_NAME"))
    return out


def construction_crane_vessels() -> dict[str, dict]:
    """Installation crane vessels from WED, shaped for go/no-go.

    Returns ``{vessel_name: {...}}`` with an installation ``CraneCurve`` built
    from the published main (+ aux) crane capacity at reach. WED carries real
    IMO numbers and a published crane reach, so these are preferred over the
    web-research curated install records on name collision.
    """
    from digitalmodel.marine_ops.installation.models import CraneCurve
    import numpy as np

    out: dict[str, dict] = {}
    for rec in construction_vessels():
        f = rec.raw_fields

        def _n(key):
            if key in f:
                v, m = parse_value(f[key])
                if m == "number":
                    return v
            return None

        main_cap = _n("main_crane_capacity_t")
        if main_cap is None or main_cap <= 0:
            continue  # not a crane vessel (e.g. pure pipelay) — skip for crane go/no-go
        main_reach = _n("main_crane_reach_m")
        aux_cap = _n("aux_crane_capacity_t")
        aux_reach = _n("aux_crane_reach_m")

        pts = []
        if main_reach is not None:
            pts.append((main_reach, main_cap))
        if aux_cap and aux_reach is not None:
            pts.append((aux_reach, aux_cap))
        if not pts:
            pts = [(0.0, main_cap)]  # headline only (no reach published)
        pts = sorted(set(pts))
        radii = np.array([p[0] for p in pts], dtype=float)
        caps = np.array([p[1] for p in pts], dtype=float)

        out[rec.name] = {
            "vessel_type": rec.vessel_type,
            "owner_operator": rec.owner_operator,
            "crane_curve": CraneCurve(radii_m=radii, capacities_te=caps,
                                      max_hook_load_te=float(main_cap)),
            "tandem_swl_te": _n("heavy_lift_capacity_t"),
            "deck_area_m2": _n("deck_area_m2"),
            "deck_strength_t_per_m2": None,
            "deck_load_capacity_t": _n("deck_load_capacity_t"),
            "dp_class": f.get("dp_class", ""),
            "imo": f.get("imo_number", ""),
            "source": "worldenergydata",
            "n_citations": 1,
        }
    return out


def fleet_summary() -> dict:
    """Counts available from the WED fleet (0 if not checked out)."""
    d = resolve_wed_curated_dir()
    if d is None:
        return {"available": False, "curated_dir": None,
                "drilling_rigs": 0, "construction_vessels": 0, "crane_vessels": 0}
    n_rigs = len(_read_csv(d / "drilling_rigs.csv")) if (d / "drilling_rigs.csv").is_file() else 0
    n_con = len(construction_vessels())
    n_crane = len(construction_crane_vessels())
    return {"available": True, "curated_dir": str(d),
            "drilling_rigs": n_rigs, "construction_vessels": n_con, "crane_vessels": n_crane}


if __name__ == "__main__":
    import json
    print(json.dumps(fleet_summary(), indent=2))
