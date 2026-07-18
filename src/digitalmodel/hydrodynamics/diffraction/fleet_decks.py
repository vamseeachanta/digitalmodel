"""Bulk diffraction-deck generation over the vessel-database fleet.

``vessel_deck_builder.build_deck`` prepares ONE OrcaWave deck from a vessel
record. This module loops the whole fleet (all ``particulars`` records across
scopes, deduped) and:

  - builds a deck per vessel (``decks/<vessel_id>/{hull.gdf, spec.yml,
    <name>.yml, manifest.json}``) with a fleet ``index.json``;
  - emits an OrcaWave **batch config** (YAML) consumable by
    ``orcawave_batch_runner`` and dispatch-ready through the licensed-run lane.

License-free: this only *prepares* decks + the batch config. The solves run on a
licensed OrcaWave/AQWA machine. Parametric forms that a single hull surface can't
represent (semisub/spar) emit a placeholder deck flagged ``mesh-to-supply`` and
are excluded from the batch config by default.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Optional

import yaml

from digitalmodel.hydrodynamics.diffraction.vessel_deck_builder import (
    DeckResult,
    build_deck,
)
from digitalmodel.marine_ops.vessel_db.loader import Record, datasets, iter_records

FLEET_LAYER = "particulars"


@dataclass
class FleetResult:
    """Outcome of a fleet deck-generation pass."""

    out_dir: Path
    decks: list[DeckResult] = field(default_factory=list)
    skipped: list[tuple[str, str]] = field(default_factory=list)  # (vessel, reason)
    index_path: Optional[Path] = None

    @property
    def n_built(self) -> int:
        return len(self.decks)

    @property
    def n_parametric(self) -> int:
        return sum(1 for d in self.decks if d.mesh_kind == "parametric")

    @property
    def n_placeholder(self) -> int:
        return sum(1 for d in self.decks if d.mesh_kind == "placeholder")


def iter_fleet_records(scopes: Optional[list[str]] = None) -> list[Record]:
    """All ``particulars`` records across scopes, deduped by normalized name.

    ``scopes`` restricts to specific dataset scopes (e.g. ``["install"]`` for the
    installation fleet); ``None`` means every scope. First occurrence of a name
    wins (a vessel may appear in more than one scope).
    """
    seen: set[str] = set()
    records: list[Record] = []
    for scope, layer in datasets():
        if layer != FLEET_LAYER:
            continue
        if scopes and scope not in scopes:
            continue
        for record in iter_records(scope, layer):
            key = _name_key(record.name)
            if key in seen:
                continue
            seen.add(key)
            records.append(record)
    return records


def generate_fleet_decks(
    out_dir: str | Path,
    *,
    scopes: Optional[list[str]] = None,
    periods: Optional[list[float]] = None,
    headings: Optional[list[float]] = None,
    water_depth: float | str = "infinite",
    target_panels: int = 1000,
) -> FleetResult:
    """Build an OrcaWave diffraction deck for every fleet vessel with usable dims.

    Vessels missing principal dimensions (LOA/beam/draft) are recorded in
    ``skipped`` rather than failing the run.
    """
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    result = FleetResult(out_dir=out)

    for record in iter_fleet_records(scopes):
        deck = build_deck(
            record,
            out,
            periods=periods,
            headings=headings,
            water_depth=water_depth,
            target_panels=target_panels,
        )
        if deck is None:
            result.skipped.append((record.name, "insufficient principal dimensions"))
            continue
        result.decks.append(deck)

    index = [
        {
            "vessel": d.vessel,
            "vessel_dir": d.deck_path.parent.name,
            "deck": d.deck_path.name,
            "spec": d.spec_path.name,
            "form": d.form,
            "mesh_kind": d.mesh_kind,
            "n_panels": d.n_panels,
            "mass_basis": d.mass_basis,
        }
        for d in result.decks
    ]
    index_path = out / "index.json"
    index_path.write_text(
        json.dumps(
            {
                "n_built": result.n_built,
                "n_parametric": result.n_parametric,
                "n_placeholder": result.n_placeholder,
                "n_skipped": len(result.skipped),
                "water_depth": water_depth,
                "decks": index,
                "skipped": [{"vessel": v, "reason": r} for v, r in result.skipped],
                "provenance": (
                    "parametric prep from vessel_db; run on a licensed OrcaWave/AQWA machine"
                ),
            },
            indent=2,
        )
    )
    result.index_path = index_path
    return result


def emit_orcawave_batch_config(
    fleet: FleetResult,
    out_path: str | Path,
    *,
    execution_mode: str = "parallel",
    max_workers: int = 4,
    include_placeholder: bool = False,
    validate: bool = True,
    generate_plots: bool = True,
    export_formats: Optional[list[str]] = None,
) -> Path:
    """Write an OrcaWave batch config (YAML) for the generated fleet decks.

    The config is consumable by ``orcawave_batch_runner.run_orcawave_batch`` (and
    dispatch-ready through the licensed-run lane). Placeholder decks (no real
    mesh) are excluded unless ``include_placeholder`` — they would fail a solve.
    """
    out_path = Path(out_path)
    base_output_dir = fleet.out_dir / "batch_output"
    jobs = []
    for deck in fleet.decks:
        if deck.mesh_kind == "placeholder" and not include_placeholder:
            continue
        vessel_dir = deck.deck_path.parent
        jobs.append(
            {
                # The batch runner loads jobs via DiffractionSpec.from_yaml, so
                # point it at the canonical spec.yml — NOT the OrcaWave-native
                # deck .yml (which would fail schema validation at load time).
                "spec_path": str(deck.spec_path),
                "job_name": vessel_dir.name,
                "output_dir": str(base_output_dir / vessel_dir.name),
                "validate": validate,
                "generate_plots": generate_plots,
                "export_formats": export_formats or ["csv"],
            }
        )

    config = {
        "execution_mode": execution_mode,
        "max_workers": max_workers,
        "base_output_dir": str(base_output_dir),
        "generate_summary_report": True,
        "jobs": jobs,
    }
    out_path.parent.mkdir(parents=True, exist_ok=True)
    out_path.write_text(yaml.safe_dump(config, sort_keys=False))
    return out_path


def _name_key(name: str) -> str:
    return "".join(ch for ch in name.lower() if ch.isalnum())


def _cli(argv=None) -> int:
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--out", default="fleet_decks", help="output directory")
    parser.add_argument(
        "--scope",
        action="append",
        dest="scopes",
        help="restrict to a dataset scope (repeatable), e.g. --scope install",
    )
    parser.add_argument("--target-panels", type=int, default=1000)
    parser.add_argument(
        "--batch-config",
        default=None,
        help="also write an OrcaWave batch config YAML at this path",
    )
    parser.add_argument(
        "--include-placeholder",
        action="store_true",
        help="include semisub/spar placeholder decks in the batch config",
    )
    args = parser.parse_args(argv)

    fleet = generate_fleet_decks(
        args.out, scopes=args.scopes, target_panels=args.target_panels
    )
    print(
        f"built {fleet.n_built} decks "
        f"({fleet.n_parametric} parametric, {fleet.n_placeholder} placeholder); "
        f"{len(fleet.skipped)} skipped -> {fleet.out_dir}"
    )
    if args.batch_config:
        path = emit_orcawave_batch_config(
            fleet, args.batch_config, include_placeholder=args.include_placeholder
        )
        n_jobs = sum(
            1
            for d in fleet.decks
            if d.mesh_kind != "placeholder" or args.include_placeholder
        )
        print(f"wrote batch config ({n_jobs} jobs) -> {path}")
    return 0


if __name__ == "__main__":
    raise SystemExit(_cli())
