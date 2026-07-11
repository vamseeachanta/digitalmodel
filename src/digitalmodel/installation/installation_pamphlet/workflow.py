"""Headless, config-driven workflow that produces an installation-vessel pamphlet.

This module is the IO / orchestration boundary (impure side-effects are allowed
here only): it resolves the vessel from the vessel database, resolves lift items
from explicit values or catalog references, optionally scans the licensed-run
queue, calls the PURE calculation functions, renders deterministic HTML, and
writes the ``*_pamphlet.html`` and ``*_pamphlet_data.json`` artifacts.

All numeric / physics work lives in ``calculations.py``; all rendering lives in
``render.py``. Determinism: identical config -> byte-identical artifacts.
"""

from __future__ import annotations

import glob
import json
from pathlib import Path
from typing import Any

from digitalmodel.installation.installation_pamphlet import calculations as calc
from digitalmodel.installation.installation_pamphlet.render import (
    DEFAULT_GENERATED_LABEL,
    render_pamphlet_html,
)

REPO_ROOT = Path(__file__).resolve().parents[4]


def router(cfg: dict) -> dict:
    settings = cfg.get("installation_pamphlet") or {}

    vessel_info = _resolve_vessel(settings)
    lifts = _resolve_lifts(cfg, settings)
    radius_m = float(settings.get("lift_radius_m", 40.0))
    daf = float(settings.get("daf", 1.30))
    operations = list(settings.get("operations") or calc.DEFAULT_OPERATIONS)
    tp_grid_s = [float(t) for t in (settings.get("tp_grid_s") or calc.DEFAULT_TP_GRID_S)]
    hs_scatter_limits = [
        float(h)
        for h in (settings.get("hs_scatter_limits") or [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0])
    ]
    completed_runs = _resolve_completed_runs(cfg, settings)

    rao_basis = str(settings.get("rao_basis", "auto"))
    if rao_basis == "auto":
        rao_basis = calc.select_rao_basis(completed_runs)
    generated_label = settings.get("generated_label") or DEFAULT_GENERATED_LABEL

    # Explicit config artifact path (resolved like catalog refs). Explicit config
    # wins over any completed-run-carried artifact path (resolved in calculations).
    rao_artifact_cfg = settings.get("rao_artifact")
    rao_artifact = (
        str(_config_path(cfg, rao_artifact_cfg)) if rao_artifact_cfg else None
    )

    result = calc.assemble_result(
        vessel_info=vessel_info,
        lifts=lifts,
        radius_m=radius_m,
        daf=daf,
        rao_basis=rao_basis,
        operations=operations,
        tp_grid_s=tp_grid_s,
        hs_scatter_limits=hs_scatter_limits,
        completed_runs=completed_runs,
        rao_artifact=rao_artifact,
    )
    html = render_pamphlet_html(result, generated_label)

    output_dir = _output_dir(cfg, settings)
    stem = _input_stem(cfg)
    html_path = output_dir / f"{stem}_pamphlet.html"
    data_path = output_dir / f"{stem}_pamphlet_data.json"
    output_dir.mkdir(parents=True, exist_ok=True)
    html_path.write_text(html, encoding="utf-8")
    data_path.write_text(
        json.dumps(_json_safe(result), indent=2, ensure_ascii=False), encoding="utf-8"
    )

    summary = {
        "vessel": vessel_info["name"],
        "rao_basis": rao_basis,
        "rao_provenance": result["rao_provenance"],
        "n_lifts": len(result["lifts"]),
        "n_completed_runs": result["provenance"]["n_completed_runs"],
        "basis_run_id": result["provenance"]["basis_run_id"],
        "all_lifts_defensible": all(bool(r["defensible"]) for r in result["lifts"]),
        "pamphlet_html": _display_path(html_path),
        "pamphlet_data_json": _display_path(data_path),
    }
    cfg["installation_pamphlet"] = {**settings, "summary": summary, "result": result}
    cfg.setdefault("outputs", {})["pamphlet_html"] = _display_path(html_path)
    cfg["outputs"]["pamphlet_data_json"] = _display_path(data_path)
    _print_self_check(summary, result)
    return cfg


# ---------------------------------------------------------------- vessel
def _resolve_vessel(settings: dict[str, Any]) -> dict[str, Any]:
    from digitalmodel.marine_ops.vessel_db.loader import installation_vessels

    match = str(settings.get("vessel", "BokaLift")).lower()
    vlib = installation_vessels()

    def disp(key, info):
        return info.get("_display") or key

    try:
        key, info = next(
            (k, i) for k, i in vlib.items() if match in disp(k, i).lower()
        )
    except StopIteration as exc:  # pragma: no cover - config error path
        raise ValueError(
            f"installation_pamphlet: no vessel matching '{match}' in vessel database"
        ) from exc

    name = disp(key, info)
    cc = info["crane_curve"]
    return {
        "name": name,
        "type": info.get("vessel_type"),
        "owner": info.get("owner_operator"),
        "max_hook_te": float(cc.max_hook_load_te),
        "deck_area_m2": info.get("deck_area_m2"),
        "deck_strength_t_per_m2": info.get("deck_strength_t_per_m2"),
        "dp_class": info.get("dp_class"),
        "crane_radii_m": [float(r) for r in cc.radii_m],
        "crane_caps_te": [float(c) for c in cc.capacities_te],
    }


# ---------------------------------------------------------------- lift items
def _resolve_lifts(cfg: dict, settings: dict[str, Any]) -> list[dict[str, Any]]:
    items = settings.get("lift_items") or []
    catalogs = settings.get("catalogs") or {}
    mud_cache: dict[str, dict[str, Any]] | None = None
    str_cache: dict[str, dict[str, Any]] | None = None
    jmp_cache: dict[str, dict[str, Any]] | None = None
    resolved: list[dict[str, Any]] = []
    for it in items:
        row = {"item": it["item"], "kind": it.get("kind")}
        if it.get("mass_air_te") is not None:
            row["mass_air_te"] = float(it["mass_air_te"])
        elif it.get("source") == "mudmat":
            if mud_cache is None:
                mud_cache = calc.load_mudmats(_config_path(cfg, catalogs["mudmats"]))
            rec = mud_cache[it["id"]]
            row["mass_air_te"] = float(rec["mass_properties"]["mass_air_te"])
            row["catalog_record"] = rec  # feeds the per-structure splash-zone envelope (T5)
        elif it.get("source") == "structure":
            if str_cache is None:
                str_cache = calc.load_structures(_config_path(cfg, catalogs["structures"]))
            rec = str_cache[it["id"]]
            row["mass_air_te"] = float(rec["mass_properties"]["mass_air_te"])
            row["source"] = "structure"  # flips provenance T8 to the real catalog
            row["catalog_record"] = rec  # feeds the per-structure splash-zone envelope (T5)
        elif it.get("source") == "jumper":
            if jmp_cache is None:
                jmp_cache = calc.load_jumpers(_config_path(cfg, catalogs["jumpers"]))
            rec = jmp_cache[it["id"]]
            row["mass_air_te"] = float(rec["total_mass_air_te"])
        else:  # pragma: no cover - config error path
            raise ValueError(
                f"installation_pamphlet: lift item '{it.get('item')}' needs mass_air_te or a catalog source/id"
            )
        resolved.append(row)
    return resolved


# ---------------------------------------------------------------- completed runs
def _resolve_completed_runs(cfg: dict, settings: dict[str, Any]) -> list[dict[str, Any]]:
    explicit = settings.get("completed_runs")
    if explicit is not None:
        return [dict(r) for r in explicit]
    results_dir = settings.get("queue_results_dir")
    if results_dir:
        return scan_completed_runs(_config_path(cfg, results_dir))
    return []


def scan_completed_runs(results_dir: str | Path) -> list[dict[str, Any]]:
    """Scan a licensed-run queue results dir for finished, successful runs.

    Reads ``*.json`` (sorted for stable ordering), keeps those with
    ``state == "finished"`` and ``returncode == 0``, and extracts
    ``run_id`` / ``workflow`` / ``input`` (and, when present, a ``rao_artifact``
    path the run exported — the real vessel RAO export that lets the "vessel"
    basis ingest an actual RAO). Lives here (not in calculations.py) because it
    touches the filesystem.
    """
    out: list[dict[str, Any]] = []
    base = Path(results_dir)
    if not base.exists():
        return out
    for fpath in sorted(glob.glob(str(base / "*.json"))):
        try:
            with open(fpath, "r", encoding="utf-8") as stream:
                r = json.load(stream)
        except Exception:
            continue
        if r.get("state") == "finished" and r.get("returncode") == 0:
            audit = r.get("audit") or {}
            summary = r.get("summary") or {}
            workflow = audit.get("workflow") or summary.get("workflow", "")
            inp = audit.get("input_relpath") or summary.get("input_relpath", "")
            rao_artifact = (
                r.get("rao_artifact")
                or audit.get("rao_artifact")
                or summary.get("rao_artifact")
            )
            if not rao_artifact:
                # Convention fallback (#1538): a completed diffraction solve
                # returns ``diffraction_results.json`` (the RAO authority, dm
                # #1537) into its results bundle. Derive the artifact path from
                # the returned-files list so the "vessel" basis ingests the real
                # vessel RAO without deckhand needing to add an explicit ref.
                # ``returned_files`` are relative to the queue root; ``base`` is
                # ``<queue>/results``, so ``base.parent`` is the queue root.
                for rf in summary.get("returned_files") or []:
                    if str(rf).endswith("diffraction_results.json"):
                        rao_artifact = str(base.parent / rf)
                        break
            out.append(
                {
                    "run_id": r.get("run_id"),
                    "workflow": workflow,
                    "input": inp,
                    "finished_at": r.get("finished_at"),
                    "rao_artifact": rao_artifact,
                }
            )
    return out


# ---------------------------------------------------------------- path helpers
def _output_dir(cfg: dict, settings: dict[str, Any]) -> Path:
    output_dir = Path(str(settings.get("output_dir", "results")))
    if not output_dir.is_absolute():
        output_dir = _config_dir(cfg) / output_dir
    return output_dir


def _config_path(cfg: dict, value: str) -> Path:
    path = Path(str(value))
    if not path.is_absolute():
        # catalog/queue refs are stated relative to the repo root, like the
        # jumper_installation idiom; fall back to config dir if not found there.
        repo_candidate = REPO_ROOT / path
        if repo_candidate.exists():
            return repo_candidate
        return _config_dir(cfg) / path
    return path


def _config_dir(cfg: dict) -> Path:
    if cfg.get("_config_dir_path"):
        return Path(cfg["_config_dir_path"])
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).parent
    return Path.cwd()


def _input_stem(cfg: dict) -> str:
    if cfg.get("_config_file_path"):
        return Path(cfg["_config_file_path"]).stem
    return str(cfg.get("basename", "installation_pamphlet"))


def _json_safe(value: Any) -> Any:
    if isinstance(value, dict):
        return {key: _json_safe(item) for key, item in value.items()}
    if isinstance(value, (list, tuple)):
        return [_json_safe(item) for item in value]
    if isinstance(value, float):
        return value
    if hasattr(value, "item") and not isinstance(value, (str, bytes)):
        try:
            return value.item()
        except Exception:
            return value
    return value


def _display_path(path: Path) -> str:
    resolved = path.resolve()
    try:
        return str(resolved.relative_to(REPO_ROOT.resolve()))
    except ValueError:
        return str(resolved)


def _print_self_check(summary: dict[str, Any], result: dict[str, Any]) -> None:
    print("SELF-CHECK installation-pamphlet")
    print(f"  vessel: {summary['vessel']}  rao_basis: {summary['rao_basis']} ({summary['rao_provenance']})")
    for r in result["lifts"]:
        print(
            f"    {r['item']:<38} m={r['mass_air_te']:>7.2f} te  "
            f"hook={r['hook_load_te']:>7.1f} te  util={r['utilization_pct']}%  ok={r['defensible']}"
        )
    for op, e in result["envelopes"].items():
        hs6 = e["hs_by_tp"].get("6.0")
        print(f"    envelope {op:<20} alpha={e['alpha']}  Hs@Tp6={hs6}")
    print(f"  html: {summary['pamphlet_html']}")
