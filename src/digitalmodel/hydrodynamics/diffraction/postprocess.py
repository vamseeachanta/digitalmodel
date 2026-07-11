"""License-free, Linux-side postprocessing for diffraction (OrcaWave/AQWA) runs.

The licensed solver runs on the OrcaFlex/OrcaWave host; only a small *numeric*
results bundle comes back through the licensed-run lane:

    diffraction_results.json   # DiffractionResults.to_dict() (the authority)
    *_validation.json          # optional host-side validation report
    *.csv                      # optional RAO/added-mass/damping tables

This module reconstructs ``DiffractionResults`` from that bundle (no OrcFxAPI, no
heavy .owr/.owd/.sim/.gdf) and produces, on Linux:

    - validation_report.json   # re-run output validation
    - <vessel>_vessel_type.yml + RAO/AddedMass/Damping CSV + Excel + summary
    - rao_<dof>.png            # RAO magnitude vs period per heading
    - index.json               # manifest of everything written

Entry point: ``postprocess_diffraction_run(bundle_dir, out_dir)``.
"""

from __future__ import annotations

import json
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List

import numpy as np

from digitalmodel.hydrodynamics.diffraction.orcaflex_exporter import OrcaFlexExporter
from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults
from digitalmodel.hydrodynamics.diffraction.output_validator import OutputValidator

RESULTS_JSON_NAMES = ("diffraction_results.json", "results.json")
_RESULT_KEYS = {"raos", "added_mass", "damping"}
_PLOT_DOFS = ("heave", "roll", "pitch")


@dataclass
class PostprocResult:
    """What a Linux postprocess pass produced."""

    vessel: str
    out_dir: Path
    validation_status: str
    exports: Dict[str, Path] = field(default_factory=dict)
    plots: List[Path] = field(default_factory=list)
    validation_report: Path | None = None
    report: Path | None = None
    index: Path | None = None


def load_results_bundle(bundle_dir: str | Path) -> DiffractionResults:
    """Reconstruct DiffractionResults from a returned bundle directory.

    Prefers a well-known ``diffraction_results.json``; otherwise picks the first
    JSON that looks like a serialized DiffractionResults. Raises if none found.
    """
    bundle = Path(bundle_dir)
    if not bundle.is_dir():
        raise NotADirectoryError(f"bundle dir not found: {bundle}")
    for name in RESULTS_JSON_NAMES:
        candidate = bundle / name
        if candidate.is_file():
            return DiffractionResults.from_dict(json.loads(candidate.read_text()))
    for candidate in sorted(bundle.rglob("*.json")):
        try:
            obj = json.loads(candidate.read_text())
        except (ValueError, OSError):
            continue
        if isinstance(obj, dict) and _RESULT_KEYS <= set(obj):
            return DiffractionResults.from_dict(obj)
    raise FileNotFoundError(f"no diffraction results JSON found under {bundle}")


def postprocess_diffraction_run(
    bundle_dir: str | Path,
    out_dir: str | Path,
    *,
    make_plots: bool = True,
) -> PostprocResult:
    """Postprocess one diffraction run bundle on Linux (no license).

    Loads the bundle, re-runs output validation, exports OrcaFlex-ready vessel
    type + coefficient CSVs, renders RAO plots, and writes an index manifest.
    """
    results = load_results_bundle(bundle_dir)
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)

    # 1) Validation rollup (re-run on Linux against the reconstructed object).
    report = OutputValidator(results).run_all_validations()
    validation_path = out / "validation_report.json"
    validation_path.write_text(json.dumps(report, indent=2, default=str))
    status = str(report.get("overall_status") or report.get("status") or "UNKNOWN")

    # 2) OrcaFlex export: vessel_type.yml + RAO/AddedMass/Damping CSV + Excel + summary.
    exports = OrcaFlexExporter(results, out).export_all()

    # 3) RAO plots (optional dep; skipped cleanly if matplotlib is absent).
    plots = _plot_raos(results, out) if make_plots else []

    # 4) Standardized self-contained HTML report (the same generator the
    #    benchmark path uses; a view over the bundle, never a copy). Optional
    #    like the plots — a missing plotly/report dep never fails postprocess.
    report_path = _build_html_report(results, out, report)

    # 5) Index manifest.
    index = {
        "vessel": results.vessel_name,
        "analysis_tool": results.analysis_tool,
        "water_depth": results.water_depth,
        "validation_status": status,
        "validation_report": validation_path.name,
        "exports": {k: Path(v).name for k, v in exports.items()},
        "plots": [p.name for p in plots],
        "report": report_path.name if report_path is not None else None,
    }
    index_path = out / "index.json"
    index_path.write_text(json.dumps(index, indent=2, default=str))

    return PostprocResult(
        vessel=results.vessel_name,
        out_dir=out,
        validation_status=status,
        exports={k: Path(v) for k, v in exports.items()},
        plots=plots,
        validation_report=validation_path,
        report=report_path,
        index=index_path,
    )


def _build_html_report(
    results: DiffractionResults, out: Path, validation_report: Dict[str, Any]
) -> Path | None:
    """Render the standardized diffraction HTML report from the bundle.

    Reuses ``generate_diffraction_report`` (the benchmark path's generator) so a
    single-solver run gets the same report shape. Self-contained
    (``include_plotlyjs`` inline) and deterministic: ``report_date`` is taken
    from the results' own analysis/created date, never ``datetime.now``.

    Returns the path, or ``None`` if the optional report/plotly dependencies are
    absent or anything fails — the report is a convenience, not a contract
    (same policy as the RAO plots).
    """
    try:
        from digitalmodel.hydrodynamics.diffraction.report_generator import (
            build_report_data_from_solver_results,
            compute_natural_periods,
            compute_stability,
            generate_diffraction_report,
        )

        if build_report_data_from_solver_results is None:
            return None

        report_data = build_report_data_from_solver_results(
            {results.analysis_tool: results}, vessel_name=results.vessel_name
        )
        # Deterministic label from the bundle itself.
        report_data.report_date = results.analysis_date or results.created_date or ""

        # Enrich with stability + natural periods when hydrostatics travelled
        # in the bundle (Optional on DiffractionResults). Clean field mapping;
        # skipped silently when absent.
        if results.hydrostatics is not None:
            report_data.hydrostatics = results.hydrostatics
            stability = compute_stability(results.hydrostatics)
            report_data.gm_transverse = stability.get("gm_transverse")
            report_data.gm_longitudinal = stability.get("gm_longitudinal")
            report_data.bm_transverse = stability.get("bm_transverse")
            report_data.bm_longitudinal = stability.get("bm_longitudinal")
            report_data.kb = stability.get("kb")
            if report_data.added_mass_diagonal:
                report_data.natural_periods = compute_natural_periods(
                    results.hydrostatics,
                    report_data.added_mass_diagonal,
                    report_data.frequencies_rad_s,
                )

        report_path = out / f"{results.vessel_name}_diffraction_report.html"
        return generate_diffraction_report(
            report_data,
            report_path,
            include_plotlyjs=True,
            validation_report=validation_report,
        )
    except Exception:  # noqa: BLE001 - report is optional, never fatal
        return None


def _plot_raos(results: DiffractionResults, out: Path) -> List[Path]:
    """RAO magnitude vs wave period, one curve per heading, per DOF.

    Uses a headless matplotlib backend. Returns [] (no error) if matplotlib is
    not installed — plots are a convenience, not a contract.
    """
    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except Exception:  # noqa: BLE001 - plotting is optional
        return []

    plots: List[Path] = []
    rao = results.raos
    for dof_name in _PLOT_DOFS:
        comp = getattr(rao, dof_name)
        periods = np.asarray(comp.frequencies.periods, dtype=float)
        order = np.argsort(periods)
        headings = np.asarray(comp.headings.values, dtype=float)

        fig, ax = plt.subplots(figsize=(7.0, 4.5))
        for j, heading in enumerate(headings):
            ax.plot(
                periods[order],
                np.asarray(comp.magnitude)[order, j],
                marker="o",
                markersize=3,
                label=f"{heading:.0f}°",
            )
        ax.set_xlabel("Wave period T (s)")
        ax.set_ylabel(f"{dof_name.title()} RAO ({comp.unit})")
        ax.set_title(f"{rao.vessel_name} — {dof_name.title()} RAO")
        ax.grid(True, alpha=0.3)
        ax.legend(title="Heading", fontsize="small", ncol=2)
        path = out / f"rao_{dof_name}.png"
        fig.savefig(path, dpi=110, bbox_inches="tight")
        plt.close(fig)
        plots.append(path)
    return plots


def _cli(argv: Any = None) -> int:
    import argparse

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "bundle_dir", help="directory holding the returned results bundle"
    )
    parser.add_argument("--out", default="postproc", help="output directory")
    parser.add_argument("--no-plots", action="store_true")
    args = parser.parse_args(argv)

    result = postprocess_diffraction_run(
        args.bundle_dir, args.out, make_plots=not args.no_plots
    )
    print(f"postprocessed {result.vessel}: validation={result.validation_status}")
    print(
        f"  -> {result.out_dir}  ({len(result.exports)} exports, {len(result.plots)} plots)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(_cli())
