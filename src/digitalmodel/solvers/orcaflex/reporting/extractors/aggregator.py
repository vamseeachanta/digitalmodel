"""
Multi-line aggregation helpers for building reports from a full OrcaFlex model.

These functions are used by the CLI ``--html-report`` flag to aggregate data
from all Line objects in a loaded simulation file into a single
``OrcaFlexAnalysisReport``.
"""
try:
    import OrcFxAPI as ofx
except ImportError:
    ofx = None

import numpy as np

from ..models.report import OrcaFlexAnalysisReport
from ..models.results import StaticResultsData, DynamicResultsData, EnvelopeData
from ..models.materials import MaterialData
from ..models.mesh import MeshData, SegmentData, MeshQualityData

from .geometry_extractor import extract_geometry
from .mesh_extractor import extract_mesh
from .results_extractor import extract_static_results, extract_dynamic_results
from .materials_extractor import extract_materials_for_lines
from .boundary_conditions_extractor import extract_boundary_conditions
from .loads_extractor import extract_loads


def build_report_from_model(
    model: 'ofx.Model',
    project_name: str = "OrcaFlex Analysis",
    structure_type: str = "riser",
    analyst: str = None,
) -> OrcaFlexAnalysisReport:
    """Build an ``OrcaFlexAnalysisReport`` from a fully loaded OrcaFlex model.

    Selects the primary (longest) line as the geometric reference, aggregates
    mesh and material data from all lines, and combines dynamic envelopes.

    Args:
        model: A loaded OrcaFlex model (simulation or static).
        project_name: Label for the report header.
        structure_type: One of 'riser', 'pipeline', 'jumper', 'mooring', 'installation'.
        analyst: Optional analyst name.

    Returns:
        OrcaFlexAnalysisReport ready to pass to ``generate_orcaflex_report()``.

    Raises:
        ImportError: If OrcFxAPI is not installed.
        ValueError: If the model contains no Line objects.
    """
    if ofx is None:
        raise ImportError("OrcFxAPI is required for extraction.")

    lines = _collect_lines(model)
    if not lines:
        raise ValueError("Model contains no Line objects; cannot build report.")

    primary_line = _select_primary_line(lines)

    # Geometry from primary line
    geometry = _safe_extract(extract_geometry, primary_line, "geometry")

    # Materials deduplicated across all lines
    materials = _safe_extract(extract_materials_for_lines, lines, "materials")

    # Boundary conditions from primary line
    bc = _safe_extract(extract_boundary_conditions, primary_line, "boundary_conditions")

    # Mesh aggregated across all lines
    mesh = _safe_extract(extract_mesh_all_lines, lines, "mesh")

    # Static results: per-line tensions
    static_results = _extract_static_results_all(lines, primary_line)

    # Dynamic results: envelopes from all lines (labelled by name)
    dynamic_results = _extract_dynamic_results_all(lines, model)

    # Loads from model environment
    loads = _safe_extract(extract_loads, model, "loads")

    orcaflex_version = _safe_attr(model, "version") or _safe_attr(model, "OrcaFlexVersion")

    return OrcaFlexAnalysisReport(
        project_name=project_name,
        structure_id=_safe_attr(primary_line, "Name") or "LINE-001",
        structure_type=structure_type,
        analyst=analyst,
        orcaflex_version=str(orcaflex_version) if orcaflex_version else None,
        geometry=geometry,
        materials=materials,
        boundary_conditions=bc,
        mesh=mesh,
        loads=loads,
        static_results=static_results,
        dynamic_results=dynamic_results,
    )


# ---------------------------------------------------------------------------
# Multi-line extraction helpers (exposed for testing)
# ---------------------------------------------------------------------------

def extract_mesh_all_lines(lines: list) -> MeshData:
    """Aggregate mesh segments from all lines into a single MeshData."""
    all_segments = []
    all_ratios = []
    offset = 0.0

    for line in lines:
        try:
            md = extract_mesh(line)
            for seg in md.segments:
                all_segments.append(
                    SegmentData(arc_length_m=seg.arc_length_m + offset, length_m=seg.length_m)
                )
            offset += sum(s.length_m for s in md.segments)
            all_ratios.extend(md.quality.adjacent_ratios or [])
        except Exception:
            continue

    if not all_segments:
        return MeshData(total_segment_count=0)

    worst = max(all_ratios) if all_ratios else 1.0
    worst_arc = all_segments[all_ratios.index(worst)].arc_length_m if all_ratios else 0.0

    return MeshData(
        total_segment_count=len(all_segments),
        segments=all_segments,
        quality=MeshQualityData(
            max_adjacent_ratio=worst,
            worst_ratio_arc_length_m=worst_arc,
            verdict="PASS" if worst < 3.0 else "WARNING",
            adjacent_ratios=all_ratios,
        ),
    )


def export_rangegraph_csvs(
    lines: list,
    variables: list,
    period,
    output_dir: 'Path',
) -> list:
    """Export arc-length distributed RangeGraph results as CSV per line.

    For each line, calls ``line.RangeGraph(var, period)`` for every variable
    and writes a single CSV with columns
    ``ArcLength_m | {Var}_Min | {Var}_Max | {Var}_Mean``.

    Args:
        lines: OrcaFlex Line objects to extract from.
        variables: OrcaFlex variable name strings (e.g. ``'Effective Tension'``).
        period: OrcaFlex period constant (e.g. ``pnDynamic``).
        output_dir: Directory in which to write CSV files.

    Returns:
        List of :class:`pathlib.Path` objects for written CSV files.

    Raises:
        ImportError: If OrcFxAPI is not installed.
    """
    if ofx is None:
        raise ImportError("OrcFxAPI is required for RangeGraph extraction.")

    from pathlib import Path
    import csv

    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    written: list = []

    for line in lines:
        line_name = _safe_attr(line, "Name") or "line"
        rows: dict = {}
        arc_x: list = []

        for var in variables:
            try:
                rg = line.RangeGraph(var, period)
                if not arc_x:
                    arc_x = list(rg.X)
                    rows["ArcLength_m"] = arc_x
                var_safe = var.replace(" ", "_").replace("/", "_")
                rows[f"{var_safe}_Min"] = list(rg.Min)
                rows[f"{var_safe}_Max"] = list(rg.Max)
                rows[f"{var_safe}_Mean"] = list(rg.Mean)
            except Exception:
                continue

        if not arc_x:
            continue

        csv_path = output_dir / f"{line_name}_rangegraph.csv"
        fieldnames = list(rows.keys())
        with open(csv_path, "w", newline="") as fh:
            writer = csv.DictWriter(fh, fieldnames=fieldnames)
            writer.writeheader()
            for i in range(len(arc_x)):
                writer.writerow({k: rows[k][i] for k in fieldnames})

        written.append(csv_path)

    return written


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _collect_lines(model: object) -> list:
    """Return all Line objects from the model."""
    try:
        return [obj for obj in model.objects if obj.type == ofx.ObjectType.Line]
    except Exception:
        try:
            return [obj for obj in model.objects
                    if _safe_attr(obj, "type") == ofx.ObjectType.Line]
        except Exception:
            return []


def _select_primary_line(lines: list) -> object:
    """Select the longest line as the primary reference."""
    def _line_length(line):
        try:
            nodes = list(line.NodeArclengths)
            return nodes[-1] if nodes else 0.0
        except Exception:
            return 0.0

    return max(lines, key=_line_length)


def _extract_static_results_all(lines: list, primary_line: object) -> StaticResultsData:
    """Build StaticResultsData from primary line profile + per-line tensions."""
    base = _safe_extract(extract_static_results, primary_line, "static_results")
    if base is None:
        base = StaticResultsData(end_tensions_kn={})

    per_line: dict = {}
    for line in lines:
        name = _safe_attr(line, "Name") or "unknown"
        try:
            arc_lengths = list(line.NodeArclengths)
            te_a = line.StaticResult("Effective Tension", ofx.oeArcLength(arc_lengths[0]))
            te_b = line.StaticResult("Effective Tension", ofx.oeArcLength(arc_lengths[-1]))
            per_line[name] = {"End A": te_a, "End B": te_b}
        except Exception:
            continue

    if per_line:
        base.per_line_tensions = per_line

    return base


def _extract_dynamic_results_all(lines: list, model: object) -> DynamicResultsData:
    """Build DynamicResultsData aggregating envelopes from all lines."""
    envelopes = []
    ramp = _safe_attr(_safe_attr(model, "general"), "RampDuration") or 0.0

    for line in lines:
        line_name = _safe_attr(line, "Name") or "Line"
        try:
            period = ofx.pnDynamic
            rg_te = line.RangeGraph("Effective Tension", period)
            arc_lengths = list(line.NodeArclengths)
            envelopes.append(EnvelopeData(
                id=f"te_{line_name}",
                label=f"Effective Tension — {line_name}",
                arc_length=arc_lengths,
                max_values=list(rg_te.Max),
                min_values=list(rg_te.Min),
                units="kN",
            ))
        except Exception:
            continue

    return DynamicResultsData(
        ramp_end_time_s=float(ramp) if ramp else 0.0,
        envelopes=envelopes,
    )


def _safe_extract(fn, *args, label: str = ""):
    """Call fn(*args); return None and swallow exceptions."""
    try:
        return fn(*args)
    except Exception:
        return None


def _safe_attr(obj: object, name: str):
    """Return ``getattr(obj, name)`` or ``None``."""
    try:
        return getattr(obj, name)
    except Exception:
        return None
