"""Pre-solve mesh quality gates for diffraction packaging (#608).

Bridges ``MeshPipeline`` (mesh loading) and ``GeometryQualityChecker``
(quality analysis) into a gate with a calibrated blocking policy:

- ``FAIL`` (fewer than 3 of 5 checks pass) **blocks** solve/package
  generation — the geometry is unusable.
- ``WARNING`` is reported but never blocks. Calibration note: legitimate
  diffraction hull meshes are open at the waterline, so the watertightness
  check fails them by design; small meshes also trip the panel-count check.
  Such meshes score WARNING and must pass through.

Reports are written machine-readably to ``mesh_quality_report.json`` in the
package directory and surfaced human-readably by the CLI.
"""

from __future__ import annotations

import contextlib
import io
import json
from dataclasses import asdict, dataclass, field
from pathlib import Path

from digitalmodel.hydrodynamics.diffraction.geometry_quality import (
    GeometryQualityChecker,
    GeometryQualityReport,
)

QUALITY_REPORT_FILENAME = "mesh_quality_report.json"

# Quality checks apply to panel meshes the pipeline can load; solver-native
# auxiliary formats (e.g. .fdf free-surface zones) have no panel topology.
_CHECKABLE_EXTENSIONS = {".gdf", ".dat", ".stl"}


class MeshQualityError(ValueError):
    """A mesh failed blocking quality gates; solve/packaging must not proceed."""


@dataclass
class QualityGateResult:
    """Outcome of the quality gate for a single mesh."""

    label: str
    mesh: str
    status: str  # PASS / WARNING / FAIL / SKIPPED
    blocking: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    report: GeometryQualityReport | None = None

    def to_dict(self) -> dict:
        data = {
            "label": self.label,
            "mesh": self.mesh,
            "status": self.status,
            "blocking": self.blocking,
            "warnings": self.warnings,
        }
        if self.report is not None:
            data["report"] = asdict(self.report)
        return data


def _gdf_structural_issues(mesh_path: Path) -> list[str]:
    """WAMIT-GDF structural validity per OrcaWave's strict reader (#740).

    The format allows exactly ONE free-text header line; then line 2 must be
    ``ULEN GRAV`` (two floats), line 3 ``ISX ISY`` (two ints), line 4 the
    panel count (one int). OrcaWave rejects deviations with
    "Unrecognised header" at solve time; failing here moves that error to
    preflight. BEMRosetta's tolerant reader is NOT the arbiter.
    """
    issues: list[str] = []
    try:
        lines = mesh_path.read_text(errors="replace").splitlines()
    except OSError as error:
        return [f"GDF file unreadable: {error}"]
    if len(lines) < 4:
        return ["GDF file has fewer than 4 lines (header, ULEN GRAV, ISX ISY, NPAN)"]

    def _parses(line: str, caster, count: int) -> bool:
        parts = line.split()
        if len(parts) < count:
            return False
        try:
            for part in parts[:count]:
                caster(part)
        except ValueError:
            return False
        return True

    if not _parses(lines[1], float, 2):
        issues.append(
            "GDF line 2 must be 'ULEN GRAV' (two floats); got "
            f"{lines[1]!r}. OrcaWave allows only one header line - extra "
            "leading comment lines are rejected with 'Unrecognised header'."
        )
    if not _parses(lines[2], int, 2):
        issues.append(
            f"GDF line 3 must be 'ISX ISY' (two integers); got {lines[2]!r}."
        )
    if not _parses(lines[3], int, 1):
        issues.append(
            f"GDF line 4 must be the panel count (integer); got {lines[3]!r}."
        )
    return issues


def _collect_issues(report: GeometryQualityReport) -> list[str]:
    return (
        list(report.watertight_issues)
        + list(report.normal_issues)
        + list(report.panel_count_issues)
        + list(report.aspect_ratio_issues)
        + list(report.element_size_issues)
    )


def run_mesh_quality_gate(mesh_path: Path, label: str = "mesh") -> QualityGateResult:
    """Run the geometry quality checks on one mesh file.

    Non-panel formats are SKIPPED. The checker's console narration is
    suppressed; callers present the result themselves.
    """
    mesh_path = Path(mesh_path)
    if mesh_path.suffix.lower() not in _CHECKABLE_EXTENSIONS:
        return QualityGateResult(
            label=label, mesh=mesh_path.name, status="SKIPPED"
        )

    # Solver-strict structural check first: BEMRosetta's readers are
    # tolerant of malformations that OrcaWave hard-rejects (#740), so a
    # structurally invalid file is a certain solver failure -> blocking.
    if mesh_path.suffix.lower() == ".gdf":
        structural = _gdf_structural_issues(mesh_path)
        if structural:
            return QualityGateResult(
                label=label,
                mesh=mesh_path.name,
                status="FAIL",
                blocking=structural,
            )

    from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import MeshPipeline

    mesh = MeshPipeline().load(mesh_path)
    checker = GeometryQualityChecker()
    with contextlib.redirect_stdout(io.StringIO()):
        report = checker.generate_report(
            str(mesh_path), mesh.vertices, mesh.panels
        )

    issues = _collect_issues(report)
    if report.overall_status == "FAIL":
        return QualityGateResult(
            label=label,
            mesh=mesh_path.name,
            status="FAIL",
            blocking=issues,
            report=report,
        )
    warnings = issues if report.overall_status == "WARNING" else []
    return QualityGateResult(
        label=label,
        mesh=mesh_path.name,
        status=report.overall_status,
        warnings=warnings,
        report=report,
    )


def enforce_quality_gates(
    results: list[QualityGateResult], output_dir: Path | None = None
) -> list[str]:
    """Write the machine-readable report and raise on any blocking FAIL.

    Returns the non-blocking warning lines (one per warning, prefixed with
    the mesh label) for the caller to surface.
    """
    if output_dir is not None and any(r.status != "SKIPPED" for r in results):
        report_path = Path(output_dir) / QUALITY_REPORT_FILENAME
        report_path.write_text(
            json.dumps(
                [r.to_dict() for r in results],
                indent=2,
                # the checker stores numpy scalars (np.bool_, np.float64)
                default=lambda o: o.item() if hasattr(o, "item") else str(o),
            )
        )

    failed = [r for r in results if r.status == "FAIL"]
    if failed:
        lines = [
            f"{r.label} ('{r.mesh}'): {issue}"
            for r in failed
            for issue in r.blocking
        ]
        raise MeshQualityError(
            "Mesh quality gates failed (geometry unusable for diffraction):\n  "
            + "\n  ".join(lines)
            + f"\nFull report: {QUALITY_REPORT_FILENAME} in the output directory."
        )

    return [
        f"{r.label} ('{r.mesh}') quality {r.status}: {w}"
        for r in results
        for w in r.warnings
    ]
