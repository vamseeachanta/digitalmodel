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
    curvature: dict | None = None

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
        if self.curvature is not None:
            data["curvature"] = self.curvature
        return data


def curvature_gate_issues(
    mesh, hull_type: str | None = None, lref: float | None = None
) -> tuple[list[str], list[str], dict | None]:
    """HullProd curvature gate (#2170 D4): returns (blocking, warnings, signature).

    Blocking: HullProd mesh curvature reliability ``poor`` (sliver triangles,
    non-manifold edges, disconnected components, extreme valence) -- these make
    a panel mesh unusable for diffraction as surely as inverted normals.
    Warning only: monohull saddle area fraction above the hull-type threshold.
    No-op (empty lists, None) when the optional ``hullprod`` extra is absent or
    the screen itself fails; the gate never blocks on its own errors.
    """
    from digitalmodel.hydrodynamics.hull_library.curvature_screen import (
        hullprod_available,
        screen_panel_mesh,
    )

    if not hullprod_available():
        return [], [], None
    try:
        result = screen_panel_mesh(
            mesh, lref=lref, hull_type=hull_type, keep_fields=False
        )
    except Exception as exc:  # noqa: BLE001 - advisory gate must not block on itself
        return [], [f"curvature screen skipped: {exc}"], None

    sig = result.signature
    blocking: list[str] = []
    warnings: list[str] = []
    if sig.reliability == "poor":
        blocking.append(
            "curvature reliability POOR (HullProd mesh diagnostics: slivers, "
            "non-manifold edges or disconnected components); repair the mesh"
        )
    saddle = sig.saddle_warning()
    if saddle:
        warnings.append(saddle)
    return blocking, warnings, sig.model_dump(mode="json")


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


def run_mesh_quality_gate(
    mesh_path: Path,
    label: str = "mesh",
    *,
    curvature: bool = True,
    hull_type: str | None = None,
    lref: float | None = None,
) -> QualityGateResult:
    """Run the geometry quality checks on one mesh file.

    Non-panel formats are SKIPPED. The checker's console narration is
    suppressed; callers present the result themselves.

    With ``curvature=True`` (default) and the optional ``hullprod`` extra
    installed, the HullProd curvature gate (#2170 D4) also runs: POOR mesh
    reliability blocks, a monohull saddle fraction above the hull-type
    threshold warns. ``hull_type`` enables the saddle threshold; ``lref``
    (typically Lpp) makes the stored signature comparable across hulls.
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
    curv_blocking: list[str] = []
    curv_warnings: list[str] = []
    signature: dict | None = None
    if curvature:
        curv_blocking, curv_warnings, signature = curvature_gate_issues(
            mesh, hull_type=hull_type, lref=lref
        )

    if report.overall_status == "FAIL" or curv_blocking:
        return QualityGateResult(
            label=label,
            mesh=mesh_path.name,
            status="FAIL",
            blocking=(issues if report.overall_status == "FAIL" else [])
            + curv_blocking,
            warnings=curv_warnings,
            report=report,
            curvature=signature,
        )
    warnings = issues if report.overall_status == "WARNING" else []
    warnings = warnings + curv_warnings
    status = report.overall_status
    if status == "PASS" and curv_warnings:
        status = "WARNING"
    return QualityGateResult(
        label=label,
        mesh=mesh_path.name,
        status=status,
        warnings=warnings,
        report=report,
        curvature=signature,
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
