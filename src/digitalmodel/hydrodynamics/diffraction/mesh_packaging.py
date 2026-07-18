"""Shared mesh-reference collection and packaging for diffraction specs (#605).

Single source of truth for which files a spec makes the solver depend on —
body meshes, the damping lid mesh, per-body control-surface meshes, and the
free-surface zone mesh — and for the path-resolution convention: relative
references resolve against the directory containing the spec file, absolute
paths are used as-is.

Used by both ``OrcaWaveRunner`` (run packaging) and ``SpecConverter``
(convert-spec packaging and pre-flight validation) so the two paths cannot
drift.
"""

from __future__ import annotations

import json
import shutil
from pathlib import Path
from typing import Any, Iterator, NamedTuple

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec

# Formats the solver consumes directly (copied unchanged — no conversion churn)
_SOLVER_READY_EXTENSIONS: dict[str, set[str]] = {
    "orcawave": {".gdf"},
    "aqwa": {".dat"},
}
# Solver-native auxiliary formats with no panel-mesh handler (copied as-is)
_PASSTHROUGH_EXTENSIONS = {".fdf"}
# WAMIT control-surface format — solver-native for OrcaWave control surfaces
# only (BodyControlSurfaceMeshFormat 'Wamit csf'; the L00_validation_wamit
# 2.7/2.8 golden benchmarks reference pyramid_zc08.csf / ellipsoid_96p.csf).
# Not valid as a body panel mesh, so it is accepted per-slot (#609).
_CONTROL_SURFACE_PASSTHROUGH_EXTENSIONS = {".csf"}
# Formats MeshPipeline can load and convert to the solver target
_CONVERTIBLE_EXTENSIONS = {".gdf", ".dat", ".stl"}

PROVENANCE_FILENAME = "mesh_provenance.json"


class MeshReference(NamedTuple):
    """A mesh file the solver will need, with a human-readable label."""

    label: str
    mesh_file: str


class _MeshSlot(NamedTuple):
    """A mesh reference plus the spec object holding it (for rewrites)."""

    label: str
    mesh_file: str
    owner: Any  # model carrying mesh_file (and possibly mesh_format)
    kind: str = "body"  # body | damping_lid | control_surface | free_surface_zone


def _slot_passthrough_extensions(slot: _MeshSlot) -> set[str]:
    """Passthrough formats accepted for this slot's role."""
    if slot.kind == "control_surface":
        return _PASSTHROUGH_EXTENSIONS | _CONTROL_SURFACE_PASSTHROUGH_EXTENSIONS
    return _PASSTHROUGH_EXTENSIONS


def _iter_mesh_slots(spec: DiffractionSpec) -> Iterator[_MeshSlot]:
    bodies = spec.get_bodies()

    for body in bodies:
        geometry = body.vessel.geometry
        if geometry.mesh_file and geometry.mesh_file.strip():
            yield _MeshSlot(
                f"Body '{body.vessel.name}' mesh",
                geometry.mesh_file,
                geometry,
                "body",
            )

    damping_lid = getattr(spec, "damping_lid", None)
    if damping_lid is not None and damping_lid.mesh_file:
        yield _MeshSlot(
            "Damping lid mesh", damping_lid.mesh_file, damping_lid, "damping_lid"
        )

    for body in bodies:
        control_surface = body.resolve_control_surface()
        if control_surface is not None and control_surface.mesh_file:
            yield _MeshSlot(
                f"Body '{body.vessel.name}' control surface mesh",
                control_surface.mesh_file,
                control_surface,
                "control_surface",
            )

    free_surface_zone = getattr(spec, "free_surface_zone", None)
    if free_surface_zone is not None and free_surface_zone.mesh_file:
        yield _MeshSlot(
            "Free surface zone mesh",
            free_surface_zone.mesh_file,
            free_surface_zone,
            "free_surface_zone",
        )


def iter_mesh_references(spec: DiffractionSpec) -> Iterator[MeshReference]:
    """Yield every mesh file referenced by *spec*, in solver-relevant order.

    Bodies with no mesh specified are skipped (reported separately by
    ``SpecConverter.validate``). Auxiliary entries (damping lid, control
    surfaces, free-surface zone) are yielded only when present and carrying
    an explicit mesh file.
    """
    for slot in _iter_mesh_slots(spec):
        yield MeshReference(slot.label, slot.mesh_file)


def resolve_mesh_path(mesh_file: str, spec_dir: Path | None) -> Path:
    """Resolve a mesh reference per the spec-relative convention."""
    mesh_path = Path(mesh_file)
    if mesh_path.is_absolute() or spec_dir is None:
        return mesh_path
    return (spec_dir / mesh_path).resolve()


def copy_spec_meshes(
    spec: DiffractionSpec,
    output_dir: Path,
    spec_dir: Path | None = None,
) -> list[Path]:
    """Copy every existing referenced mesh into *output_dir*.

    Missing files are skipped (pre-flight validation is responsible for
    failing on those); already-copied identical files are not re-copied.
    Returns the destination paths.
    """
    copied: list[Path] = []
    for reference in iter_mesh_references(spec):
        source = resolve_mesh_path(reference.mesh_file, spec_dir)
        if source.exists():
            dest = output_dir / source.name
            if not dest.exists() or not dest.samefile(source):
                shutil.copy2(source, dest)
            copied.append(dest)
    return copied


def _check_supported_formats(spec: DiffractionSpec, solver: str) -> None:
    """Raise ValueError listing every unsupported mesh format, before any write."""
    ready = _SOLVER_READY_EXTENSIONS[solver]
    unsupported = [
        f"{slot.label}: '{slot.mesh_file}' "
        f"({Path(slot.mesh_file).suffix.lower() or 'no extension'})"
        for slot in _iter_mesh_slots(spec)
        if Path(slot.mesh_file).suffix.lower()
        not in (
            ready | _CONVERTIBLE_EXTENSIONS | _slot_passthrough_extensions(slot)
        )
    ]
    if unsupported:
        raise ValueError(
            f"Unsupported mesh format(s) for solver '{solver}':\n  "
            + "\n  ".join(unsupported)
            + f"\nSupported formats: solver-ready {sorted(ready)}, "
            f"convertible via MeshPipeline {sorted(_CONVERTIBLE_EXTENSIONS)}, "
            f"passthrough {sorted(_PASSTHROUGH_EXTENSIONS)} "
            f"(control surfaces additionally "
            f"{sorted(_CONTROL_SURFACE_PASSTHROUGH_EXTENSIONS)})."
        )


class _DestinationNames:
    """Unique packaged filenames per distinct source (#606).

    Packaged files are named by source basename (converted meshes by stem +
    solver extension), so two references with the same basename in different
    directories would silently overwrite each other and every spec slot would
    end up pointing at the last-copied file. Distinct sources claiming the
    same name get a deterministic ``_2``, ``_3``, ... suffix instead; repeat
    references to the same source reuse their name.
    """

    def __init__(self) -> None:
        self._claimed: dict[str, Path] = {}

    def claim(self, base_name: str, source_resolved: Path) -> str:
        candidate = base_name
        counter = 2
        while True:
            previous = self._claimed.get(candidate)
            if previous is None or previous == source_resolved:
                self._claimed[candidate] = source_resolved
                return candidate
            stem = Path(base_name).stem
            suffix = Path(base_name).suffix
            candidate = f"{stem}_{counter}{suffix}"
            counter += 1


def _set_mesh_reference(owner: Any, packaged_name: str, target_format: str) -> None:
    """Point a spec object's mesh reference at the packaged file."""
    owner.mesh_file = packaged_name
    if not hasattr(owner, "mesh_format"):
        return
    current_format = owner.mesh_format
    if current_format is None:
        # ControlSurfaceSpec: format is optional (None = infer from file).
        # Record the packaged format explicitly so the backend emits the
        # matching BodyControlSurfaceMeshFormat (#609).
        owner.mesh_format = target_format
    else:
        # GeometrySpec uses a (str-based) MeshFormatType enum; lid/zone specs
        # use plain str — reconstruct with the original type either way.
        owner.mesh_format = type(current_format)(target_format)


def package_spec_meshes(
    spec: DiffractionSpec,
    output_dir: Path,
    spec_dir: Path | None = None,
    solver: str = "orcawave",
    quality_gates: bool = True,
) -> tuple[DiffractionSpec, list[Path], list[str]]:
    """Prepare every referenced mesh for *solver* into *output_dir* (#606).

    Solver-ready formats (e.g. ``.gdf`` for OrcaWave) and solver-native
    passthrough formats (``.fdf``) are copied unchanged — no conversion
    churn. Convertible formats (``.dat``, ``.stl``) are converted to the
    solver target via ``MeshPipeline.prepare_for_solver``. Unsupported
    formats raise ``ValueError`` with the supported-format list before
    anything is written. Missing files are skipped — existence is the
    pre-flight's responsibility (#500). Distinct sources that would package
    to the same filename get deterministic ``_2``/``_3`` suffixes (and their
    spec references rewritten accordingly) instead of silently overwriting
    each other.

    With *quality_gates* enabled (#608), every packaged panel mesh runs the
    geometry quality checks: a FAIL verdict raises ``MeshQualityError`` and
    a machine-readable ``mesh_quality_report.json`` is written next to the
    packaged meshes. Non-blocking warnings are returned for the caller to
    surface.

    Conversions are recorded in ``mesh_provenance.json`` next to the
    packaged meshes (source path, formats, units/symmetry where the spec
    carries them).

    Returns
    -------
    tuple[DiffractionSpec, list[Path], list[str]]
        A deep copy of *spec* whose mesh references (and formats) point at
        the packaged files, the packaged file paths, and non-blocking
        quality warnings.
    """
    solver = solver.lower()
    if solver not in _SOLVER_READY_EXTENSIONS:
        raise ValueError(
            f"Unknown solver '{solver}'. "
            f"Choose from: {', '.join(sorted(_SOLVER_READY_EXTENSIONS))}"
        )

    _check_supported_formats(spec, solver)
    ready = _SOLVER_READY_EXTENSIONS[solver]

    packaged_spec = spec.model_copy(deep=True)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    pipeline = None
    packaged: list[Path] = []
    provenance: list[dict[str, Any]] = []
    gate_results: list[Any] = []
    dest_names = _DestinationNames()
    convert_dir = output_dir / "_mesh_convert_tmp"

    try:
        for slot in _iter_mesh_slots(packaged_spec):
            source = resolve_mesh_path(slot.mesh_file, spec_dir)
            if not source.exists():
                continue
            extension = source.suffix.lower()
            source_resolved = source.resolve()
            if extension in ready or extension in _slot_passthrough_extensions(
                slot
            ):
                dest = output_dir / dest_names.claim(source.name, source_resolved)
                if not dest.exists() or not dest.samefile(source):
                    shutil.copy2(source, dest)
            else:
                if pipeline is None:
                    from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import (
                        MeshPipeline,
                    )

                    pipeline = MeshPipeline()
                # Convert into a staging dir, then move to the (collision-free)
                # final name: converting straight into output_dir could clobber
                # an already-packaged mesh with the same stem (#606).
                convert_dir.mkdir(parents=True, exist_ok=True)
                converted = pipeline.prepare_for_solver(
                    source, solver, convert_dir
                )
                dest = output_dir / dest_names.claim(
                    converted.name, source_resolved
                )
                shutil.move(str(converted), str(dest))
                record: dict[str, Any] = {
                    "reference": slot.label,
                    "source": str(source),
                    "source_format": extension.lstrip("."),
                    "packaged": dest.name,
                    "target_format": dest.suffix.lstrip("."),
                }
                units = getattr(slot.owner, "length_units", None)
                if units is not None:
                    record["length_units"] = str(units)
                symmetry = getattr(slot.owner, "symmetry", None)
                if symmetry is not None:
                    record["symmetry"] = str(
                        getattr(symmetry, "value", symmetry)
                    )
                provenance.append(record)
            _set_mesh_reference(slot.owner, dest.name, dest.suffix.lstrip("."))
            packaged.append(dest)

            if quality_gates:
                from digitalmodel.hydrodynamics.diffraction.quality_gates import (
                    run_mesh_quality_gate,
                )

                gate_results.append(
                    run_mesh_quality_gate(dest, label=slot.label)
                )
    finally:
        shutil.rmtree(convert_dir, ignore_errors=True)

    if provenance:
        (output_dir / PROVENANCE_FILENAME).write_text(
            json.dumps(provenance, indent=2)
        )

    quality_warnings: list[str] = []
    if quality_gates and gate_results:
        from digitalmodel.hydrodynamics.diffraction.quality_gates import (
            enforce_quality_gates,
        )

        quality_warnings = enforce_quality_gates(gate_results, output_dir)

    return packaged_spec, packaged, quality_warnings
