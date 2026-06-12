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


def _iter_mesh_slots(spec: DiffractionSpec) -> Iterator[_MeshSlot]:
    bodies = spec.get_bodies()

    for body in bodies:
        geometry = body.vessel.geometry
        if geometry.mesh_file and geometry.mesh_file.strip():
            yield _MeshSlot(
                f"Body '{body.vessel.name}' mesh", geometry.mesh_file, geometry
            )

    damping_lid = getattr(spec, "damping_lid", None)
    if damping_lid is not None and damping_lid.mesh_file:
        yield _MeshSlot("Damping lid mesh", damping_lid.mesh_file, damping_lid)

    for body in bodies:
        control_surface = getattr(body.vessel, "control_surface", None)
        if control_surface is not None and control_surface.mesh_file:
            yield _MeshSlot(
                f"Body '{body.vessel.name}' control surface mesh",
                control_surface.mesh_file,
                control_surface,
            )

    free_surface_zone = getattr(spec, "free_surface_zone", None)
    if free_surface_zone is not None and free_surface_zone.mesh_file:
        yield _MeshSlot(
            "Free surface zone mesh",
            free_surface_zone.mesh_file,
            free_surface_zone,
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
    accepted = ready | _PASSTHROUGH_EXTENSIONS | _CONVERTIBLE_EXTENSIONS
    unsupported = [
        f"{slot.label}: '{slot.mesh_file}' "
        f"({Path(slot.mesh_file).suffix.lower() or 'no extension'})"
        for slot in _iter_mesh_slots(spec)
        if Path(slot.mesh_file).suffix.lower() not in accepted
    ]
    if unsupported:
        raise ValueError(
            f"Unsupported mesh format(s) for solver '{solver}':\n  "
            + "\n  ".join(unsupported)
            + f"\nSupported formats: solver-ready {sorted(ready)}, "
            f"convertible via MeshPipeline {sorted(_CONVERTIBLE_EXTENSIONS)}, "
            f"passthrough {sorted(_PASSTHROUGH_EXTENSIONS)}."
        )


def _set_mesh_reference(owner: Any, packaged_name: str, target_format: str) -> None:
    """Point a spec object's mesh reference at the packaged file."""
    owner.mesh_file = packaged_name
    current_format = getattr(owner, "mesh_format", None)
    if current_format is not None:
        # GeometrySpec uses a (str-based) MeshFormatType enum; lid/zone specs
        # use plain str — reconstruct with the original type either way.
        owner.mesh_format = type(current_format)(target_format)


def package_spec_meshes(
    spec: DiffractionSpec,
    output_dir: Path,
    spec_dir: Path | None = None,
    solver: str = "orcawave",
) -> tuple[DiffractionSpec, list[Path]]:
    """Prepare every referenced mesh for *solver* into *output_dir* (#606).

    Solver-ready formats (e.g. ``.gdf`` for OrcaWave) and solver-native
    passthrough formats (``.fdf``) are copied unchanged — no conversion
    churn. Convertible formats (``.dat``, ``.stl``) are converted to the
    solver target via ``MeshPipeline.prepare_for_solver``. Unsupported
    formats raise ``ValueError`` with the supported-format list before
    anything is written. Missing files are skipped — existence is the
    pre-flight's responsibility (#500).

    Conversions are recorded in ``mesh_provenance.json`` next to the
    packaged meshes (source path, formats, units/symmetry where the spec
    carries them).

    Returns
    -------
    tuple[DiffractionSpec, list[Path]]
        A deep copy of *spec* whose mesh references (and formats) point at
        the packaged files, and the packaged file paths.
    """
    solver = solver.lower()
    if solver not in _SOLVER_READY_EXTENSIONS:
        raise ValueError(
            f"Unknown solver '{solver}'. "
            f"Choose from: {', '.join(sorted(_SOLVER_READY_EXTENSIONS))}"
        )

    _check_supported_formats(spec, solver)

    packaged_spec = spec.model_copy(deep=True)
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    ready = _SOLVER_READY_EXTENSIONS[solver]

    pipeline = None
    packaged: list[Path] = []
    provenance: list[dict[str, Any]] = []

    for slot in _iter_mesh_slots(packaged_spec):
        source = resolve_mesh_path(slot.mesh_file, spec_dir)
        if not source.exists():
            continue
        extension = source.suffix.lower()
        if extension in ready or extension in _PASSTHROUGH_EXTENSIONS:
            dest = output_dir / source.name
            if not dest.exists() or not dest.samefile(source):
                shutil.copy2(source, dest)
        else:
            if pipeline is None:
                from digitalmodel.hydrodynamics.diffraction.mesh_pipeline import (
                    MeshPipeline,
                )

                pipeline = MeshPipeline()
            dest = pipeline.prepare_for_solver(source, solver, output_dir)
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

    if provenance:
        (output_dir / PROVENANCE_FILENAME).write_text(
            json.dumps(provenance, indent=2)
        )
    return packaged_spec, packaged
