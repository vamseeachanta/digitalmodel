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

import shutil
from pathlib import Path
from typing import Iterator, NamedTuple

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec


class MeshReference(NamedTuple):
    """A mesh file the solver will need, with a human-readable label."""

    label: str
    mesh_file: str


def iter_mesh_references(spec: DiffractionSpec) -> Iterator[MeshReference]:
    """Yield every mesh file referenced by *spec*, in solver-relevant order.

    Bodies with no mesh specified are skipped (reported separately by
    ``SpecConverter.validate``). Auxiliary entries (damping lid, control
    surfaces, free-surface zone) are yielded only when present and carrying
    an explicit mesh file.
    """
    bodies = spec.get_bodies()

    for body in bodies:
        mesh_file = body.vessel.geometry.mesh_file
        if mesh_file and mesh_file.strip():
            yield MeshReference(f"Body '{body.vessel.name}' mesh", mesh_file)

    damping_lid = getattr(spec, "damping_lid", None)
    if damping_lid is not None and damping_lid.mesh_file:
        yield MeshReference("Damping lid mesh", damping_lid.mesh_file)

    for body in bodies:
        control_surface = getattr(body.vessel, "control_surface", None)
        if control_surface is not None and control_surface.mesh_file:
            yield MeshReference(
                f"Body '{body.vessel.name}' control surface mesh",
                control_surface.mesh_file,
            )

    free_surface_zone = getattr(spec, "free_surface_zone", None)
    if free_surface_zone is not None and free_surface_zone.mesh_file:
        yield MeshReference(
            "Free surface zone mesh", free_surface_zone.mesh_file
        )


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
