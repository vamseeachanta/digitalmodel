#!/usr/bin/env python3
"""
ABOUTME: Case construction for an ARBITRARY client hull (#2023). Takes the
ingestion lane's manifest plus a normalised STL, a tow speed and a rank count,
and emits a complete solvable interFoam case in which every hull-scale-
dependent quantity is DERIVED rather than inherited from a benchmark.

WHY A SEPARATE TEMPLATE TREE. ``templates/ship_resistance`` is frozen: its test
suite diffs the emitted KCS case against the shipped DTC tutorial in both
directions, against an enumerated allowlist. Parameterising it in place would
break that gate, and that gate is the reason the KCS numbers are trustworthy.
``templates/hull_resistance`` is the same tree with the four scale bombs
tokenised, and the KCS path is left exactly as it is.

The four, and what each does when it is wrong:

  setFieldsDict     ``box (-999 ...)`` clips any domain wider than 999 m. The
                    clipped region keeps alpha.water = 0, so the far field
                    starts as air and the case solves anyway.
  decomposeParDict  ``numberOfSubdomains 8`` is untokenised, so a ranks setting
                    never arrives; the mismatch is fatal AFTER meshing.
  snappyHexMeshDict ``maxGlobalCells 2000000`` stops refinement and reports
                    success, returning a mesh missing levels nobody chose.
  0.orig/{k,omega,nut}  another hull's inlet turbulence, at its condition.
"""

from __future__ import annotations

import dataclasses
import json
import math
import re
import shutil
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Optional, Sequence

from .hull_case_physics import (
    DEFAULT_CELL_SAFETY_FACTOR,
    DEFAULT_LENGTH_SCALE_FACTOR,
    DEFAULT_TURBULENCE_INTENSITY,
    DEFAULT_VISCOSITY_RATIO,
    CellBudget,
    ForceReference,
    InletTurbulence,
    TurbulenceMethod,
    decomposition_vector,
    derive_cell_budget,
    derive_force_reference,
    derive_inlet_turbulence,
    wall_normal_first_cell_height,
)
from .hull_domain import (
    Box,
    HullDomain,
    block_divisions,
    build_hull_domain,
    refinement_boxes,
)
from .hull_case_dicts import case_provenance, hull_case_tokens
from .hull_manifest import HullManifest, load_hull_manifest

__all__ = [
    "GRAVITY",
    "HullCaseConfig",
    "HullCaseDerivation",
    "build_hull_case",
    "derive_hull_case",
    "hull_case_templates_dir",
    "hull_case_tokens",
]

GRAVITY = 9.80665

#: Fresh water at 20 C. A PARAMETERISED DEFAULT, not a derivation: the manifest
#: does not state the fluid. Supply ``reynolds`` instead when the case is being
#: matched to a towing-tank condition, exactly as the KCS path does.
DEFAULT_KINEMATIC_VISCOSITY = 1.0e-6
DEFAULT_DENSITY = 998.2

#: LTS iterations and write cadence. Published practice for a calm-water
#: resistance solve of this kind is 20000-40000; also a stated default.
DEFAULT_END_TIME = 25000
DEFAULT_WRITE_INTERVAL = 2500

#: y+ the near-wall mesh is scored against in the provenance.
DEFAULT_Y_PLUS_TARGET = 50.0


@dataclass(frozen=True)
class HullCaseConfig:
    """Everything the builder needs, and nothing it can derive."""

    manifest: HullManifest
    stl_path: Path
    velocity: float
    ranks: int
    name: str = "hull_resistance"

    density: float = DEFAULT_DENSITY
    #: Supply one of these. ``reynolds`` wins, and sets nu = U Lpp / Re, which
    #: is how a case is matched to a benchmark condition rather than to a fluid.
    kinematic_viscosity: Optional[float] = None
    reynolds: Optional[float] = None

    end_time: int = DEFAULT_END_TIME
    write_interval: int = DEFAULT_WRITE_INTERVAL

    turbulence_intensity: float = DEFAULT_TURBULENCE_INTENSITY
    turbulence_method: TurbulenceMethod = TurbulenceMethod.VISCOSITY_RATIO
    eddy_viscosity_ratio: float = DEFAULT_VISCOSITY_RATIO
    length_scale_factor: float = DEFAULT_LENGTH_SCALE_FACTOR

    base_cell_size: Optional[float] = None
    max_global_cells: Optional[int] = None
    cell_safety_factor: float = DEFAULT_CELL_SAFETY_FACTOR
    y_plus_target: float = DEFAULT_Y_PLUS_TARGET
    half_domain: bool = True

    def __post_init__(self) -> None:
        if self.velocity <= 0:
            raise ValueError(f"velocity must be positive, got {self.velocity}")
        if self.ranks < 1:
            raise ValueError(f"ranks must be >= 1, got {self.ranks}")
        if self.end_time < 1:
            raise ValueError(f"end_time must be >= 1, got {self.end_time}")

    @property
    def nu(self) -> float:
        """Kinematic viscosity actually used."""
        if self.reynolds is not None:
            if self.reynolds <= 0:
                raise ValueError(f"reynolds must be positive, got {self.reynolds}")
            return self.velocity * self.manifest.lpp_m / self.reynolds
        if self.kinematic_viscosity is not None:
            return self.kinematic_viscosity
        return DEFAULT_KINEMATIC_VISCOSITY

    @property
    def froude(self) -> float:
        return self.velocity / math.sqrt(GRAVITY * self.manifest.lpp_m)

    @property
    def reynolds_number(self) -> float:
        return self.velocity * self.manifest.lpp_m / self.nu

    @property
    def stl_name(self) -> str:
        return Path(self.stl_path).name

    @property
    def emesh_name(self) -> str:
        """What ``surfaceFeatureExtract`` will write for this surface.

        Derived, never stated a second time: snappyHexMesh reads this name back
        out of ``constant/triSurface``, and a stale literal there aborts the
        mesher only after blockMesh, the extraction and six topoSet/refineMesh
        pairs have already run.
        """
        return Path(self.stl_name).with_suffix(".eMesh").name

    def replace(self, **changes: Any) -> "HullCaseConfig":
        return dataclasses.replace(self, **changes)


@dataclass(frozen=True)
class HullCaseDerivation:
    """Every derived quantity, computed once and used by both the tokens and
    the provenance so the two cannot disagree."""

    config: HullCaseConfig
    domain: HullDomain
    boxes: List[Box]
    divisions: Dict[str, int]
    budget: CellBudget
    turbulence: InletTurbulence
    force_reference: ForceReference
    decomposition: Sequence[int]
    first_cell_height: float


def derive_hull_case(config: HullCaseConfig) -> HullCaseDerivation:
    """Run every derivation. Raises before anything is written."""
    manifest = config.manifest
    domain = build_hull_domain(manifest, base_cell_size=config.base_cell_size)
    boxes = refinement_boxes(manifest, domain)
    divisions = block_divisions(domain)

    return HullCaseDerivation(
        config=config,
        domain=domain,
        boxes=boxes,
        divisions=divisions,
        budget=derive_cell_budget(
            domain,
            boxes,
            divisions,
            ranks=config.ranks,
            safety_factor=config.cell_safety_factor,
            cap=config.max_global_cells,
        ),
        turbulence=derive_inlet_turbulence(
            config.velocity,
            manifest.lpp_m,
            config.nu,
            intensity=config.turbulence_intensity,
            method=config.turbulence_method,
            viscosity_ratio=config.eddy_viscosity_ratio,
            length_scale_factor=config.length_scale_factor,
        ),
        force_reference=derive_force_reference(
            manifest,
            config.velocity,
            config.density,
            domain,
            half_domain=config.half_domain,
        ),
        decomposition=decomposition_vector(config.ranks),
        first_cell_height=wall_normal_first_cell_height(
            config.velocity, manifest.lpp_m, config.nu, y_plus=config.y_plus_target
        ),
    )


def hull_case_templates_dir() -> Path:
    """The parameterised template tree (installed package data)."""
    cand = Path(__file__).resolve().parent / "templates" / "hull_resistance"
    if not (cand / "system" / "controlDict").is_file():
        raise FileNotFoundError(f"hull_resistance templates not found at {cand}")
    return cand


def build_hull_case(
    config: HullCaseConfig, parent_dir: Path | str = "."
) -> Path:
    """Emit a complete, solvable case directory.

    Every derivation runs first, so a case that cannot be built correctly is
    never partially written.
    """
    derivation = derive_hull_case(config)
    tokens = hull_case_tokens(derivation)
    templates = hull_case_templates_dir()

    stl_source = Path(config.stl_path)
    if not stl_source.is_file():
        raise FileNotFoundError(f"hull surface not found: {stl_source}")

    case = Path(parent_dir) / config.name
    for src in sorted(templates.rglob("*")):
        if not src.is_file():
            continue
        rel = src.relative_to(templates)
        text = src.read_text()
        for key, value in tokens.items():
            text = text.replace(f"@{key}@", value)
        leftover = re.findall(r"@[A-Z0-9_]+@", text)
        if leftover:
            raise ValueError(
                f"unsubstituted token(s) {sorted(set(leftover))} in {rel}"
            )
        dst = case / rel
        dst.parent.mkdir(parents=True, exist_ok=True)
        dst.write_text(text)

    # The Allrun pipeline restores 0/ from 0.orig/ after meshing; both have to
    # exist for the runner's own structural check to pass.
    (case / "0").mkdir(exist_ok=True)
    tri_surface = case / "constant" / "triSurface"
    tri_surface.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(stl_source, tri_surface / config.stl_name)

    (case / "case_provenance.json").write_text(
        json.dumps(case_provenance(derivation), indent=2, sort_keys=True) + "\n"
    )
    return case


def build_hull_case_from_files(
    manifest_path: Path | str,
    stl_path: Path | str,
    velocity: float,
    ranks: int,
    parent_dir: Path | str = ".",
    **options: Any,
) -> Path:
    """``(hull_manifest, STL, speed, target_ranks) -> case directory``."""
    manifest = load_hull_manifest(manifest_path)
    config = HullCaseConfig(
        manifest=manifest,
        stl_path=Path(stl_path),
        velocity=velocity,
        ranks=ranks,
        **options,
    )
    return build_hull_case(config, parent_dir)
