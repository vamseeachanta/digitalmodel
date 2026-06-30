#!/usr/bin/env python3
"""
ABOUTME: Foundational CFD validation case #1167 — 2D laminar flow over a
zero-pressure-gradient flat plate, validated against the Blasius (1908)
similarity solution. Pure analytical reference functions live here with no
OpenFOAM dependency; the case builder reuses the existing simpleFoam
(single-phase, external) path.

Source / citation
-----------------
- Blasius, H. (1908). "Grenzschichten in Flüssigkeiten mit kleiner Reibung."
  Similarity solution for the laminar boundary layer on a flat plate.
- OpenFOAM community canonical laminar-flat-plate case (simpleFoam/pimpleFoam):
  https://tariqkhamlaj.com/2018/11/27/flow-over-a-flat-plate/
  https://cfdmonkey.com/verification-of-flow-over-a-flat-plate-in-openfoam/
- Cross-reference: SU2 laminar flat plate tutorial
  https://su2code.github.io/tutorials/Laminar_Flat_Plate/

Validation gate (#1167): local skin-friction Cf(x) within 5% of 0.664/sqrt(Re_x)
and boundary-layer thickness delta99(x) within 5% of 5.0*x/sqrt(Re_x).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict

from ..case_builder import OpenFOAMCaseBuilder
from ..marine_solvers import CurrentLoadingSetup
from ..models import DomainConfig, TurbulenceModel, TurbulenceType

# Validation tolerance for the Blasius known-answer gate (5%).
BLASIUS_TOLERANCE = 0.05

# Constants of the Blasius similarity solution.
_CF_COEFF = 0.664  # local skin-friction coefficient numerator
_DELTA99_COEFF = 5.0  # 99% boundary-layer thickness numerator
_PLATE_CD_COEFF = 1.328  # one-side integrated plate drag coefficient numerator


# ---------------------------------------------------------------------------
# Pure analytical reference functions (no OpenFOAM dependency)
# ---------------------------------------------------------------------------


def blasius_cf(re_x: float) -> float:
    """Local laminar skin-friction coefficient (Blasius).

    ``Cf(Re_x) = 0.664 / sqrt(Re_x)``

    Args:
        re_x: Local Reynolds number ``U*x/nu`` (> 0).

    Returns:
        Local skin-friction coefficient.

    Raises:
        ValueError: If ``re_x`` is not positive.
    """
    if re_x <= 0.0:
        raise ValueError(f"re_x must be positive, got {re_x}")
    return _CF_COEFF / math.sqrt(re_x)


def blasius_delta99(x: float, re_x: float) -> float:
    """Boundary-layer (99%) thickness (Blasius).

    ``delta99(x) = 5.0 * x / sqrt(Re_x)``

    Args:
        x: Streamwise distance from the leading edge (m, > 0).
        re_x: Local Reynolds number ``U*x/nu`` (> 0).

    Returns:
        Boundary-layer thickness at ``x`` (m).

    Raises:
        ValueError: If ``x`` or ``re_x`` is not positive.
    """
    if x <= 0.0:
        raise ValueError(f"x must be positive, got {x}")
    if re_x <= 0.0:
        raise ValueError(f"re_x must be positive, got {re_x}")
    return _DELTA99_COEFF * x / math.sqrt(re_x)


def blasius_plate_cd(re_l: float) -> float:
    """Integrated (one-side) flat-plate drag coefficient (Blasius).

    ``Cd(Re_L) = 1.328 / sqrt(Re_L)``

    Args:
        re_l: Plate-length Reynolds number ``U*L/nu`` (> 0).

    Returns:
        Plate drag coefficient.

    Raises:
        ValueError: If ``re_l`` is not positive.
    """
    if re_l <= 0.0:
        raise ValueError(f"re_l must be positive, got {re_l}")
    return _PLATE_CD_COEFF / math.sqrt(re_l)


# ---------------------------------------------------------------------------
# Case configuration + builder
# ---------------------------------------------------------------------------


@dataclass
class FlatPlateConfig:
    """Small config for the laminar flat-plate validation case.

    The kinematic viscosity defaults to ``1e-6`` to match the single-phase
    ``transportProperties`` template (``TRANSPORT_SINGLE``) the case builder
    writes; the free-stream velocity is derived from the target ``re_l`` and
    the plate length so the generated case is self-consistent.

    Attributes:
        re_l: Target plate-length Reynolds number ``U*L/nu``.
        plate_length: Plate length L (m).
        nu: Kinematic viscosity (m^2/s); keep aligned with the template.
        name: Case directory name.
    """

    re_l: float = 1.0e5
    plate_length: float = 1.0
    nu: float = 1.0e-6
    name: str = "validation_flat_plate_blasius"

    @property
    def free_stream_velocity(self) -> float:
        """Free-stream velocity U = Re_L * nu / L (m/s)."""
        return self.re_l * self.nu / self.plate_length

    def reynolds_at(self, x: float) -> float:
        """Local Reynolds number Re_x = U*x/nu at streamwise position ``x``."""
        return self.free_stream_velocity * x / self.nu


def build_flat_plate_case(
    config: FlatPlateConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the laminar flat-plate case directory.

    Reuses :class:`CurrentLoadingSetup` (single-phase ``simpleFoam`` — the
    external-aero path in this repo) and forces a laminar closure, which is the
    correct closure for the Blasius regime. A dedicated ``EXTERNAL_AERO``
    CaseType is *not* introduced here: the routing layer owns the enum, so we
    reuse ``CaseType.CURRENT_LOADING`` to avoid a collision (see #1167). The
    block mesh emitted by :class:`OpenFOAMCaseBuilder` uses ``symmetry`` side
    patches rather than 2D ``empty`` patches; the analytical gate is solver-
    independent, and the solve is only exercised on solver-capable hosts.

    Args:
        config: Flat-plate configuration; defaults to ``FlatPlateConfig()``.
        parent_dir: Directory under which the case directory is created.

    Returns:
        Path to the generated case directory (contains ``system/``,
        ``constant/`` and ``0/``).
    """
    config = config or FlatPlateConfig()

    setup = CurrentLoadingSetup(
        steady=True,
        current_speed=config.free_stream_velocity,
        name=config.name,
    )
    case = setup.case

    # Laminar closure is mandatory for the Blasius regime.
    case.turbulence_model = TurbulenceModel(TurbulenceType.LAMINAR)

    # A flat, plate-resolving 2D-style domain: leading-edge development region
    # upstream plus the plate length downstream, one cell deep in z.
    upstream = 0.25 * config.plate_length
    height = 0.5 * config.plate_length
    case.domain = DomainConfig(
        min_coords=[-upstream, 0.0, 0.0],
        max_coords=[config.plate_length, height, 0.01 * config.plate_length],
        n_cells=[120, 60, 1],
    )

    case.metadata.update(_provenance(config))

    builder = OpenFOAMCaseBuilder(case)
    return builder.build(Path(parent_dir))


def _provenance(config: FlatPlateConfig) -> Dict[str, Any]:
    """Provenance metadata stamped into the case for traceability."""
    return {
        "validation_case": "flat_plate_blasius",
        "issue": "#1167",
        "reference": "Blasius (1908) similarity solution",
        "citations": [
            "Blasius, H. (1908), Grenzschichten in Fluessigkeiten mit "
            "kleiner Reibung",
            "https://tariqkhamlaj.com/2018/11/27/flow-over-a-flat-plate/",
            "https://cfdmonkey.com/verification-of-flow-over-a-flat-plate-in-openfoam/",
            "https://su2code.github.io/tutorials/Laminar_Flat_Plate/",
        ],
        "re_l": config.re_l,
        "plate_length_m": config.plate_length,
        "nu_m2_s": config.nu,
        "free_stream_velocity_m_s": config.free_stream_velocity,
        "tolerance": BLASIUS_TOLERANCE,
        "known_answers": {
            "cf_at_re_l": blasius_cf(config.re_l),
            "delta99_at_plate_length_m": blasius_delta99(
                config.plate_length, config.re_l
            ),
            "plate_cd": blasius_plate_cd(config.re_l),
        },
    }
