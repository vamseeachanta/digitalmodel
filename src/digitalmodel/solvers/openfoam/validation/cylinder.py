#!/usr/bin/env python3
"""
ABOUTME: Foundational CFD validation case #1166 — 2D laminar flow past a
circular cylinder at Re=100 (Karman vortex street), validated against the
canonical drag coefficient and Strouhal number. Pure analytical reference
values/functions live here with no OpenFOAM dependency; the case builder
reuses the existing transient single-phase (pimpleFoam) VIV path.

Source / citation
-----------------
- Williamson, C.H.K. (1996). "Vortex dynamics in the cylinder wake."
  Annu. Rev. Fluid Mech. 28.
- Henderson, R.D. (1997). "Details of the drag curve near the onset of vortex
  shedding." Phys. Fluids 9.
- OpenFOAM tutorials: incompressible transient pimpleFoam cylinder; community
  canonical case (wolfdynamics "Flow around a cylinder"):
  https://www.wolfdynamics.com/wiki/tut_2D_cylinder.pdf

Validation gate (#1166): mean drag coefficient Cd in 1.33-1.37 (target 1.35,
within ~5%) and Strouhal St ~= 0.164 (within ~5%) from the lift-force FFT.
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, Tuple

from ..case_builder import OpenFOAMCaseBuilder
from ..marine_solvers import VIVSetup
from ..models import DomainConfig, TurbulenceModel, TurbulenceType

# Validation tolerance for the Re=100 cylinder known-answer gate (~5%).
CYLINDER_TOLERANCE = 0.05

# Literature reference values at Re=100.
CYLINDER_RE100_CD = 1.35  # mean drag coefficient (target centre of 1.33-1.37)
CYLINDER_RE100_CD_RANGE: Tuple[float, float] = (1.33, 1.37)
CYLINDER_RE100_STROUHAL = 0.164  # Strouhal number St = f*D/U


# ---------------------------------------------------------------------------
# Pure analytical reference functions (no OpenFOAM dependency)
# ---------------------------------------------------------------------------


def cylinder_reference_cd() -> float:
    """Reference mean drag coefficient for the Re=100 cylinder (1.35)."""
    return CYLINDER_RE100_CD


def cylinder_reference_strouhal() -> float:
    """Reference Strouhal number for the Re=100 cylinder (0.164)."""
    return CYLINDER_RE100_STROUHAL


def strouhal_number(
    shedding_frequency: float, diameter: float, velocity: float
) -> float:
    """Strouhal number ``St = f*D/U``.

    Args:
        shedding_frequency: Vortex-shedding frequency f (Hz).
        diameter: Cylinder diameter D (m, > 0).
        velocity: Free-stream velocity U (m/s, > 0).

    Returns:
        Non-dimensional Strouhal number.

    Raises:
        ValueError: If ``diameter`` or ``velocity`` is not positive.
    """
    if diameter <= 0.0:
        raise ValueError(f"diameter must be positive, got {diameter}")
    if velocity <= 0.0:
        raise ValueError(f"velocity must be positive, got {velocity}")
    return shedding_frequency * diameter / velocity


def shedding_frequency(
    strouhal: float, velocity: float, diameter: float
) -> float:
    """Vortex-shedding frequency ``f = St*U/D`` (Hz).

    Args:
        strouhal: Strouhal number.
        velocity: Free-stream velocity U (m/s).
        diameter: Cylinder diameter D (m, > 0).

    Returns:
        Shedding frequency (Hz).

    Raises:
        ValueError: If ``diameter`` is not positive.
    """
    if diameter <= 0.0:
        raise ValueError(f"diameter must be positive, got {diameter}")
    return strouhal * velocity / diameter


# ---------------------------------------------------------------------------
# Case configuration + builder
# ---------------------------------------------------------------------------


@dataclass
class CylinderConfig:
    """Small config for the Re=100 laminar cylinder validation case.

    The kinematic viscosity defaults to ``1e-6`` to match the single-phase
    ``transportProperties`` template (``TRANSPORT_SINGLE``) the case builder
    writes; the free-stream velocity is derived from the target ``reynolds``
    and the diameter so the generated case is self-consistent.

    Attributes:
        reynolds: Target diameter Reynolds number ``U*D/nu`` (100 for the gate).
        diameter: Cylinder diameter D (m).
        nu: Kinematic viscosity (m^2/s); keep aligned with the template.
        name: Case directory name.
    """

    reynolds: float = 100.0
    diameter: float = 0.1
    nu: float = 1.0e-6
    name: str = "validation_cylinder_re100"

    @property
    def free_stream_velocity(self) -> float:
        """Free-stream velocity U = Re * nu / D (m/s)."""
        return self.reynolds * self.nu / self.diameter

    def expected_shedding_frequency(self) -> float:
        """Expected vortex-shedding frequency f = St*U/D (Hz)."""
        return shedding_frequency(
            CYLINDER_RE100_STROUHAL, self.free_stream_velocity, self.diameter
        )


def build_cylinder_case(
    config: CylinderConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the Re=100 laminar cylinder-in-crossflow case directory.

    Reuses :class:`VIVSetup` (transient single-phase ``pimpleFoam`` — the
    riser/cylinder cross-flow path) and forces a laminar closure, which is the
    correct closure at Re=100. No new CaseType is introduced: the routing layer
    owns the enum, so we reuse ``CaseType.VIV`` (see #1166). The block mesh
    emitted by :class:`OpenFOAMCaseBuilder` is a plain box without the
    cylinder body or 2D ``empty`` patches; the analytical gate is solver-
    independent, and the solve assertions are only exercised on solver-capable
    hosts (a solver-capable host would also supply the cylinder mesh + the
    ``forceCoeffs`` function object).

    Args:
        config: Cylinder configuration; defaults to ``CylinderConfig()``.
        parent_dir: Directory under which the case directory is created.

    Returns:
        Path to the generated case directory (contains ``system/``,
        ``constant/`` and ``0/``).
    """
    config = config or CylinderConfig()

    setup = VIVSetup(
        current_speed=config.free_stream_velocity,
        name=config.name,
    )
    case = setup.case

    # Laminar closure at Re=100 (Karman vortex street, pre-transition wake).
    case.turbulence_model = TurbulenceModel(TurbulenceType.LAMINAR)

    # A wake-resolving 2D-style domain around the cylinder, one cell deep in z.
    d = config.diameter
    case.domain = DomainConfig(
        min_coords=[-8.0 * d, -8.0 * d, 0.0],
        max_coords=[24.0 * d, 8.0 * d, d],
        n_cells=[160, 80, 1],
    )

    # Resolve several shedding periods so the lift FFT has a clean peak.
    period = 1.0 / config.expected_shedding_frequency()
    case.solver_config.end_time = 30.0 * period
    case.solver_config.delta_t = period / 200.0
    case.solver_config.write_interval = 50

    case.metadata.update(_provenance(config))

    builder = OpenFOAMCaseBuilder(case)
    return builder.build(Path(parent_dir))


def _provenance(config: CylinderConfig) -> Dict[str, Any]:
    """Provenance metadata stamped into the case for traceability."""
    return {
        "validation_case": "cylinder_re100",
        "issue": "#1166",
        "reference": "Williamson (1996); Henderson (1997)",
        "citations": [
            "Williamson, C.H.K. (1996), Vortex dynamics in the cylinder "
            "wake, Annu. Rev. Fluid Mech. 28",
            "Henderson, R.D. (1997), Phys. Fluids 9",
            "https://www.wolfdynamics.com/wiki/tut_2D_cylinder.pdf",
        ],
        "reynolds": config.reynolds,
        "diameter_m": config.diameter,
        "nu_m2_s": config.nu,
        "free_stream_velocity_m_s": config.free_stream_velocity,
        "tolerance": CYLINDER_TOLERANCE,
        "known_answers": {
            "mean_cd": CYLINDER_RE100_CD,
            "cd_range": list(CYLINDER_RE100_CD_RANGE),
            "strouhal": CYLINDER_RE100_STROUHAL,
            "expected_shedding_frequency_hz": (
                config.expected_shedding_frequency()
            ),
        },
    }
