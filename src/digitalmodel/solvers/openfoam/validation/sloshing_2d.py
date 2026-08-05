#!/usr/bin/env python3
"""
ABOUTME: First validated 2D rectangular-tank sloshing case (#639) for
ballast-tank tuned-liquid-damper studies. Assembles a 2D interFoam VOF tank
from the existing case stack (blockMesh + setFields partial fill (#659) +
optional prescribed forced roll from the motion engine (#658)) and validates the
measured first-mode sloshing frequency against the analytical prismatic-tank
tanh dispersion relation (spectral_analysis.prismatic_tank_natural_frequency).

Two configurations
------------------
1. Free-decay (PRIMARY / hard gate). A static tank with a small first-mode
   cosine perturbation of the free surface is released and left to oscillate.
   The free-surface elevation at a wall probe is FFT'd and the fundamental
   sloshing frequency is compared to the analytical value within
   ``SLOSHING_FREQ_TOLERANCE`` (~5%). No mesh motion, slip walls (minimise
   numerical damping of the mode).

2. Forced-roll (CORROBORATION). The SPHERIC Test 10 rectangular-tank forced-roll
   benchmark (Delorme et al. 2009): breadth 0.9 m, height 0.508 m, 18% fill
   (h = 0.093 m), forced roll ~4 deg near the first sloshing mode. The tank is
   driven with the prescribed-motion engine (in-plane rotation about the
   out-of-plane z axis = the 2D section's physical roll) and the resonant
   free-surface run-up on the walls is observed. Not gated on reproducing the
   experimental pressure trace (needs the raw dataset).

2D convention
-------------
Sloshing plane is x (breadth L) - y (vertical), z is the thin out-of-plane slab
with ``empty`` front/back patches; gravity is ``(0 -9.81 0)``. The physical roll
of this section is therefore rotation about the z axis, which the motion engine
calls ``MotionType.YAW`` (Euler angle on z). Gravity stays in the global frame,
so rotating the tank reproduces the oscillating body force of the benchmark.

Source / citation
-----------------
- Analytical: linear-potential prismatic-tank dispersion
  omega_n^2 = (n*pi*g/L) * tanh(n*pi*h/L) (implemented in spectral_analysis).
- SPHERIC Test 10 / Delorme, L., Colagrossi, A., Souto-Iglesias, A.,
  Zamora-Rodriguez, R. & Botia-Vera, E. (2009). "A set of canonical problems in
  sloshing. Part I: Pressure field in forced roll - comparison between
  experimental results and SPH." Ocean Engineering 36(2), 168-178.
  Tank 0.9 m x 0.508 m; 18% fill h=0.093 m; first-mode period T1=1.9191 s
  (the analytical tanh relation reproduces T1 to 4 significant figures);
  rotation axis at the centre of the tank floor.
- Dictionaries derive from $FOAM_TUTORIALS/multiphase/interFoam/laminar/damBreak
  (ESI v2312), consistent with the verified dam-break/Kleefsman cases.
"""

from __future__ import annotations

from .sloshing_2d_config import (  # noqa: F401 - re-exported for callers
    SLOSHING_FREQ_TOLERANCE,
    SloshingForcedRollConfig,
    SloshingFreeDecayConfig,
    _CASE_DEPTH,
)

from .sloshing_2d_dicts import (  # noqa: F401 - re-exported for callers
    ROLL_MOMENT_FO_NAME,
    ROLL_MOMENT_PATCHES,
    _BLOCKMESHDICT,
    _CONTROLDICT,
    _FIELD_ALPHA,
    _FIELD_P_RGH,
    _FIELD_U_MOVING,
    _FIELD_U_SLIP,
    _FVSCHEMES,
    _FVSOLUTION,
    _GRAVITY_DICT,
    _HEADER,
    _TRANSPORT,
    _TURBULENCE,
    _dynamic_mesh_dict_text,
    _hdr,
    cosine_mode_setfields_body,
    roll_moment_function_object,
)

from .sloshing_2d_analysis import (  # noqa: F401 - re-exported for callers
    _refine_peak_parabolic,
    _variance,
    analyze_free_decay,
    measure_natural_frequency,
    parse_interface_height,
    parse_roll_moment,
)

from .sloshing_2d_case import (  # noqa: F401 - re-exported for callers
    _blockmesh,
    _forced_roll_provenance,
    _free_decay_provenance,
    _write_common,
    build_forced_roll_case,
    build_free_decay_case,
)

__all__ = [
    "ROLL_MOMENT_FO_NAME",
    "ROLL_MOMENT_PATCHES",
    "SLOSHING_FREQ_TOLERANCE",
    "SloshingForcedRollConfig",
    "SloshingFreeDecayConfig",
    "analyze_free_decay",
    "build_forced_roll_case",
    "build_free_decay_case",
    "cosine_mode_setfields_body",
    "measure_natural_frequency",
    "parse_interface_height",
    "parse_roll_moment",
    "roll_moment_function_object",
]
