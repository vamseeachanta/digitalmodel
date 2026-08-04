#!/usr/bin/env python3
"""
ABOUTME: Case builders for the 2D sloshing validation cases (#639). Assembles a
free-decay or forced-roll interFoam case on disk from the shared case stack
(blockMesh + setFields partial fill + optional prescribed motion) and records the
provenance each case carries.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any, Dict

from ..partial_fill import (
    partial_fill_box,
    render_set_fields_dict_body,
)
from ..spectral_analysis import (
    GRAVITY,
)

from .sloshing_2d_config import (
    SLOSHING_FREQ_TOLERANCE,
    _CASE_DEPTH,
    SloshingForcedRollConfig,
    SloshingFreeDecayConfig,
)
from .sloshing_2d_dicts import (
    _BLOCKMESHDICT,
    _CONTROLDICT,
    _FIELD_ALPHA,
    _FIELD_P_RGH,
    _FIELD_U_MOVING,
    _FIELD_U_SLIP,
    _FVSCHEMES,
    _FVSOLUTION,
    _GRAVITY_DICT,
    _TRANSPORT,
    _TURBULENCE,
    _dynamic_mesh_dict_text,
    _hdr,
    cosine_mode_setfields_body,
    roll_moment_function_object,
)

# ---------------------------------------------------------------------------
# Case builders
# ---------------------------------------------------------------------------


def _write_common(
    case_dir: Path,
    *,
    blockmesh: str,
    control: str,
    setfields: str,
    u_field: str,
    dynamic_mesh: str | None,
    provenance: Dict[str, Any],
) -> Path:
    for sub in ("system", "constant", "0"):
        (case_dir / sub).mkdir(parents=True, exist_ok=True)
    sysd, constd, zerod = (
        case_dir / "system",
        case_dir / "constant",
        case_dir / "0",
    )
    (sysd / "blockMeshDict").write_text(_hdr("dictionary", "blockMeshDict") + blockmesh)
    (sysd / "controlDict").write_text(_hdr("dictionary", "controlDict") + control)
    (sysd / "fvSchemes").write_text(_hdr("dictionary", "fvSchemes") + _FVSCHEMES)
    (sysd / "fvSolution").write_text(_hdr("dictionary", "fvSolution") + _FVSOLUTION)
    (sysd / "setFieldsDict").write_text(
        _hdr("dictionary", "setFieldsDict") + setfields
    )
    (constd / "transportProperties").write_text(
        _hdr("dictionary", "transportProperties") + _TRANSPORT
    )
    (constd / "g").write_text(_hdr("uniformDimensionedVectorField", "g") + _GRAVITY_DICT)
    (constd / "turbulenceProperties").write_text(
        _hdr("dictionary", "turbulenceProperties") + _TURBULENCE
    )
    if dynamic_mesh is not None:
        (constd / "dynamicMeshDict").write_text(dynamic_mesh)
    (zerod / "alpha.water").write_text(_hdr("volScalarField", "alpha.water") + _FIELD_ALPHA)
    (zerod / "p_rgh").write_text(_hdr("volScalarField", "p_rgh") + _FIELD_P_RGH)
    (zerod / "U").write_text(_hdr("volVectorField", "U") + u_field)
    (case_dir / "provenance.json").write_text(json.dumps(provenance, indent=2) + "\n")
    return case_dir


def _blockmesh(config, cfg_ny: int) -> str:
    fmt = "{:.6g}".format
    return (
        _BLOCKMESHDICT
        .replace("@LX@", fmt(config.breadth))
        .replace("@LY@", fmt(config.tank_height))
        .replace("@DEPTH@", fmt(_CASE_DEPTH))
        .replace("@NX@", str(config.nx))
        .replace("@NY@", str(cfg_ny))
    )


def build_free_decay_case(
    config: SloshingFreeDecayConfig | None = None,
    parent_dir: Path | str = ".",
) -> Path:
    """Generate the free-decay first-mode sloshing-frequency validation case.

    Static tank (no mesh motion), slip walls, a first-mode cosine free-surface
    perturbation initialised by ``setFields``. Run ``blockMesh`` -> ``setFields``
    -> ``interFoam``; the ``interfaceHeight`` functionObject records the wall
    free-surface elevation for the FFT.
    """
    config = config or SloshingFreeDecayConfig()
    case_dir = Path(parent_dir) / config.name
    fmt = "{:.6g}".format

    blockmesh = _blockmesh(config, config.ny)
    control = (
        _CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@DELTAT@", fmt(config.delta_t))
        .replace("@WRITECONTROL@", "adjustableRunTime")
        .replace("@WRITEINTERVAL@", fmt(config.field_write_interval))
        .replace("@ADJUST@", "no")
        .replace("@MAXCO@", "1")
        .replace("@PROBEX@", fmt(config.probe_x))
        .replace("@PROBEZ@", fmt(0.5 * _CASE_DEPTH))
        .replace("@SAMPLEEVERY@", str(config.sample_every))
        .replace("@EXTRAFUNCTIONS@", "")
    )
    setfields = cosine_mode_setfields_body(config)
    return _write_common(
        case_dir,
        blockmesh=blockmesh,
        control=control,
        setfields=setfields,
        u_field=_FIELD_U_SLIP,
        dynamic_mesh=None,
        provenance=_free_decay_provenance(config),
    )


def build_forced_roll_case(
    config: SloshingForcedRollConfig | None = None,
    parent_dir: Path | str = ".",
    *,
    with_moment: bool = False,
    moment_write_interval: int = 1,
) -> Path:
    """Generate the SPHERIC Test 10 forced-roll validation case.

    Moving-mesh tank driven by the prescribed-motion engine (in-plane roll about
    z), flat partial fill at 18%, ``movingWallVelocity`` walls. Run
    ``blockMesh`` -> ``setFields`` -> ``interFoam`` (dynamic mesh handled by the
    ``dynamicMeshDict``).

    Args:
        config: Forced-roll configuration (defaults to SPHERIC Test 10).
        parent_dir: Parent directory for the generated case directory.
        with_moment: If True, also emit the tank roll-reaction moment ``forces``
            functionObject (#641) about the roll axis (origin = the motion's roll
            centre, axis = z). Its ``moment.dat`` z-component is the roll moment
            consumed by the fill/frequency sweep reduction.
        moment_write_interval: ``timeStep`` stride for the moment FO (1 = every
            step; a dense series improves the first-harmonic fit).
    """
    config = config or SloshingForcedRollConfig()
    case_dir = Path(parent_dir) / config.name
    fmt = "{:.6g}".format
    ny = config.ny

    extra_functions = ""
    if with_moment:
        extra_functions = roll_moment_function_object(
            config.roll_origin, write_interval=moment_write_interval
        )

    blockmesh = _blockmesh(config, ny)
    control = (
        _CONTROLDICT
        .replace("@ENDTIME@", fmt(config.end_time))
        .replace("@DELTAT@", fmt(config.delta_t))
        .replace("@WRITECONTROL@", "adjustableRunTime")
        .replace("@WRITEINTERVAL@", fmt(config.field_write_interval))
        .replace("@ADJUST@", "yes")
        .replace("@MAXCO@", "0.5")
        .replace("@PROBEX@", fmt(0.5 * config.breadth))
        .replace("@PROBEZ@", fmt(0.5 * _CASE_DEPTH))
        .replace("@SAMPLEEVERY@", "5")
        .replace("@EXTRAFUNCTIONS@", extra_functions)
    )

    # Flat partial fill snapped onto a cell face (#659).
    snap = config.fill_snap
    box_min, box_max = partial_fill_box(
        [0.0, 0.0, 0.0],
        [config.breadth, config.tank_height, _CASE_DEPTH],
        snap.fill_height,
        vertical_axis=1,
    )
    setfields = render_set_fields_dict_body(box_min, box_max)

    return _write_common(
        case_dir,
        blockmesh=blockmesh,
        control=control,
        setfields=setfields,
        u_field=_FIELD_U_MOVING,
        dynamic_mesh=_dynamic_mesh_dict_text(config),
        provenance=_forced_roll_provenance(config),
    )


# ---------------------------------------------------------------------------
# Provenance
# ---------------------------------------------------------------------------


def _free_decay_provenance(config: SloshingFreeDecayConfig) -> Dict[str, Any]:
    return {
        "validation_case": "sloshing_2d_free_decay",
        "issue": "#639",
        "gate": "measured first-mode sloshing frequency vs analytical tanh",
        "analytical_frequency_hz": config.analytical_frequency(),
        "analytical_relation": "omega_n^2 = (n*pi*g/L)*tanh(n*pi*h/L)",
        "breadth_m": config.breadth,
        "tank_height_m": config.tank_height,
        "fill_level_requested": config.fill_level,
        "fill_level_snapped": config.fill_snap.fill_level,
        "fill_depth_m": config.fill_depth,
        "perturbation_amplitude_m": config.perturbation_amplitude,
        "mesh_cells": [config.nx, config.ny, 1],
        "delta_t_s": config.delta_t,
        "end_time_s": config.end_time,
        "gravity": GRAVITY,
        "tolerance": SLOSHING_FREQ_TOLERANCE,
    }


def _forced_roll_provenance(config: SloshingForcedRollConfig) -> Dict[str, Any]:
    return {
        "validation_case": "sloshing_2d_spheric_test10",
        "issue": "#639",
        "reference": (
            "Delorme et al. (2009) Ocean Engineering 36(2) 168-178; "
            "SPHERIC Test 10 forced-roll rectangular tank"
        ),
        "breadth_m": config.breadth,
        "tank_height_m": config.tank_height,
        "fill_depth_m": config.fill_depth,
        "fill_level": config.fill_level,
        "roll_amplitude_deg": config.roll_amplitude_deg,
        "drive_period_s": config.drive_period,
        "first_mode_period_s": config.first_mode_period,
        "analytical_frequency_hz": config.analytical_frequency(),
        "roll_origin_m": list(config.roll_origin),
        "mesh_cells": [config.nx, config.ny, 1],
        "n_cycles": config.n_cycles,
        "end_time_s": config.end_time,
    }
