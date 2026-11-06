# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Tabulate operation limits using the existing linear spectral envelope basis."""

from __future__ import annotations

from dataclasses import dataclass, replace
from pathlib import Path
from typing import Sequence

import numpy as np
import pandas as pd

from digitalmodel.hydrodynamics.diffraction.output_schemas import RAOComponent, RAOSet
from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra
from digitalmodel.marine_ops.envelope_numerics import (
    DOF_UNITS,
    MAX_EXTRAPOLATED_ENERGY,
    extrapolated_fraction,
    finite_grid,
    positive,
    spectrum,
    validate_component,
)
from digitalmodel.marine_ops.installation.crane_tip_motion import crane_tip_raos
from digitalmodel.marine_ops.installation.models import CraneTipConfig, Vessel
from digitalmodel.marine_ops.operation_envelope import _rao_by_heading, _significant_response

_RESPONSE_ORDER = {"motion": 0, "velocity": 1, "acceleration": 2}


@dataclass(frozen=True)
class ResponseCriterion:
    """A labelled significant single-amplitude limit at the RAO origin or a point.

    ``unit`` is m or deg for motion, m/s or deg/s for velocity, and m/s^2
    or deg/s^2 for acceleration. ``point_m`` is an optional (x, y, z) offset
    in metres using the existing crane-tip transfer convention. Point criteria
    support surge, sway and heave; rotations are evaluated at the RAO origin.
    """

    label: str
    dof: str
    max_significant: float
    unit: str
    response: str = "motion"
    point_m: tuple[float, float, float] | None = None


@dataclass(frozen=True)
class BatchOperation:
    """Named operation with caller-supplied criteria and a forecast reduction alpha."""

    key: str
    criteria: tuple[ResponseCriterion, ...]
    alpha: float = 1.0


def allowable_hs_table(
    rao_set: RAOSet,
    operations: Sequence[BatchOperation],
    *,
    tp_range_s: Sequence[float],
    headings: Sequence[float],
    spectrum_type: str = "jonswap",
    gamma: float = 3.3,
    hs_cap_m: float = 20.0,
    csv_path: str | Path | None = None,
) -> pd.DataFrame:
    """Return allowable Hs per operation, Tp and requested heading; optionally CSV.

    Reuses envelope quadrature, bounded heading selection, coverage checks and
    linear Hs scaling. Motion, velocity and acceleration use response moments
    m0, m2 and m4 respectively. The unenhanced spectrum uses the JONSWAP shape with gamma=1.
    Point RAOs use the installation module's complex geometric transfer.

    Rows retain input operation, period and heading order. Labels identify the
    minimum criterion in each cell; ties retain the first criterion. A capped
    value is a search bound. Insufficient coverage yields a null Hs. All rows
    remain unqualified screening results, independently of numerical status.

    Raises:
        ValueError: If inputs violate the envelope or criterion contracts.
    """
    tps = finite_grid(tp_range_s, "Tp", positive_values=True)
    heads = finite_grid(headings, "headings")
    if len(np.unique(tps)) != len(tps) or len(np.unique(heads % 360)) != len(heads):
        raise ValueError("Tp and heading grids must not contain duplicates")
    positive(hs_cap_m, "Hs cap")
    positive(gamma, "gamma")
    if gamma < 1:
        raise ValueError("gamma must be >= 1")
    if spectrum_type not in ("jonswap", "pierson_moskowitz"):
        raise ValueError("spectrum_type must be 'jonswap' or 'pierson_moskowitz'")
    ops = list(operations)
    if not ops:
        raise ValueError("operations must not be empty")
    keys = set()
    prepared = []
    for op in ops:
        if not op.key.strip() or op.key in keys:
            raise ValueError("blank or duplicate operation key")
        keys.add(op.key)
        positive(op.alpha, "alpha")
        if op.alpha > 1:
            raise ValueError("alpha must be <= 1")
        if not op.criteria:
            raise ValueError("operation must contain criteria")
        labels = set()
        components = []
        for criterion in op.criteria:
            if not criterion.label.strip() or criterion.label in labels:
                raise ValueError("blank or duplicate criterion label")
            labels.add(criterion.label)
            components.append(_criterion_component(rao_set, criterion))
        prepared.append((op, components))

    spectra = WaveSpectra()
    # The same wave spectrum serves every operation and heading at each Tp.
    waves = [spectrum(spectra, 1.0, tp, 1.0 if spectrum_type == "pierson_moskowitz" else gamma)
             for tp in tps]
    rows = []
    for op, components in prepared:
        for tp, (omega, density, captured) in zip(tps, waves):
            for heading in heads:
                row = _cell(op, components, omega, density, heading, hs_cap_m)
                rows.append({
                    "operation": op.key, "tp_s": float(tp), "heading_deg": float(heading),
                    **row, "alpha": op.alpha, "spectrum_type": spectrum_type,
                    "gamma": 1.0 if spectrum_type == "pierson_moskowitz" else gamma,
                    "captured_variance_fraction": captured,
                    "criterion_type": "forecast_limit",
                    "qualification_status": "NOT_EVALUATED",
                })
    frame = pd.DataFrame(rows)
    if csv_path is not None:
        frame.to_csv(csv_path, index=False)
    return frame


def _criterion_component(rao_set: RAOSet, criterion: ResponseCriterion) -> RAOComponent:
    """Validate a criterion and adapt existing point-transfer output if requested."""
    if criterion.dof not in DOF_UNITS:
        raise ValueError(f"unknown DOF: {criterion.dof}")
    if criterion.response not in _RESPONSE_ORDER:
        raise ValueError(f"unknown response: {criterion.response}")
    suffix = ("", "/s", "/s^2")[_RESPONSE_ORDER[criterion.response]]
    if criterion.unit != DOF_UNITS[criterion.dof] + suffix:
        raise ValueError(f"invalid response unit for {criterion.dof}")
    positive(criterion.max_significant, "response limit")
    component = getattr(rao_set, criterion.dof, None)
    validate_component(component, criterion.dof)
    if criterion.point_m is None:
        return component
    point = np.asarray(criterion.point_m, dtype=float)
    if point.shape != (3,) or not np.all(np.isfinite(point)):
        raise ValueError("point must contain three finite coordinates")
    if criterion.dof not in ("surge", "sway", "heave"):
        raise ValueError("point criteria require a translation DOF")
    data = {}
    for dof in DOF_UNITS:
        source = getattr(rao_set, dof, None)
        validate_component(source, dof)
        if (not np.array_equal(source.frequencies.values, component.frequencies.values)
                or not np.array_equal(source.headings.values, component.headings.values)):
            raise ValueError("point-transfer RAOs must share frequency and heading grids")
        phase = np.asarray(source.phase, dtype=float)
        if phase.shape != np.shape(source.magnitude) or not np.all(np.isfinite(phase)):
            raise ValueError("point-transfer phase must be finite and match magnitude shape")
        data[dof] = {"amplitude": np.asarray(source.magnitude), "phase": phase}
    vessel = Vessel(
        name=rao_set.vessel_name, rao_frequencies=component.frequencies.values,
        rao_headings=component.headings.values, rao_data=data,
        crane_tip=CraneTipConfig(*point),
    )
    transferred = crane_tip_raos(vessel)[criterion.dof]
    result = replace(component, magnitude=transferred["amplitude"], phase=transferred["phase"])
    validate_component(result, criterion.dof)
    return result


def _cell(op, components, omega, density, heading, hs_cap_m):
    """Apply the existing envelope scaling and coverage gate to one table cell."""
    best, governing, actual_heading, excluded = hs_cap_m, "cap", None, 0.0
    for criterion, component in zip(op.criteria, components):
        actual, amplitude = next(_rao_by_heading(component, omega, [heading]))
        # Weighting by omega^(2k) gives m0/m2/m4 through the existing response
        # integrator. The coverage gate sees the same weighted response moment.
        weighted = density * omega ** (2 * _RESPONSE_ORDER[criterion.response])
        excluded = max(excluded, extrapolated_fraction(component, omega, weighted, amplitude))
        response = _significant_response(amplitude, omega, weighted)
        if response == 0:
            continue
        allowable = op.alpha * criterion.max_significant / response  # Envelope basis B2, Hs_ref=1.
        if not np.isfinite(allowable) or allowable <= 0:
            raise ValueError("invalid forecast limit from response")
        if allowable < best:
            best, governing, actual_heading = allowable, criterion.label, actual
    status = "capped" if actual_heading is None else "evaluated"
    if excluded > MAX_EXTRAPOLATED_ENERGY:
        best, governing, actual_heading, status = None, "coverage", None, "insufficient_coverage"
    return {
        "hs_limit_m": best, "governing_criterion": governing,
        "governing_heading_deg": actual_heading, "status": status,
        "extrapolated_energy_fraction": excluded,
    }
