# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Complex diffraction export through an explicit, solver-independent adapter.

The adapter will convert native units to the stated SI units and will declare
its phase and coordinate conventions; no licensed module is imported here.
"""

import copy
import hashlib
import json
from pathlib import Path
from typing import Protocol

import numpy as np

COMPLEX_FIELDS = ("motion", "load_haskind", "load_diffraction")
REAL_FIELDS = ("omega_rad_s", "headings_deg", "added_mass", "damping")


class ResultAdapter(Protocol):
    """Boundary for every native API call, with no calculation entry point."""

    def snapshot(self, origin_m):
        """Return detached arrays and explicit conventions in SI units."""
        ...

    def save_native(self, path: Path):
        """Save the already calculated native result at the requested path."""
        ...

    def reload_native(self, path: Path) -> "ResultAdapter":
        """Return a fresh adapter loaded only from the saved native result."""
        ...


def _validated(snapshot, origin_m):
    try:
        data = {key: np.array(snapshot[key], copy=True) for key in REAL_FIELDS + COMPLEX_FIELDS}
        metadata = copy.deepcopy(snapshot["metadata"])
        json.dumps(metadata, allow_nan=False)
        origin = np.asarray(origin_m, dtype=float)
        if origin.shape != (3,) or not np.isfinite(origin).all():
            raise ValueError("Invalid reference origin")
        if metadata["axes"] != ["surge", "sway", "heave", "roll", "pitch", "yaw"]:
            raise ValueError("Six motion axes must be declared in order")
        if not np.array_equal(metadata["origin_m"], origin):
            raise ValueError("Result origin differs from the requested origin")
        wave_origin = np.asarray(metadata["wave_reference_origin_m"], dtype=float)
        if wave_origin.shape != (3,) or not np.isfinite(wave_origin).all():
            raise ValueError("Invalid wave phase reference")
        for key in ("coordinate_frame", "heading_convention"):
            if not isinstance(metadata[key], str) or not metadata[key].strip():
                raise ValueError("Coordinate and heading conventions must be declared")
        sign = metadata["phase_time_sign"]
        if sign not in (-1, 1):
            raise ValueError("Unknown harmonic time sign")
        phase = (f"Re{{RAO exp({'+' if sign == 1 else '-'}i omega t)}} per unit incident "
                 "wave amplitude at wave_reference_origin_m")
        if metadata["phase_convention"] != phase:
            raise ValueError("Phase convention must agree with the time sign and wave reference")
        if (metadata["motion_units"] != ["m/m"] * 3 + ["rad/m"] * 3
                or metadata["load_units"] != ["N/m"] * 3 + ["N*m/m"] * 3):
            raise ValueError("Adapter must provide the declared SI response units")
        for key, array in data.items():
            if not np.isfinite(array).all():
                raise ValueError(f"Nonfinite results: {key}")
        omega, headings = data["omega_rad_s"], data["headings_deg"]
        for array in (omega, headings):
            if array.ndim != 1 or not len(array) or len(np.unique(array)) != len(array):
                raise ValueError("Frequency and heading axes must be nonempty and unique")
        if np.any(omega <= 0) or any(np.iscomplexobj(data[key]) for key in REAL_FIELDS):
            raise ValueError("Expected positive real frequencies and real coefficient arrays")
        for key in ("added_mass", "damping"):
            if data[key].shape != (len(omega), 6, 6):
                raise ValueError("Coefficient axes must be frequency, dof, dof")
        for key in COMPLEX_FIELDS:
            if data[key].shape != (len(headings), len(omega), 6) or not np.iscomplexobj(data[key]):
                raise ValueError("Expected complex heading, frequency, dof responses")
    except (KeyError, TypeError, OverflowError) as error:
        raise ValueError("Incomplete or invalid result snapshot") from error
    data["metadata"] = metadata
    return data


def export_verified(adapter: ResultAdapter, native_path, output_path, *, origin_m):
    """Publish complex JSON only after all native arrays and metadata reload exactly."""
    native_path, output_path = Path(native_path), Path(output_path)
    if native_path.exists() or output_path.exists() or native_path == output_path:
        raise ValueError("Existing results will not be overwritten")
    before = _validated(adapter.snapshot(origin_m), origin_m)
    adapter.save_native(native_path)
    loaded = adapter.reload_native(native_path)
    try:
        after = _validated(loaded.snapshot(origin_m), origin_m)
    except ValueError as error:
        raise ValueError("Native reload produced an invalid snapshot") from error
    if (before["metadata"] != after["metadata"]
            or any(not np.array_equal(before[key], after[key]) for key in REAL_FIELDS + COMPLEX_FIELDS)):
        raise ValueError("Native reload differs in arrays, axes, origins, units or phase")
    record = {"schema_version": 2, "native_reload_verified": True,
              "native_sha256": hashlib.sha256(native_path.read_bytes()).hexdigest(),
              "array_axes": ["heading", "frequency", "dof"],
              "coefficient_axes": ["frequency", "dof", "dof"],
              "coefficient_units": "Aij: kg*m**r; Bij: kg*m**r/s; r counts rotational row and column axes",
              "metadata": before["metadata"]}
    record.update({key: before[key].tolist() for key in REAL_FIELDS})
    record.update({key: {"real": before[key].real.tolist(), "imag": before[key].imag.tolist()}
                   for key in COMPLEX_FIELDS})
    serialized = json.dumps(record, indent=2, allow_nan=False) + "\n"
    with output_path.open("x", encoding="utf-8") as output:
        output.write(serialized)
    if json.loads(output_path.read_text()) != record:
        raise ValueError("Export readback differs")
    return record
