# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Conditional omitted-tail variance bounds, never extrapolated response.

For the supplied gamma=1 seas, F(w)=exp[-(5/4)*(wp/w)**4] is the
normalised wave-variance CDF and total variance is Hs²/16. No measured
or interpolated RAO is evaluated outside its supplied frequency band.
A last-window maximum is a declared envelope assumption, not evidence
that an unresolved high-frequency resonance cannot exceed that envelope.
"""

import hashlib
import importlib.util
import json
import math
from pathlib import Path

import numpy as np

AXES = ["surge", "sway", "heave", "roll", "pitch", "yaw"]
OUTPUTS = AXES + ["hangoff_x", "hangoff_y", "hangoff_z"]
FRAME = "Right handed; z up; x along the reference axis"
HEADINGS = "Propagation direction, counterclockwise from positive x"


def _export_arrays(record):
    if (record.get("schema_version") != 2 or record.get("native_reload_verified") is not True
            or record.get("array_axes") != ["heading", "frequency", "dof"]
            or record.get("coefficient_axes") != ["frequency", "dof", "dof"]):
        raise ValueError("A reload-verified v2 complex export is required")
    spec = importlib.util.spec_from_file_location("complex_export_v2", Path(__file__).with_name("complex_export_v2.py"))
    adapter = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(adapter)
    try:
        snapshot = {key: record[key] for key in adapter.REAL_FIELDS}
        for key in adapter.COMPLEX_FIELDS:
            real, imag = (np.asarray(record[key][component]) for component in ("real", "imag"))
            if real.shape != imag.shape:
                raise ValueError("Complex component shape mismatch")
            snapshot[key] = real + 1j * imag
        snapshot["metadata"] = record["metadata"]
        data = adapter._validated(snapshot, record["metadata"]["origin_m"])
    except (KeyError, TypeError) as error:
        raise ValueError("Incomplete complex export") from error
    if (data["metadata"]["coordinate_frame"] != FRAME
            or data["metadata"]["heading_convention"] != HEADINGS):
        raise ValueError("Unsupported coordinate or heading convention")
    if np.any(np.diff(data["omega_rad_s"]) <= 0):
        raise ValueError("Frequencies must be strictly increasing")
    return data


def _wave_cdf(omega, a):
    return np.exp(-a / np.asarray(omega)**4)


def _integrate(omega, responses, a):
    """Integrate the piecewise-linear complex RAO against the analytic spectrum.

    Gaussian quadrature uses the unmodified gamma=1 density dF/dw; no
    interval weights or spectrum values are rescaled to force agreement.
    """
    nodes, weights = np.polynomial.legendre.leggauss(24)
    half_step = np.diff(omega)[:, None] / 2
    sample_w = omega[:-1, None] + (nodes + 1) * half_step
    blend = ((nodes + 1) / 2)[None, :, None]
    response = responses[:-1, None, :] * (1 - blend) + responses[1:, None, :] * blend
    density = np.exp(math.log(4 * a) - 5 * np.log(sample_w) - a / sample_w**4)
    return np.sum(abs(response)**2 * weights[None, :, None]
                  * (density * half_step)[:, :, None], axis=(0, 1))


def _sea_result(sea, omega, responses, envelope, lower_envelope, threshold):
    try:
        hs, tp, gamma, occurrence = (sea[key] for key in ("hs_m", "tp_s", "gamma", "annual_fraction"))
        if (any(not math.isfinite(v) for v in (hs, tp, gamma, occurrence))
                or hs <= 0 or tp <= 0 or gamma != 1 or not 0 <= occurrence <= 1):
            raise ValueError("Only positive gamma=1 seas with annual fractions are supported")
        identity = sea["id"]
        if not isinstance(identity, str) or not identity.strip():
            raise ValueError("A sea identity is required")
    except (KeyError, TypeError) as error:
        raise ValueError("Invalid fatigue sea") from error
    a = 1.25 * (2 * math.pi / tp)**4
    m0 = hs**2 / 16
    low = float(_wave_cdf(omega[0], a))
    high = -math.expm1(-a / omega[-1]**4)
    variances = m0 * _integrate(omega, responses, a)
    bounds = m0 * (high * envelope**2 + low * lower_envelope**2)
    outputs = {}
    for i, name in enumerate(OUTPUTS):
        ratio = float(bounds[i] / variances[i]) if variances[i] > 0 else (0.0 if bounds[i] == 0 else None)
        outputs[name] = {"in_band_variance": float(variances[i]),
                         "omitted_variance_upper_bound": float(bounds[i]),
                         "bound_to_in_band_ratio": ratio,
                         "threshold_pass": ratio is not None and ratio <= threshold,
                         "variance_unit": "rad2" if name in AXES[3:] else "m2"}
    return {"id": identity, "annual_fraction": occurrence, "wave_variance_m2": m0,
            "wave_variance_fraction_omitted": high + low,
            "upper_wave_fraction_omitted": high, "lower_wave_fraction_omitted": low,
            "lower_log_wave_fraction": -a / omega[0]**4, "outputs": outputs}


def response_bound(record, seas, *, band_upper_rad_s, heading_deg=0,
                   threshold=0.01, envelope_window_rad_s=0.5, envelope_multiplier=1):
    """Return a conditional UPPER BOUND for each sea and nine response outputs.

    The high-tail envelope is multiplier times the maximum magnitude in the
    last window, including its interpolated lower boundary. The low-tail
    envelope is multiplier times the full-band maximum. Both require review
    of their validity outside the measured band. Numerical threshold passes
    never grant release; the default 1% of in-band variance is ASSUMED.
    """
    if (any(not math.isfinite(v) or v <= 0 for v in
            (threshold, envelope_window_rad_s, envelope_multiplier, band_upper_rad_s))
            or envelope_multiplier < 1):
        raise ValueError("Invalid threshold, band or envelope parameters")
    data = _export_arrays(record)
    omega = data["omega_rad_s"]
    if (len(omega) < 2 or omega[-1] != band_upper_rad_s or omega[0] != 0.05
            or max(np.diff(omega)) > 0.01 + 1e-12
            or omega[-1] - omega[0] < envelope_window_rad_s):
        raise ValueError("Export must cover the declared 0.05-to-limit band at most every 0.01 rad/s")
    matches = np.flatnonzero(data["headings_deg"] == heading_deg)
    if len(matches) != 1:
        raise ValueError("Unsupported heading; heading interpolation is not permitted")
    motion = data["motion"][matches[0]]
    offset = np.array([0, 0, -10]) - np.asarray(data["metadata"]["origin_m"])
    point = motion[:, :3] + np.cross(motion[:, 3:], offset)
    responses = np.concatenate([motion, point], axis=1)
    boundary = omega[-1] - envelope_window_rad_s
    interpolated = np.array([complex(np.interp(boundary, omega, col.real),
                                    np.interp(boundary, omega, col.imag)) for col in responses.T])
    envelope = envelope_multiplier * np.maximum(abs(interpolated),
                                                np.max(abs(responses[omega >= boundary]), axis=0))
    lower = envelope_multiplier * np.max(abs(responses), axis=0)
    # Triangle inequality: measured translation/rotation cancellation cannot
    # reduce the point envelope outside the measured band.
    rotation_map = np.abs(np.cross(np.eye(3), offset).T)
    envelope[6:] = envelope[:3] + rotation_map @ envelope[3:6]
    lower[6:] = lower[:3] + rotation_map @ lower[3:6]
    if not seas:
        raise ValueError("Nonempty unique fatigue seas are required")
    results = [_sea_result(sea, omega, responses, envelope, lower, threshold) for sea in seas]
    if len({result["id"] for result in results}) != len(results):
        raise ValueError("Nonempty unique fatigue seas are required")
    return {"schema_version": 3, "heading_deg": heading_deg, "band_rad_s": [float(omega[0]), float(omega[-1])],
            "hangoff_m": [0, 0, -10], "metadata": data["metadata"], "renormalised": False,
            "export_record_sha256": hashlib.sha256(json.dumps(record, sort_keys=True).encode()).hexdigest(),
            "threshold": threshold, "threshold_status": "ASSUMED for quantitative review",
            "envelope_status": "ASSUMED_NOT_VALIDATED_ABOVE_BAND", "release_eligible": False,
            "envelope_window_rad_s": envelope_window_rad_s, "envelope_multiplier": envelope_multiplier,
            "upper_envelope_magnitudes": dict(zip(OUTPUTS, envelope.tolist())),
            "lower_envelope_magnitudes": dict(zip(OUTPUTS, lower.tolist())), "seas": results}
