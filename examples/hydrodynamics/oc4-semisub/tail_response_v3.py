# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Conditional omitted-tail response and fatigue bounds, never response.

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


def _integrate(omega, responses, a, moment=0):
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
                  * (sample_w**moment * density * half_step)[:, :, None], axis=(0, 1))


def wave_tail_moments(lower, upper, a):
    """Return exact normalized (m0, m2) below/above the measured band.

    Substitution x=a/w**4 gives the second moment sqrt(pi*a)*erf(sqrt(x))
    above the endpoint, and erfc below it. No tail cutoff or rescaling is used.
    """
    scale = math.sqrt(math.pi * a)
    return ((math.exp(-a / lower**4), scale * math.erfc(math.sqrt(a) / lower**2)),
            (-math.expm1(-a / upper**4), scale * math.erf(math.sqrt(a) / upper**2)))


def _tail_metrics(m0, m2, tail0, tail2, slope, threshold):
    total0, total2 = m0 + tail0, m2 + tail2
    variance_fraction = tail0 / total0 if total0 else 0.0
    # D is proportional to m0**(m/2)*sqrt(m2/m0)/(2*pi).
    # A slope >= 1 makes this monotone in both bounded moments.
    if not total0 or not total2:
        damage_fraction = 0.0
    elif not m0 or not m2:
        damage_fraction = 1.0
    else:
        damage_fraction = -math.expm1(
            ((slope - 1) / 2) * math.log(m0 / total0) + 0.5 * math.log(m2 / total2))
    return {"response_variance_tail_fraction": float(variance_fraction),
            "fatigue_damage_tail_fraction": float(damage_fraction),
            "variance_status": "PASS" if variance_fraction <= threshold else "FAIL",
            "damage_status": "PASS" if damage_fraction <= threshold else "FAIL",
            "threshold_pass": bool(variance_fraction <= threshold and damage_fraction <= threshold)}


def _sea_result(sea, omega, responses, envelope, lower_envelope, threshold, slope, names):
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
    (low, low2), (high, high2) = wave_tail_moments(omega[0], omega[-1], a)
    variances = m0 * _integrate(omega, responses, a)
    moments2 = m0 * _integrate(omega, responses, a, moment=2)
    bounds = m0 * (high * envelope**2 + low * lower_envelope**2)
    bounds2 = m0 * (high2 * envelope**2 + low2 * lower_envelope**2)
    outputs = {}
    for i, name in enumerate(names):
        ratio = float(bounds[i] / variances[i]) if variances[i] > 0 else (0.0 if bounds[i] == 0 else None)
        outputs[name] = {"in_band_variance": float(variances[i]),
                         "omitted_variance_upper_bound": float(bounds[i]),
                         "bound_to_in_band_ratio": ratio,
                         "in_band_m2": float(moments2[i]),
                         "omitted_m2_upper_bound": float(bounds2[i]),
                         "variance_unit": ("MPa2" if name == "stress_range_proxy" else
                                           "rad2" if name in AXES[3:] else "m2"),
                         "m2_unit": "variance_unit/s2",
                         **_tail_metrics(variances[i], moments2[i], bounds[i], bounds2[i], slope, threshold)}
    return {"id": identity, "annual_fraction": occurrence, "wave_variance_m2": m0,
            "wave_variance_fraction_omitted": high + low,
            "upper_wave_fraction_omitted": high, "lower_wave_fraction_omitted": low,
            "lower_log_wave_fraction": -a / omega[0]**4, "outputs": outputs}


def response_bound(record, seas, *, band_upper_rad_s, heading_deg=0,
                   threshold=0.01, envelope_window_rad_s=0.5, envelope_multiplier=1,
                   sn_slope=3, stress_transfer=None):
    """Return conditional variance and narrow-band damage tail fractions.

    The high-tail envelope is multiplier times the maximum magnitude in the
    last window, including its interpolated lower boundary. The low-tail
    envelope is multiplier times the full-band maximum. Both require review
    of their validity outside the measured band. Numerical threshold passes
    never grant release; default slope 3 and 1% fractions of the augmented
    response are ASSUMED for review. Damage is D(augmented)-D(in-band),
    divided by D(augmented); it is not damage from a separate tail process.
    Optional six real stress-transfer coefficients map [m,m,m,rad,rad,rad]
    to MPa. Twice that complex stress amplitude is the stress-range proxy.
    The caller must qualify that transfer; motion proxies are not riser stress.
    """
    if (any(not math.isfinite(v) or v <= 0 for v in
            (threshold, envelope_window_rad_s, envelope_multiplier, band_upper_rad_s))
            or envelope_multiplier < 1 or threshold > 1
            or not math.isfinite(sn_slope) or sn_slope < 1):
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
    envelope[6:9] = envelope[:3] + rotation_map @ envelope[3:6]
    lower[6:9] = lower[:3] + rotation_map @ lower[3:6]
    names = list(OUTPUTS)
    transfer = None
    if stress_transfer is not None:
        raw = np.asarray(stress_transfer)
        if raw.shape != (6,) or np.iscomplexobj(raw):
            raise ValueError("Stress transfer requires six finite real coefficients")
        transfer = np.asarray(raw, dtype=float)
        if not np.isfinite(transfer).all():
            raise ValueError("Stress transfer requires six finite real coefficients")
        responses = np.column_stack([responses, 2 * (motion @ transfer)])
        # Preserve a componentwise bound even when measured stresses cancel.
        envelope = np.append(envelope, 2 * (abs(transfer) @ envelope[:6]))
        lower = np.append(lower, 2 * (abs(transfer) @ lower[:6]))
        names.append("stress_range_proxy")
    if not seas:
        raise ValueError("Nonempty unique fatigue seas are required")
    results = [_sea_result(sea, omega, responses, envelope, lower, threshold, sn_slope, names) for sea in seas]
    if len({result["id"] for result in results}) != len(results):
        raise ValueError("Nonempty unique fatigue seas are required")
    return {"schema_version": 3, "heading_deg": heading_deg, "band_rad_s": [float(omega[0]), float(omega[-1])],
            "hangoff_m": [0, 0, -10], "metadata": data["metadata"], "renormalised": False,
            "export_record_sha256": hashlib.sha256(json.dumps(record, sort_keys=True).encode()).hexdigest(),
            "threshold": threshold, "threshold_status": "ASSUMED for quantitative review",
            "fraction_denominator": "in_band_plus_tail_bound",
            "damage_basis": "NARROW_BAND_RAYLEIGH_ASSUMED_FOR_REVIEW",
            "damage_formula": "D proportional to m0^(m/2)*sqrt(m2/m0)/(2*pi)",
            "sn_slope": sn_slope, "sn_slope_status": "ASSUMED for review",
            "wide_band_correction": "NOT_IMPLEMENTED",
            "stress_transfer": None if transfer is None else transfer.tolist(),
            "stress_transfer_units": "MPa per [m,m,m,rad,rad,rad]; proxy is twice stress amplitude",
            "stress_status": "OPEN_NOT_SUPPLIED" if transfer is None else "CALLER_SUPPLIED_REQUIRES_REVIEW",
            "envelope_status": "ASSUMED_NOT_VALIDATED_ABOVE_BAND", "release_eligible": False,
            "upper_envelope_rule": "Last-window maximum times multiplier >= 1; governs over boundary-flat minimum",
            "envelope_window_rad_s": envelope_window_rad_s, "envelope_multiplier": envelope_multiplier,
            "upper_envelope_magnitudes": dict(zip(names, envelope.tolist())),
            "lower_envelope_magnitudes": dict(zip(names, lower.tolist())), "seas": results}
