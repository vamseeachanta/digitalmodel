# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Declared-band DF3 alongside the unchanged historical 3.50 rad/s checks."""

import importlib.util
import math
from itertools import combinations
from pathlib import Path

import numpy as np


def module(name):
    """Load the adjacent offline utilities without a licensed dependency."""
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(f"{name}.py"))
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


def maximum_span(panels):
    """Measure every vertex pair from serialized mesh coordinates."""
    panels = np.asarray(panels, dtype=float)
    if (panels.ndim != 3 or panels.shape[1:] != (4, 3) or not len(panels)
            or not np.isfinite(panels).all()):
        raise ValueError("Expected nonempty finite GDF panels")
    return np.max(np.stack([np.linalg.norm(panels[:, a] - panels[:, b], axis=1)
                            for a, b in combinations(range(4), 2)]), axis=0)


def valid_band(max_span_m, *, depth, panels_per_wave=6, gravity=9.80665):
    """Invert lambda/rule using omega²=g*k*tanh(k*h), at the stated depth.

    The six-panel default is an ASSUMED study rule. Rounding is toward the
    supported side so the existing dispersion checker passes the endpoint.
    """
    if any(not math.isfinite(x) or x <= 0
           for x in (max_span_m, depth, panels_per_wave, gravity)):
        raise ValueError("Span, depth, gravity and panel rule must be finite and positive")
    k = 2 * math.pi / (max_span_m * panels_per_wave)
    omega = math.sqrt(gravity * k * math.tanh(k * depth))
    check = module("check_mesh")
    while check.panel_span_limit(omega, depth, gravity, panels_per_wave) < max_span_m:
        omega = math.nextafter(omega, 0)
    return omega


def df3_declared(panels, upper_rad_s, *, depth, panels_per_wave=6, gravity=9.80665):
    """PASS only when every panel supports the declared upper frequency."""
    spans = maximum_span(panels)
    limit = module("check_mesh").panel_span_limit(upper_rad_s, depth, gravity, panels_per_wave)
    oversized = int(np.count_nonzero(spans > limit))
    return {"pass": oversized == 0, "mode": "declared_band", "panels": len(spans),
            "upper_rad_s": upper_rad_s, "depth_m": depth,
            "panels_per_wavelength": panels_per_wave, "rule_status": "ASSUMED study criterion",
            "max_span_m": float(max(spans)), "limit_m": limit,
            "oversized_panels": oversized}
