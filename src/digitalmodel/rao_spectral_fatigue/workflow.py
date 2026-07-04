"""Durable workflow: spectral fatigue from a stress RAO and a wave spectrum.

The ``spectral_fatigue`` workflow takes a stress response PSD directly. In
practice a project has a stress transfer function (RAO) ``H(f)`` -- stress per
unit wave amplitude at a structural hot spot -- and a sea state described by a
parametric wave spectrum, not the stress PSD itself. This workflow closes that
gap with the standard linear (quasi-static / frequency-domain) transfer:

    S_stress(f) = |H(f)|**2 * S_wave(f)          [MPa**2 / Hz]

S_wave(f) is generated from (Hs, Tp) by the in-repo DNV-RP-C205 wave spectra
(JONSWAP default; Pierson-Moskowitz / Bretschneider), H(f) is interpolated onto
the spectral grid, and the resulting stress PSD is handed to the tested
``spectral_fatigue`` damage chain (spectral moments -> Dirlik / narrow-band /
Wirsching-Light / Benasciutti-Tovo -> DNV-RP-C203 S-N -> damage accumulated over
sea states by occurrence). This generalises the fixed-gain screening already
baked into the spectral-fatigue atlas (``parametric.generate`` uses a constant
``stress_gain_MPa_per_m``; here the gain is the frequency-dependent RAO).

Reference: DNV-RP-C205 (wave spectra), DNV-RP-F204 / DNV-RP-C203 Sec. 4
(frequency-domain fatigue from a stress transfer function).
"""

from __future__ import annotations

import math
from typing import Any

from digitalmodel.spectral_fatigue.workflow import router as spectral_fatigue_router

_SPECTRA = {"jonswap", "pierson_moskowitz", "bretschneider"}


def router(cfg: dict) -> dict:
    settings = cfg.get("rao_spectral_fatigue") or {}
    locations = _locations(settings)

    # Build a spectral_fatigue-schema config: convert each (RAO, wave spectrum)
    # into a stress PSD, then delegate to the tested damage chain.
    sf_locations: list[dict[str, Any]] = []
    for location in locations:
        location_id = str(location.get("id", location.get("name", "location")))
        rao_freq, rao_stress = _stress_rao(location, location_id)
        sea_states_out: list[dict[str, Any]] = []
        for sea_state in _sea_states(location, location_id):
            occurrence = _occurrence(sea_state, location_id)
            frequency_hz, stress_psd = _stress_psd_from_rao(
                rao_freq,
                rao_stress,
                _wave_spectrum(sea_state, location_id),
                location_id,
            )
            sea_states_out.append(
                {
                    "occurrence_fraction": occurrence,
                    "frequency_Hz": frequency_hz,
                    "stress_psd_MPa2_Hz": stress_psd,
                }
            )
        sf_locations.append({"id": location_id, "sea_states": sea_states_out})

    sf_settings = {
        "design_life_years": settings.get("design_life_years"),
        "dff": settings.get("dff"),
        "method": settings.get("method", "dirlik"),
        "sn_curve": settings.get("sn_curve"),
        "output_dir": settings.get("output_dir", "results"),
        "locations": sf_locations,
    }

    inner = dict(cfg)
    inner["basename"] = "spectral_fatigue"
    inner["spectral_fatigue"] = sf_settings
    inner.pop("rao_spectral_fatigue", None)
    inner = spectral_fatigue_router(inner)

    result = dict(inner["spectral_fatigue"])
    result["wave_to_stress_transfer"] = "S_stress(f) = |H(f)|**2 * S_wave(f)"
    cfg["rao_spectral_fatigue"] = result
    cfg["screening_status"] = inner["screening_status"]
    return cfg


def _stress_psd_from_rao(
    rao_freq: list[float],
    rao_stress: list[float],
    wave_spectrum: dict[str, Any],
    location_id: str,
) -> tuple[list[float], list[float]]:
    """S_stress(f) = |H(f)|**2 * S_wave(f), on the wave spectrum's frequency grid.

    Mirrors ``parametric.generate._spectral_fatigue_annual_damage`` exactly, with
    the constant ``stress_gain_MPa_per_m`` replaced by the interpolated RAO H(f).
    """
    import numpy as np

    from digitalmodel.hydrodynamics.wave_spectra import WaveSpectra

    kind = str(wave_spectrum.get("type", "jonswap")).strip().lower()
    if kind not in _SPECTRA:
        raise ValueError(
            f"{location_id} wave_spectrum.type must be one of {sorted(_SPECTRA)}; got {kind!r}"
        )
    hs = _positive(wave_spectrum, "Hs", f"{location_id} wave_spectrum.Hs")
    tp = _positive(wave_spectrum, "Tp", f"{location_id} wave_spectrum.Tp")

    spectra = WaveSpectra()
    if kind == "jonswap":
        gamma = float(wave_spectrum.get("gamma", 3.3))
        omega, s_omega = spectra.jonswap(hs=hs, tp=tp, gamma=gamma)
    elif kind == "pierson_moskowitz":
        omega, s_omega = spectra.pierson_moskowitz(hs=hs, tp=tp)
    else:
        omega, s_omega = spectra.bretschneider(hs=hs, tp=tp)

    f_hz = omega / (2.0 * np.pi)  # rad/s -> Hz
    s_wave_hz = s_omega * 2.0 * np.pi  # m^2*s (per rad/s) -> m^2/Hz

    rao_interp = np.interp(f_hz, rao_freq, rao_stress)  # MPa/m, clamped to RAO ends
    stress_psd = (rao_interp**2) * s_wave_hz  # MPa^2/Hz
    return f_hz.tolist(), stress_psd.tolist()


def _stress_rao(
    location: dict[str, Any], location_id: str
) -> tuple[list[float], list[float]]:
    freq = location.get("rao_frequency_Hz")
    stress = location.get("rao_stress_MPa_per_m")
    if not isinstance(freq, list) or not isinstance(stress, list):
        raise ValueError(
            f"{location_id} needs list rao_frequency_Hz and rao_stress_MPa_per_m"
        )
    if len(freq) != len(stress) or len(freq) < 2:
        raise ValueError(
            f"{location_id} rao_frequency_Hz and rao_stress_MPa_per_m must be equal-length (>=2)"
        )
    freq = [float(v) for v in freq]
    stress = [float(v) for v in stress]
    if any(b <= a for a, b in zip(freq, freq[1:])):
        raise ValueError(f"{location_id} rao_frequency_Hz must be strictly increasing")
    if any(v < 0.0 for v in stress):
        raise ValueError(f"{location_id} rao_stress_MPa_per_m must be non-negative")
    return freq, stress


def _locations(settings: dict[str, Any]) -> list[dict[str, Any]]:
    locations = settings.get("locations")
    if not isinstance(locations, list) or not locations:
        raise ValueError("rao_spectral_fatigue locations must be a non-empty list")
    return locations


def _sea_states(location: dict[str, Any], location_id: str) -> list[dict[str, Any]]:
    sea_states = location.get("sea_states")
    if not isinstance(sea_states, list) or not sea_states:
        raise ValueError(f"{location_id} sea_states must be a non-empty list")
    return sea_states


def _wave_spectrum(sea_state: dict[str, Any], location_id: str) -> dict[str, Any]:
    wave_spectrum = sea_state.get("wave_spectrum")
    if not isinstance(wave_spectrum, dict):
        raise ValueError(f"{location_id} sea_state needs a wave_spectrum mapping")
    return wave_spectrum


def _occurrence(sea_state: dict[str, Any], location_id: str) -> float:
    value = float(sea_state.get("occurrence_fraction", 1.0))
    if value <= 0.0:
        raise ValueError(f"{location_id} occurrence_fraction must be positive")
    return value


def _positive(source: dict[str, Any], name: str, label: str) -> float:
    value = source.get(name)
    if value is None:
        raise ValueError(f"{label} is required")
    value = float(value)
    if value <= 0.0 or math.isnan(value):
        raise ValueError(f"{label} must be positive")
    return value
