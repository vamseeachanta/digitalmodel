# ABOUTME: Ship hydrostatic data registry — principal dimensions and curves
# ABOUTME: DDG-51, FFG-7, CVN-65, AOE-6 from EN400 and public sources
"""
Ship-specific hydrostatic data tables.

Provides principal dimensions, cross curves of stability, and curves
of form for USN vessel classes. Data sourced from USNA EN400 (DDG-51)
and publicly available naval architecture references.

References:
- USNA EN400 Chapters 3-4 (DDG-51 hydrostatics)
- Jane's Fighting Ships (principal dimensions)
- NavSource Naval History (public domain vessel data)
"""

from typing import Optional

# ── Ship Registry ─────────────────────────────────────────────────

_SHIPS: dict[str, dict] = {
    "DDG-51": {
        "name": "Arleigh Burke-class destroyer",
        "hull_number": "DDG-51",
        "loa_ft": 505.0,
        "lwl_ft": 466.0,
        "beam_ft": 59.0,
        "draft_ft": 20.5,
        "displacement_lt": 8600.0,
        "full_load_lt": 9200.0,
        "kg_ft": 23.84,
        "km_ft": 27.44,
        "gm_ft": 3.60,
        "cb": 0.51,
    },
    "FFG-7": {
        "name": "Oliver Hazard Perry-class frigate",
        "hull_number": "FFG-7",
        "loa_ft": 445.0,
        "lwl_ft": 408.0,
        "beam_ft": 45.0,
        "draft_ft": 24.5,
        "displacement_lt": 4100.0,
        "full_load_lt": 4200.0,
        "kg_ft": 19.5,
        "km_ft": 22.8,
        "gm_ft": 3.30,
        "cb": 0.48,
    },
    "CVN-65": {
        "name": "Enterprise-class aircraft carrier",
        "hull_number": "CVN-65",
        "loa_ft": 1088.0,
        "lwl_ft": 1040.0,
        "beam_ft": 133.0,
        "draft_ft": 39.0,
        "displacement_lt": 75700.0,
        "full_load_lt": 93400.0,
        "kg_ft": 32.0,
        "km_ft": 39.5,
        "gm_ft": 7.50,
        "cb": 0.59,
    },
    "AOE-6": {
        "name": "Supply-class fast combat support ship",
        "hull_number": "AOE-6",
        "loa_ft": 754.0,
        "lwl_ft": 714.0,
        "beam_ft": 107.0,
        "draft_ft": 39.0,
        "displacement_lt": 48800.0,
        "full_load_lt": 49500.0,
        "kg_ft": 28.0,
        "km_ft": 35.0,
        "gm_ft": 7.00,
        "cb": 0.62,
    },
}

# ── DDG-51 Cross Curves (EN400 p.153, 8600 LT) ──────────────────

_DDG51_CROSS_CURVES = {
    "displacement_lt": 8600.0,
    "heel_angles_deg": [0, 10, 20, 30, 40, 50, 60, 70, 80, 90],
    "kn_values_ft": [
        0.0, 5.08, 10.10, 15.02, 19.67, 22.98,
        24.42, 23.73, 21.23, 17.20,
    ],
}

# ── DDG-51 Curves of Form (EN400 table, p.130) ──────────────────

_DDG51_CURVES_OF_FORM = {
    "drafts_ft": [16.0, 16.25, 18.0, 20.0, 20.75, 22.0],
    "displacement_lt": [3992, 4092, 5500, 7200, 8443, 9100],
    "tpi_lt_per_in": [33.0, 33.2, 34.5, 36.0, 36.5, 37.0],
    "lcf_ft_aft_mid": [24.03, 24.09, 23.5, 22.8, 22.5, 22.0],
}

_CROSS_CURVES: dict[str, dict] = {
    "DDG-51": _DDG51_CROSS_CURVES,
}

_CURVES_OF_FORM: dict[str, dict] = {
    "DDG-51": _DDG51_CURVES_OF_FORM,
}


# ── Public API ────────────────────────────────────────────────────

def get_ship(hull_id: str) -> Optional[dict]:
    """Get principal dimensions for a ship class."""
    return _SHIPS.get(hull_id)


def list_ships() -> list[str]:
    """List all available ship hull IDs."""
    return sorted(_SHIPS.keys())


def get_cross_curves(hull_id: str) -> Optional[dict]:
    """Get cross curves of stability for a ship class."""
    return _CROSS_CURVES.get(hull_id)


def get_curves_of_form(hull_id: str) -> Optional[dict]:
    """Get curves of form (hydrostatic curves) for a ship class."""
    return _CURVES_OF_FORM.get(hull_id)
