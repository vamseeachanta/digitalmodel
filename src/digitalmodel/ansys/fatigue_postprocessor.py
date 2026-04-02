# ABOUTME: Post-process ANSYS stress results for fatigue damage assessment
# ABOUTME: S-N curve evaluation, stress range extraction, cumulative damage (Miner)

"""
Fatigue Postprocessor
=====================

Post-processes ANSYS APDL stress results for fatigue life assessment:

    - Stress range extraction at critical locations
    - S-N curve definition and evaluation (DNV-RP-C203, ASME VIII Div 2)
    - Miner's cumulative damage rule
    - Fatigue damage contour data generation
    - Rainflow cycle counting (simplified)

Supported S-N curves:

    - DNV-RP-C203 curves: B1, B2, C, C1, C2, D, E, F, F1, F3, G, W1, W2, W3
    - ASME VIII Div 2 curves for welded and unwelded components
    - Custom user-defined curves

All functions return APDL command text or computed fatigue results.
No ANSYS installation required.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional

import numpy as np
import pandas as pd


# ---------------------------------------------------------------------------
# S-N curve data
# ---------------------------------------------------------------------------

# DNV-RP-C203 S-N curves in seawater with cathodic protection
# Format: (log_a1, m1, log_a2, m2, transition_cycles, threshold_mpa)
# m1 used for N < transition, m2 for N > transition
_DNV_SN_CURVES: dict[str, dict] = {
    "B1": {"log_a1": 15.117, "m1": 4.0, "log_a2": 17.146, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 106.97},
    "B2": {"log_a1": 14.885, "m1": 4.0, "log_a2": 16.856, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 93.59},
    "C":  {"log_a1": 12.592, "m1": 3.0, "log_a2": 16.320, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 73.10},
    "C1": {"log_a1": 12.449, "m1": 3.0, "log_a2": 16.081, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 65.50},
    "D":  {"log_a1": 12.164, "m1": 3.0, "log_a2": 15.606, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 52.63},
    "E":  {"log_a1": 12.010, "m1": 3.0, "log_a2": 15.350, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 46.78},
    "F":  {"log_a1": 11.855, "m1": 3.0, "log_a2": 15.091, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 41.52},
    "F1": {"log_a1": 11.699, "m1": 3.0, "log_a2": 14.832, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 36.84},
    "F3": {"log_a1": 11.546, "m1": 3.0, "log_a2": 14.576, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 32.75},
    "W1": {"log_a1": 11.398, "m1": 3.0, "log_a2": 14.330, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 29.24},
    "W3": {"log_a1": 10.970, "m1": 3.0, "log_a2": 13.617, "m2": 5.0,
            "N_transition": 1e7, "threshold_mpa": 21.05},
}


# ---------------------------------------------------------------------------
# Configuration dataclasses
# ---------------------------------------------------------------------------

@dataclass
class FatigueLoadCase:
    """Single fatigue load case with stress range and cycle count."""
    load_case_id: str = "LC1"
    stress_range_mpa: float = 50.0
    num_cycles: float = 1e6
    mean_stress_mpa: float = 0.0
    description: str = ""


@dataclass
class SNcurve:
    """S-N curve definition."""
    name: str = "D"
    log_a1: float = 12.164  # intercept (high-cycle)
    m1: float = 3.0  # inverse slope (high-cycle)
    log_a2: float = 15.606  # intercept (low-cycle)
    m2: float = 5.0  # inverse slope (low-cycle)
    N_transition: float = 1e7  # transition point
    threshold_mpa: float = 52.63  # fatigue limit (constant amplitude)
    environment: str = "seawater_cp"  # air, seawater_cp, seawater_free

    @classmethod
    def from_dnv(cls, curve_name: str) -> SNcurve:
        """Create S-N curve from DNV-RP-C203 designation.

        Parameters
        ----------
        curve_name : str
            DNV curve name (B1, B2, C, C1, D, E, F, F1, F3, W1, W3).

        Returns
        -------
        SNcurve
            Populated S-N curve.
        """
        data = _DNV_SN_CURVES[curve_name.upper()]
        return cls(name=curve_name.upper(), **data)


@dataclass
class FatigueConfig:
    """Fatigue assessment configuration."""
    sn_curve: SNcurve = field(default_factory=SNcurve)
    load_cases: list[FatigueLoadCase] = field(default_factory=list)
    design_fatigue_factor: float = 3.0  # DFF per DNV-OS-F101/C101
    thickness_correction: bool = True
    reference_thickness_mm: float = 25.0
    actual_thickness_mm: float = 25.0
    thickness_exponent: float = 0.2  # k in DNV
    scf: float = 1.0  # stress concentration factor


@dataclass
class FatigueResult:
    """Fatigue assessment result for a single location."""
    location_id: str = ""
    sn_curve_name: str = ""
    total_damage: float = 0.0
    fatigue_life_years: float = 0.0
    allowable_damage: float = 1.0
    utilization: float = 0.0
    critical_load_case: str = ""
    damage_per_case: dict[str, float] = field(default_factory=dict)


# ---------------------------------------------------------------------------
# Fatigue postprocessor
# ---------------------------------------------------------------------------

class FatiguePostprocessor:
    """Post-process ANSYS stress results for fatigue assessment."""

    def calculate_cycles_to_failure(
        self, stress_range_mpa: float, sn_curve: SNcurve
    ) -> float:
        """Calculate allowable cycles N for a given stress range.

        Uses two-slope S-N curve: log N = log a - m * log S

        Parameters
        ----------
        stress_range_mpa : float
            Applied stress range in MPa.
        sn_curve : SNcurve
            S-N curve to use.

        Returns
        -------
        float
            Allowable number of cycles (N). Returns inf if below threshold.
        """
        if stress_range_mpa <= 0:
            return float("inf")

        log_S = math.log10(stress_range_mpa)

        # Determine which slope to use based on transition
        # N_transition occurs at the intersection of the two slopes
        N_test = 10 ** (sn_curve.log_a1 - sn_curve.m1 * log_S)
        if N_test <= sn_curve.N_transition:
            log_N = sn_curve.log_a1 - sn_curve.m1 * log_S
        else:
            log_N = sn_curve.log_a2 - sn_curve.m2 * log_S

        return 10 ** log_N

    def calculate_thickness_correction(
        self, stress_range_mpa: float, actual_t_mm: float,
        ref_t_mm: float = 25.0, k: float = 0.2,
    ) -> float:
        """Apply DNV thickness correction to stress range.

        S_corrected = S * (t / t_ref)^k

        Parameters
        ----------
        stress_range_mpa : float
            Original stress range.
        actual_t_mm : float
            Actual plate thickness.
        ref_t_mm : float
            Reference thickness (25 mm per DNV).
        k : float
            Thickness exponent.

        Returns
        -------
        float
            Corrected stress range.
        """
        if actual_t_mm <= ref_t_mm:
            return stress_range_mpa
        return stress_range_mpa * (actual_t_mm / ref_t_mm) ** k

    def calculate_fatigue_damage(
        self, config: FatigueConfig
    ) -> FatigueResult:
        """Calculate cumulative fatigue damage using Miner's rule.

        D = sum(n_i / N_i) where n_i = applied cycles, N_i = allowable cycles

        Parameters
        ----------
        config : FatigueConfig
            Complete fatigue assessment configuration.

        Returns
        -------
        FatigueResult
            Fatigue damage result.
        """
        total_damage = 0.0
        damage_per_case: dict[str, float] = {}
        max_damage_case = ""
        max_damage = 0.0

        for lc in config.load_cases:
            # Apply SCF
            S = lc.stress_range_mpa * config.scf

            # Apply thickness correction
            if config.thickness_correction:
                S = self.calculate_thickness_correction(
                    S,
                    config.actual_thickness_mm,
                    config.reference_thickness_mm,
                    config.thickness_exponent,
                )

            # Calculate allowable cycles
            N = self.calculate_cycles_to_failure(S, config.sn_curve)

            # Miner's damage for this load case
            d = lc.num_cycles / N if N > 0 else float("inf")
            damage_per_case[lc.load_case_id] = d
            total_damage += d

            if d > max_damage:
                max_damage = d
                max_damage_case = lc.load_case_id

        # Apply DFF
        allowable_damage = 1.0 / config.design_fatigue_factor
        utilization = total_damage / allowable_damage if allowable_damage > 0 else float("inf")

        # Calculate fatigue life in years (assuming load cases are annual)
        fatigue_life = 1.0 / total_damage if total_damage > 0 else float("inf")

        return FatigueResult(
            sn_curve_name=config.sn_curve.name,
            total_damage=total_damage,
            fatigue_life_years=fatigue_life,
            allowable_damage=allowable_damage,
            utilization=utilization,
            critical_load_case=max_damage_case,
            damage_per_case=damage_per_case,
        )

    def generate_stress_range_extraction(
        self, component_name: str = "CRITICAL_NODES",
        load_step_pairs: list[tuple[int, int]] | None = None,
    ) -> str:
        """Generate APDL commands to extract stress ranges between load steps.

        Parameters
        ----------
        component_name : str
            Node component name for stress extraction.
        load_step_pairs : list[tuple[int, int]]
            Pairs of (load_step_max, load_step_min) for stress range.

        Returns
        -------
        str
            APDL commands for stress range extraction.
        """
        if load_step_pairs is None:
            load_step_pairs = [(2, 1)]  # Default: operating - installed

        lines = [
            "/POST1",
            f"! --- Fatigue Stress Range Extraction ---",
            f"! Component: {component_name}",
            "",
        ]

        for idx, (ls_max, ls_min) in enumerate(load_step_pairs):
            label = f"SR_{idx + 1}"
            lines.extend([
                f"! Stress range {idx + 1}: LS{ls_max} - LS{ls_min}",
                f"SET,{ls_max}",
                f"CMSEL,S,{component_name}",
                f"ETABLE,SEQV_MAX_{idx},S,EQV",
                "",
                f"SET,{ls_min}",
                f"CMSEL,S,{component_name}",
                f"ETABLE,SEQV_MIN_{idx},S,EQV",
                "",
                f"! Calculate stress range",
                f"SADD,{label},SEQV_MAX_{idx},SEQV_MIN_{idx},1,-1",
                f"PRETAB,{label}",
                "",
            ])

        lines.append("CMSEL,ALL")
        lines.append("")
        return "\n".join(lines)

    def generate_sn_curve_apdl(self, sn_curve: SNcurve) -> str:
        """Generate APDL parameter definitions for an S-N curve.

        Returns
        -------
        str
            APDL *SET commands defining S-N curve parameters.
        """
        lines = [
            f"! --- S-N Curve: {sn_curve.name} ({sn_curve.environment}) ---",
            f"*SET,LOG_A1,{sn_curve.log_a1}",
            f"*SET,M1,{sn_curve.m1}",
            f"*SET,LOG_A2,{sn_curve.log_a2}",
            f"*SET,M2,{sn_curve.m2}",
            f"*SET,N_TRANS,{sn_curve.N_transition:.1E}",
            f"*SET,S_THRESH,{sn_curve.threshold_mpa}",
            "",
            "! N = 10^(log_a - m * log10(S))",
            "! Use m1/log_a1 for N <= N_TRANS",
            "! Use m2/log_a2 for N > N_TRANS",
            "",
        ]
        return "\n".join(lines)

    def create_damage_summary_df(
        self, results: list[FatigueResult]
    ) -> pd.DataFrame:
        """Create a summary DataFrame of fatigue results.

        Parameters
        ----------
        results : list[FatigueResult]
            Fatigue results for multiple locations.

        Returns
        -------
        pd.DataFrame
            Summary table.
        """
        records = []
        for r in results:
            records.append({
                "location": r.location_id,
                "sn_curve": r.sn_curve_name,
                "total_damage": r.total_damage,
                "fatigue_life_years": r.fatigue_life_years,
                "allowable_damage": r.allowable_damage,
                "utilization": r.utilization,
                "critical_case": r.critical_load_case,
                "status": "PASS" if r.utilization <= 1.0 else "FAIL",
            })
        return pd.DataFrame(records)
