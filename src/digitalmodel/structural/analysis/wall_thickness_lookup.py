# ABOUTME: 50x50 moment-tension allowable lookup table generator for multi-code comparison
# ABOUTME: Generates CSV with utilisation and allowable status across M-T grid for all codes

"""50x50 Moment-Tension Allowable Lookup Table Generator.

Generates a grid of (moment, tension) combinations, runs the combined loading
check for each design code, and outputs a CSV with utilisation values.
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from pathlib import Path
from typing import List, Optional

import numpy as np
import pandas as pd

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY

logger = logging.getLogger(__name__)


@dataclass
class LookupConfig:
    """Configuration for the M-T lookup table generation."""

    geometry: PipeGeometry
    material: PipeMaterial
    internal_pressure: float = 0.0
    external_pressure: float = 0.0
    safety_class: SafetyClass = SafetyClass.MEDIUM
    codes: List[DesignCode] = field(default_factory=lambda: list(CODE_REGISTRY.keys()))
    n_moment_steps: int = 50
    n_tension_steps: int = 50


class MTLookupGenerator:
    """Generate 50x50 moment-tension utilisation lookup tables.

    For each code, computes the plastic moment (M_p) and plastic tension (S_p)
    capacities, creates a grid from 0 to M_p / S_p, and evaluates the full
    analysis at each grid point. The result is a DataFrame with columns for
    each code's utilisation and allowable status.
    """

    def __init__(self, config: LookupConfig):
        self.config = config

    def generate(self) -> pd.DataFrame:
        """Generate the M-T lookup table.

        Returns:
            DataFrame with columns: moment_kNm, tension_kN, and for each code:
            util_{code}, allowable_{code}.
        """
        cfg = self.config

        # Determine grid bounds from maximum capacities across codes
        max_M_p = 0.0
        max_S_p = 0.0
        for code in cfg.codes:
            strategy_cls = CODE_REGISTRY.get(code)
            if strategy_cls is None:
                continue
            strategy = strategy_cls()
            M_p = strategy.compute_plastic_moment(cfg.geometry, cfg.material)
            S_p = strategy.compute_plastic_tension(cfg.geometry, cfg.material)
            max_M_p = max(max_M_p, M_p)
            max_S_p = max(max_S_p, S_p)

        if max_M_p <= 0 or max_S_p <= 0:
            return pd.DataFrame()

        moments = np.linspace(0, max_M_p, cfg.n_moment_steps)
        tensions = np.linspace(0, max_S_p, cfg.n_tension_steps)

        rows = []
        factors = DesignFactors(safety_class=cfg.safety_class)

        for m_val in moments:
            for t_val in tensions:
                row = {
                    "moment_kNm": m_val / 1e3,
                    "tension_kN": t_val / 1e3,
                }

                loads = DesignLoads(
                    internal_pressure=cfg.internal_pressure,
                    external_pressure=cfg.external_pressure,
                    bending_moment=m_val,
                    effective_tension=t_val,
                )

                for code in cfg.codes:
                    strategy_cls = CODE_REGISTRY.get(code)
                    if strategy_cls is None:
                        continue

                    analyzer = WallThicknessAnalyzer(
                        geometry=cfg.geometry,
                        material=cfg.material,
                        loads=loads,
                        factors=factors,
                        code=code,
                    )
                    result = analyzer.perform_analysis()

                    code_key = code.value.replace("-", "_").replace(".", "_")
                    row[f"util_{code_key}"] = result.max_utilisation
                    row[f"allowable_{code_key}"] = result.is_safe

                rows.append(row)

        df = pd.DataFrame(rows)
        logger.info(
            "M-T lookup generated: %d rows, %d codes, grid %dx%d",
            len(df), len(cfg.codes), cfg.n_moment_steps, cfg.n_tension_steps,
        )
        return df

    def to_csv(self, output_path: str) -> str:
        """Generate the lookup table and write to CSV.

        Args:
            output_path: File path for the CSV output.

        Returns:
            The output path string.
        """
        df = self.generate()
        path = Path(output_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        df.to_csv(path, index=False, float_format="%.6f")
        logger.info("Lookup CSV written to %s", output_path)
        return str(path)
