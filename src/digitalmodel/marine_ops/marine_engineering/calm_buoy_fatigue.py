"""
CALM Buoy Mooring Fatigue Analysis.

Spectral fatigue for CALM buoy mooring systems using time-domain tension
histories exported from OrcaFlex.

Implements:
    - OrcaFlex CSV/XLSX tension reader (per mooring line)
    - Rainflow cycle counting per ASTM E1049
    - S-N curves per API RP 2SK (chain and wire rope in seawater + CP)
    - Miner's rule accumulated damage
    - Scatter diagram weighting (Hs/Tp bins)
    - Fatigue life and inspection interval output

References:
    - API RP 2SK: Design and Analysis of Stationkeeping Systems for FPUs
    - DNVGL-RP-C205: Environmental Conditions and Environmental Loads
    - ASTM E1049-85: Cycle Counting in Fatigue Analysis
"""

from __future__ import annotations

import math
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Optional, Tuple, Union

import numpy as np
import pandas as pd

# ---------------------------------------------------------------------------
# S-N Curve
# ---------------------------------------------------------------------------

_EFFECTIVE_TENSION_SUFFIX = "Effective Tension"
_TIME_COLUMN_KEYWORDS = ("time", "Time")


@dataclass
class SNcurve:
    """S-N curve: log10(N) = log10(K) - m * log10(S).

    Attributes:
        name: Human-readable identifier.
        K: Intercept parameter (N at S=1).
        m: Inverse slope of S-N curve.
    """

    name: str
    K: float
    m: float

    def cycles_to_failure(self, stress_range: float) -> float:
        """Return number of cycles to failure N for given stress range S.

        Formula: N = K / S^m

        Args:
            stress_range: Tension range in kN (or MPa for structural curves).

        Returns:
            Cycles to failure. Returns math.inf when stress_range == 0.
        """
        if stress_range <= 0.0:
            return math.inf
        return self.K / (stress_range ** self.m)


# API RP 2SK S-N curves (stud-link chain, seawater with cathodic protection)
CHAIN_SN_SEAWATER: SNcurve = SNcurve(
    name="API RP 2SK Chain Seawater+CP",
    K=1.57e11,
    m=3.0,
)

# API RP 2SK S-N curve for wire rope (seawater)
WIRE_ROPE_SN_SEAWATER: SNcurve = SNcurve(
    name="API RP 2SK Wire Rope Seawater",
    K=1.0e11,
    m=4.0,
)


# ---------------------------------------------------------------------------
# OrcaFlex tension reader
# ---------------------------------------------------------------------------

class OrcaFlexTensionReader:
    """Parse OrcaFlex time-domain tension export files (CSV or XLSX).

    Expected CSV format (typical OrcaFlex export):
        Time (s),Line1 Effective Tension (kN),Line2 Effective Tension (kN),...
        0.000,512.3,488.1,...
    """

    # Column name patterns used for auto-detection
    _TIME_KEYWORDS = ("time", "Time", "TIME")
    _TENSION_SUFFIX = "Effective Tension"

    def read_csv(
        self, source: Union[str, Path, "io.StringIO"]
    ) -> Dict[str, np.ndarray]:
        """Read tension time histories from a CSV file or file-like object.

        Args:
            source: Path string, Path object, or file-like object (StringIO).

        Returns:
            Dictionary mapping line name to 1-D numpy array of tension values.

        Raises:
            ValueError: If no time column is detected.
        """
        df = pd.read_csv(source)
        return self._parse_dataframe(df)

    def read_xlsx(
        self, source: Union[str, Path], sheet_name: Union[str, int] = 0
    ) -> Dict[str, np.ndarray]:
        """Read tension time histories from an Excel workbook.

        Args:
            source: Path to .xlsx file.
            sheet_name: Sheet name or index (default 0).

        Returns:
            Dictionary mapping line name to 1-D numpy array of tension values.

        Raises:
            ValueError: If no time column is detected.
        """
        df = pd.read_excel(source, sheet_name=sheet_name)
        return self._parse_dataframe(df)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    def _parse_dataframe(self, df: pd.DataFrame) -> Dict[str, np.ndarray]:
        """Extract line tensions from a DataFrame.

        Args:
            df: Raw DataFrame read from file.

        Returns:
            Dict of line_name -> tension array.

        Raises:
            ValueError: If no time column found.
        """
        columns = list(df.columns)
        time_col = self._find_time_column(columns)
        if time_col is None:
            raise ValueError(
                "No time column found. "
                "Expected a column containing 'Time' or 'time'."
            )
        result: Dict[str, np.ndarray] = {}
        for col in columns:
            if col == time_col:
                continue
            line_name = self.parse_line_name(col)
            result[line_name] = df[col].to_numpy(dtype=float)
        return result

    def _find_time_column(self, columns: list) -> Optional[str]:
        """Return the first column whose name suggests it contains time data."""
        for col in columns:
            for keyword in self._TIME_KEYWORDS:
                if keyword in col:
                    return col
        return None

    def parse_line_name(self, column_name: str) -> str:
        """Extract mooring line identifier from a column header.

        E.g. 'Line3 Effective Tension (kN)' -> 'Line3'.
        Bare column names without the descriptor pass through unchanged.

        Args:
            column_name: Raw CSV column header.

        Returns:
            Clean line identifier string.
        """
        idx = column_name.find(self._TENSION_SUFFIX)
        if idx > 0:
            return column_name[:idx].strip()
        return column_name.strip()


# ---------------------------------------------------------------------------
# Rainflow cycle counting (ASTM E1049)
# ---------------------------------------------------------------------------

class RainflowFatigue:
    """ASTM E1049 rainflow cycle counting for tension time histories.

    Implements the classic 4-point (stack-based) algorithm.  Half-cycles
    from incomplete sequences are counted with weight 0.5.
    """

    def count_cycles(
        self, tension: np.ndarray
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Extract stress/tension cycles via rainflow counting.

        Args:
            tension: 1-D array of tension values (kN).

        Returns:
            Tuple (ranges, counts):
                ranges: Unique tension ranges extracted.
                counts: Corresponding cycle counts (sum ~ n_full_cycles).
        """
        data = np.asarray(tension, dtype=float)
        if len(data) < 2:
            return np.array([]), np.array([])

        reversals = self._extract_reversals(data)
        if len(reversals) < 2:
            return np.array([]), np.array([])

        raw_cycles = self._rainflow_algorithm(reversals)
        return self._aggregate(raw_cycles)

    # ------------------------------------------------------------------
    # Internal helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _extract_reversals(data: np.ndarray) -> np.ndarray:
        """Return peaks and valleys of the time series."""
        if len(data) < 3:
            return data.copy()

        rev: list = [data[0]]
        for i in range(1, len(data) - 1):
            if (data[i] > data[i - 1] and data[i] > data[i + 1]) or (
                data[i] < data[i - 1] and data[i] < data[i + 1]
            ):
                if data[i] != rev[-1]:
                    rev.append(data[i])
        if data[-1] != rev[-1]:
            rev.append(data[-1])
        return np.array(rev, dtype=float)

    @staticmethod
    def _rainflow_algorithm(
        reversals: np.ndarray,
    ) -> list:
        """Stack-based ASTM E1049 algorithm.

        Returns list of (range, count) pairs where count is 0.5 or 1.0.
        """
        cycles: list = []
        stack: list = []

        for val in reversals:
            stack.append(float(val))
            while len(stack) >= 3:
                x_range = abs(stack[-2] - stack[-1])
                y_range = abs(stack[-3] - stack[-2])
                if x_range >= y_range:
                    if len(stack) == 3:
                        cycles.append((y_range, 0.5))
                        stack.pop(-2)
                        break
                    else:
                        cycles.append((y_range, 1.0))
                        del stack[-3:-1]
                else:
                    break

        # Residual half-cycles
        for i in range(len(stack) - 1):
            cycles.append((abs(stack[i] - stack[i + 1]), 0.5))

        return cycles

    @staticmethod
    def _aggregate(
        raw_cycles: list,
    ) -> Tuple[np.ndarray, np.ndarray]:
        """Aggregate (range, count) pairs by unique range value."""
        if not raw_cycles:
            return np.array([]), np.array([])

        acc: Dict[float, float] = {}
        for rng, cnt in raw_cycles:
            key = round(rng, 8)
            acc[key] = acc.get(key, 0.0) + cnt

        # Remove zero-range entries
        acc = {k: v for k, v in acc.items() if k > 0.0}
        if not acc:
            return np.array([]), np.array([])

        sorted_items = sorted(acc.items())
        ranges = np.array([it[0] for it in sorted_items], dtype=float)
        counts = np.array([it[1] for it in sorted_items], dtype=float)
        return ranges, counts


# ---------------------------------------------------------------------------
# Miner's rule damage accumulation
# ---------------------------------------------------------------------------

class MinersRuleDamage:
    """Linear damage accumulation per Miner's rule.

    D = sum( n_i / N_i )  where N_i = K / S_i^m.
    """

    def __init__(self, sn_curve: SNcurve) -> None:
        """Initialise with an S-N curve.

        Args:
            sn_curve: SNcurve instance defining K and m.
        """
        self.sn_curve = sn_curve

    def compute(
        self, ranges: np.ndarray, counts: np.ndarray
    ) -> float:
        """Compute cumulative Miner's damage.

        Args:
            ranges: Array of tension/stress ranges.
            counts: Array of cycle counts corresponding to each range.

        Returns:
            Scalar damage ratio D (dimensionless).
        """
        if len(ranges) == 0:
            return 0.0

        total_damage = 0.0
        for s, n in zip(ranges, counts):
            n_fail = self.sn_curve.cycles_to_failure(float(s))
            if math.isinf(n_fail):
                continue
            total_damage += float(n) / n_fail
        return total_damage


# ---------------------------------------------------------------------------
# Scatter diagram weighted fatigue
# ---------------------------------------------------------------------------

class ScatterDiagramFatigue:
    """Compute sea-state-weighted fatigue damage from a scatter diagram.

    D_total = sum_i( p_i * D_i )

    where p_i is the occurrence probability of sea state (Hs_i, Tp_i)
    and D_i is the Miner's damage computed from the tension history for
    that sea state.
    """

    _PROB_TOLERANCE = 0.02  # allow 2 % rounding in scatter table

    def __init__(self, sn_curve: SNcurve) -> None:
        """Initialise with an S-N curve.

        Args:
            sn_curve: SNcurve instance.
        """
        self.sn_curve = sn_curve
        self._rf = RainflowFatigue()
        self._miners = MinersRuleDamage(sn_curve)

    def compute(
        self,
        scatter: pd.DataFrame,
        tension_per_seastate: Dict[Tuple[float, float], Dict[str, np.ndarray]],
    ) -> Dict[str, float]:
        """Compute weighted damage per mooring line.

        Args:
            scatter: DataFrame with columns ['Hs', 'Tp', 'probability'].
            tension_per_seastate: Dict keyed by (Hs, Tp) to a dict of
                line_name -> tension_array.

        Returns:
            Dict mapping line name to total weighted damage.

        Raises:
            ValueError: If probabilities do not sum to approximately 1.0.
            KeyError: If a (Hs, Tp) bin from the scatter is absent from
                tension_per_seastate.
        """
        prob_sum = float(scatter["probability"].sum())
        if abs(prob_sum - 1.0) > self._PROB_TOLERANCE:
            raise ValueError(
                f"Scatter diagram probabilities sum to {prob_sum:.4f}; "
                "expected ~1.0."
            )

        weighted: Dict[str, float] = {}

        for _, row in scatter.iterrows():
            hs = float(row["Hs"])
            tp = float(row["Tp"])
            prob = float(row["probability"])
            key = (hs, tp)

            if key not in tension_per_seastate:
                raise KeyError(
                    f"No tension data for sea state (Hs={hs}, Tp={tp})."
                )

            line_tensions = tension_per_seastate[key]
            for line_name, tension in line_tensions.items():
                ranges, counts = self._rf.count_cycles(tension)
                damage = self._miners.compute(ranges, counts)
                weighted[line_name] = weighted.get(line_name, 0.0) + (
                    prob * damage
                )

        return weighted


# ---------------------------------------------------------------------------
# Result dataclass
# ---------------------------------------------------------------------------

@dataclass
class MooringLineResult:
    """Fatigue analysis result for a single mooring line.

    Attributes:
        line_name: Identifier string for the mooring line.
        damage: Miner's damage ratio (dimensionless).
        fatigue_life_years: Estimated fatigue life in years.
        inspection_interval_years: Recommended inspection interval.
        design_life_exceeded: True when damage ratio > 1.0.
    """

    line_name: str
    damage: float
    fatigue_life_years: float
    inspection_interval_years: float
    design_life_exceeded: bool


# ---------------------------------------------------------------------------
# Fatigue life report builder
# ---------------------------------------------------------------------------

class FatigueLifeReport:
    """Convert Miner's damage ratios to fatigue life and inspection intervals.

    Fatigue life [years] = design_life_years / damage_ratio
    Inspection interval [years] = fatigue_life / safety_factor
    """

    DEFAULT_INSPECTION_SAFETY_FACTOR = 3.0

    def __init__(
        self,
        design_life_years: float = 25.0,
        inspection_interval_safety_factor: float = DEFAULT_INSPECTION_SAFETY_FACTOR,
    ) -> None:
        """Initialise report builder.

        Args:
            design_life_years: Target design life in years.
            inspection_interval_safety_factor: Divisor applied to fatigue
                life to obtain the inspection interval.
        """
        self.design_life_years = design_life_years
        self.safety_factor = inspection_interval_safety_factor

    def build(
        self, damages: Dict[str, float]
    ) -> Dict[str, MooringLineResult]:
        """Build fatigue life report for all mooring lines.

        Args:
            damages: Dict mapping line name to Miner's damage ratio
                (computed over the design life).

        Returns:
            Dict mapping line name to MooringLineResult.
        """
        results: Dict[str, MooringLineResult] = {}
        for line_name, damage in damages.items():
            if damage <= 0.0:
                fatigue_life = math.inf
                inspection_interval = math.inf
            else:
                fatigue_life = self.design_life_years / damage
                inspection_interval = fatigue_life / self.safety_factor

            results[line_name] = MooringLineResult(
                line_name=line_name,
                damage=damage,
                fatigue_life_years=fatigue_life,
                inspection_interval_years=inspection_interval,
                design_life_exceeded=(damage > 1.0),
            )
        return results


# ---------------------------------------------------------------------------
# Top-level convenience function
# ---------------------------------------------------------------------------

def compute_fatigue_life(
    tension_csv: Optional[Union[str, Path, "io.StringIO"]],
    sn_curve: SNcurve,
    design_life_years: float = 25.0,
    inspection_interval_safety_factor: float = FatigueLifeReport.DEFAULT_INSPECTION_SAFETY_FACTOR,
    scatter_diagram: Optional[pd.DataFrame] = None,
    tension_per_seastate: Optional[
        Dict[Tuple[float, float], Dict[str, np.ndarray]]
    ] = None,
) -> Dict[str, MooringLineResult]:
    """Compute CALM buoy mooring fatigue life.

    Two operating modes:

    1. **Single sea state** — provide *tension_csv* only.  All tension
       histories in the file contribute equally (no probability weighting).

    2. **Scatter diagram** — provide *scatter_diagram* and
       *tension_per_seastate*; *tension_csv* should be None.
       Damage is weighted by sea-state occurrence probability.

    Args:
        tension_csv: Path or file-like object containing OrcaFlex tension
            export (CSV format).  Pass None when using scatter mode.
        sn_curve: S-N curve to apply (use CHAIN_SN_SEAWATER or
            WIRE_ROPE_SN_SEAWATER for API RP 2SK).
        design_life_years: Target design life in years (default 25).
        inspection_interval_safety_factor: Safety factor on fatigue life
            for inspection interval (default 3.0).
        scatter_diagram: Optional DataFrame with columns
            ['Hs', 'Tp', 'probability'].
        tension_per_seastate: Optional dict keyed by (Hs, Tp) to a dict
            of line_name -> tension_array.

    Returns:
        Dict mapping line name to MooringLineResult.

    Raises:
        ValueError: If neither tension_csv nor scatter_diagram is provided.
    """
    if tension_csv is None and scatter_diagram is None:
        raise ValueError(
            "Either tension_csv or (scatter_diagram + tension_per_seastate) "
            "must be provided."
        )

    report_builder = FatigueLifeReport(
        design_life_years=design_life_years,
        inspection_interval_safety_factor=inspection_interval_safety_factor,
    )

    if scatter_diagram is not None and tension_per_seastate is not None:
        # Scatter diagram mode
        calc = ScatterDiagramFatigue(sn_curve=sn_curve)
        damages = calc.compute(scatter_diagram, tension_per_seastate)
        return report_builder.build(damages)

    # Single sea-state mode: read CSV, run rainflow + Miner's
    reader = OrcaFlexTensionReader()
    line_tensions = reader.read_csv(tension_csv)

    rf = RainflowFatigue()
    miners = MinersRuleDamage(sn_curve=sn_curve)
    damages: Dict[str, float] = {}

    for line_name, tension in line_tensions.items():
        ranges, counts = rf.count_cycles(tension)
        damages[line_name] = miners.compute(ranges, counts)

    return report_builder.build(damages)
