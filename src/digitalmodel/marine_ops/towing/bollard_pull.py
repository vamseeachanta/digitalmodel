"""Compare supplied tug capacity with steady environmental tow demand.

``tug.bollard_pull`` already estimates capacity from installed power and
propulsion. This check accepts that estimate (converted from tonnes-force to N)
or a measured capacity; it does not duplicate the propulsion model. A PASS is
limited to force balance, without a yaw/control or dynamic-response assessment.
"""
from dataclasses import dataclass
import math

from .drift_force import DriftForceResult
from .towline_analysis import _positive


@dataclass(frozen=True)
class BollardPullResult:
    passed: bool
    required_pull_n: float
    available_pull_n: float
    margin_n: float
    reason: str


@dataclass(frozen=True)
class BollardPullCheck:
    tug_capacity: float  # N, nominal bollard pull
    env_loads: float | DriftForceResult  # N scalar resultant or vector load result
    efficiency: float = 1.0  # caller-supplied derating, dimensionless
    reserve_factor: float = 1.0  # caller-supplied demand factor, dimensionless

    def check(self) -> BollardPullResult:
        """Return demand, effective capacity and signed capacity-minus-demand (N)."""
        _positive(tug_capacity=self.tug_capacity, efficiency=self.efficiency,
                  reserve_factor=self.reserve_factor)
        if self.efficiency > 1 or self.reserve_factor < 1:
            raise ValueError("efficiency must be <= 1 and reserve_factor >= 1")
        demand = self.env_loads.resultant_n if isinstance(self.env_loads, DriftForceResult) else self.env_loads
        if not math.isfinite(demand) or demand < 0:
            raise ValueError("environmental demand must be finite and nonnegative")
        required = demand * self.reserve_factor
        available = self.tug_capacity * self.efficiency
        margin = available - required
        return BollardPullResult(margin >= 0, required, available, margin,
                                 f"required {required:.3f} N versus available {available:.3f} N")
