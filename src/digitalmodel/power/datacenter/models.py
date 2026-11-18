"""Data models for data center power topology components."""

from __future__ import annotations

import math
from dataclasses import dataclass
from enum import Enum


class RedundancyScheme(Enum):
    """Declared redundancy labels for the local topology heuristic.

    These labels do not establish equipment counts or certified tier compliance.
    """

    N = "n"
    N_PLUS_1 = "n+1"
    TWO_N = "2n"
    TWO_N_PLUS_1 = "2n+1"


@dataclass
class PowerComponent:
    """Generic power distribution component in a data center path.

    Attributes
    ----------
    component_type : str
        Component category (utility, transformer, switchgear, pdu, generator).
    rated_kva : float
        Input-side apparent power capacity in kVA under the local convention.
        An equipment output nameplate rating requires conversion before use.
    efficiency : float
        Real-power output/input ratio. The apparent-power calculations assume
        equal, nonzero power factors at every input and output. The default
        represents lossless passthrough; the base class does not validate it.
    name : str
        Human-readable identifier.
    """

    component_type: str = ""
    rated_kva: float = 0.0
    efficiency: float = 1.0
    name: str = ""

    def __post_init__(self) -> None:
        if not self.name:
            self.name = f"{self.component_type}_{self.rated_kva:.0f}kVA"


@dataclass
class UPSModule(PowerComponent):
    """Uninterruptible Power Supply module.

    Construction requires a finite, positive rated_kva (local input contract).

    Attributes
    ----------
    battery_runtime_min : float
        Battery autonomy in minutes at rated load.
    bypass_enabled : bool
        Whether automatic bypass is available during maintenance.
    """

    efficiency: float = 0.95  # Local assumption: docs/agent-pack.md, section 6 S05.
    battery_runtime_min: float = 10.0
    bypass_enabled: bool = True

    def __post_init__(self) -> None:
        # Python 3.11 Library Reference, math.isfinite (NaN/infinities excluded):
        # https://docs.python.org/3.11/library/math.html#math.isfinite
        if not math.isfinite(self.rated_kva):
            raise ValueError(f"rated_kva must be finite, got {self.rated_kva}")
        # Local positive-rating contract: issue 2749, item 2 (not a design limit).
        if self.rated_kva <= 0:
            raise ValueError(f"rated_kva must be positive, got {self.rated_kva}")
        if not math.isfinite(self.efficiency) or not 0.0 < self.efficiency <= 1.0:
            raise ValueError(
                f"efficiency must be in (0, 1], got {self.efficiency}"
            )
        if not self.component_type:
            self.component_type = "ups"
        if not self.name:
            self.name = f"UPS_{self.rated_kva:.0f}kVA"

    @property
    def output_kva(self) -> float:
        """Local input-rated output estimate: rated_kva times efficiency.

        Equal, nonzero input/output power factors are assumed, so the real-power
        efficiency also scales apparent power. This is not an output nameplate
        rating. Basis: ENERGY STAR UPS Test Method (Dec-2017), section 5 A,
        equation 2; Eligibility Criteria v2.0, section 1 K (power factor).
        """
        return self.rated_kva * self.efficiency


@dataclass
class STSModule(PowerComponent):
    """Static Transfer Switch — automatic source transfer between paths.

    Construction requires a finite, positive rated_kva and a finite,
    nonnegative transfer_time_ms. Efficiency must be finite and in (0, 1]
    under the local passive-component contract (issue 2749, item 3).
    Boolean and non-real efficiencies are rejected with ValueError.

    Attributes
    ----------
    transfer_time_ms : float
        Transfer time in milliseconds; the default is a local assumption.
    efficiency : float
        Real-power output/input ratio; the inherited default is lossless.
    preferred_source : str
        Label of the preferred power source.
    alternate_source : str
        Label of the alternate power source.
    retransfer_enabled : bool
        Whether automatic retransfer to preferred source is enabled.
    """

    transfer_time_ms: float = 4.0
    preferred_source: str = "A"
    alternate_source: str = "B"
    retransfer_enabled: bool = True

    def __post_init__(self) -> None:
        # Python 3.11 Library Reference, math.isfinite (NaN/infinities excluded):
        # https://docs.python.org/3.11/library/math.html#math.isfinite
        if not math.isfinite(self.rated_kva):
            raise ValueError(f"rated_kva must be finite, got {self.rated_kva}")
        # Local positive-rating contract: issue 2749, item 2 (not a design limit).
        if self.rated_kva <= 0:
            raise ValueError(f"rated_kva must be positive, got {self.rated_kva}")
        if not math.isfinite(self.transfer_time_ms):
            raise ValueError(
                f"transfer_time_ms must be finite, got {self.transfer_time_ms}"
            )
        # Local nonnegative-time contract: issue 2749, item 2.
        if self.transfer_time_ms < 0:
            raise ValueError(
                f"transfer_time_ms must be non-negative, got {self.transfer_time_ms}"
            )
        # Local finite, passive-efficiency contract: issue 2749, item 3.
        # Bounds 0 < eta <= 1 exclude zero output and power creation.
        try:
            valid_efficiency = (
                not isinstance(self.efficiency, bool)
                and math.isfinite(self.efficiency)
                and 0.0 < self.efficiency <= 1.0
            )
        except TypeError:
            valid_efficiency = False
        if not valid_efficiency:
            raise ValueError(
                "efficiency must be a non-boolean finite real number in (0, 1], "
                f"got {self.efficiency}"
            )
        if not self.component_type:
            self.component_type = "sts"
        if not self.name:
            self.name = f"STS_{self.rated_kva:.0f}kVA"
