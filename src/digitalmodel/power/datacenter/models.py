"""Data models for data center power topology components."""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum


class RedundancyScheme(Enum):
    """Power redundancy schemes per Uptime Institute topology standards."""

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
        Rated apparent power capacity in kVA.
    efficiency : float
        Power conversion efficiency (0.0–1.0). Default 1.0 (lossless passthrough).
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

    Attributes
    ----------
    battery_runtime_min : float
        Battery autonomy in minutes at rated load.
    bypass_enabled : bool
        Whether automatic bypass is available during maintenance.
    """

    efficiency: float = 0.95  # UPS typical efficiency
    battery_runtime_min: float = 10.0
    bypass_enabled: bool = True

    def __post_init__(self) -> None:
        if self.rated_kva <= 0:
            raise ValueError(f"rated_kva must be positive, got {self.rated_kva}")
        if not 0.0 < self.efficiency <= 1.0:
            raise ValueError(
                f"efficiency must be in (0, 1], got {self.efficiency}"
            )
        if not self.component_type:
            self.component_type = "ups"
        if not self.name:
            self.name = f"UPS_{self.rated_kva:.0f}kVA"

    @property
    def output_kva(self) -> float:
        """Effective output after efficiency losses."""
        return self.rated_kva * self.efficiency


@dataclass
class STSModule(PowerComponent):
    """Static Transfer Switch — automatic source transfer between paths.

    Attributes
    ----------
    transfer_time_ms : float
        Transfer time in milliseconds (typically 4–10 ms).
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
        if self.rated_kva <= 0:
            raise ValueError(f"rated_kva must be positive, got {self.rated_kva}")
        if self.transfer_time_ms < 0:
            raise ValueError(
                f"transfer_time_ms must be non-negative, got {self.transfer_time_ms}"
            )
        if not self.component_type:
            self.component_type = "sts"
        if not self.name:
            self.name = f"STS_{self.rated_kva:.0f}kVA"
