"""Microgrid domain models — enums and dataclasses.

Provides the core data structures for microgrid energy management:
MicrogridMode (operating states), DERType (distributed energy resource
categories), DERAsset (individual DER configuration), and MicrogridState
(system snapshot).

References
----------
IEEE 1547.4-2011 — Guide for Design, Operation, and Integration of
Distributed Resource Island Systems with Electric Power Systems.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum


class MicrogridMode(Enum):
    """Operating mode of the microgrid.

    Attributes
    ----------
    GRID_CONNECTED : str
        Normal operation with utility interconnection.
    ISLANDED : str
        Disconnected from utility; local generation serves load.
    BLACK_START : str
        Restoration sequence from complete outage.
    """

    GRID_CONNECTED = "grid_connected"
    ISLANDED = "islanded"
    BLACK_START = "black_start"


class DERType(Enum):
    """Distributed energy resource type classification.

    Attributes
    ----------
    PV : str
        Photovoltaic solar.
    WIND : str
        Wind turbine generator.
    BESS : str
        Battery energy storage system.
    GENSET : str
        Diesel or gas generator set.
    FUEL_CELL : str
        Hydrogen or natural gas fuel cell.
    """

    PV = "pv"
    WIND = "wind"
    BESS = "bess"
    GENSET = "genset"
    FUEL_CELL = "fuel_cell"


@dataclass
class DERAsset:
    """Configuration for a single distributed energy resource.

    Parameters
    ----------
    asset_id : str
        Unique identifier for the asset.
    der_type : DERType
        Category of the DER.
    capacity_kw : float
        Rated capacity [kW]. Must be positive.
    marginal_cost : float
        Marginal cost of generation [$/kWh].
    dispatch_priority : int
        Merit-order priority (1 = highest). Must be >= 1.
    available : bool
        Whether the asset is currently available for dispatch.
    """

    asset_id: str
    der_type: DERType
    capacity_kw: float
    marginal_cost: float
    dispatch_priority: int
    available: bool = True

    def __post_init__(self) -> None:
        if self.capacity_kw <= 0:
            raise ValueError(
                f"capacity_kw must be positive, got {self.capacity_kw}"
            )
        if self.dispatch_priority < 1:
            raise ValueError(
                f"dispatch_priority must be >= 1, got {self.dispatch_priority}"
            )


@dataclass
class MicrogridState:
    """Snapshot of microgrid system state at a point in time.

    Parameters
    ----------
    mode : MicrogridMode
        Current operating mode.
    total_load_kw : float
        Total electrical load [kW].
    total_generation_kw : float
        Total DER generation [kW].
    grid_import_kw : float
        Power imported from utility grid [kW]. Negative = export.
    frequency_hz : float
        System frequency [Hz]. Must be positive.
    unserved_load_kw : float
        Load that could not be served [kW].
    curtailment_kw : float
        Generation curtailed [kW].
    """

    mode: MicrogridMode
    total_load_kw: float
    total_generation_kw: float
    grid_import_kw: float
    frequency_hz: float
    unserved_load_kw: float = 0.0
    curtailment_kw: float = 0.0

    def __post_init__(self) -> None:
        if self.frequency_hz <= 0:
            raise ValueError(
                f"frequency_hz must be positive, got {self.frequency_hz}"
            )

    @property
    def net_load_kw(self) -> float:
        """Net load = total_load - total_generation [kW]."""
        return self.total_load_kw - self.total_generation_kw
