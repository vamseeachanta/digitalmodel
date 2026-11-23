"""Microgrid energy management system — merit-order dispatch, mode transitions.

Implements a rule-based microgrid EMS with merit-order DER dispatch,
islanding detection integration, and black-start sequencing.

References
----------
IEEE 1547.4-2011 — Guide for Design, Operation, and Integration of
Distributed Resource Island Systems with Electric Power Systems.
"""

from __future__ import annotations

from digitalmodel.power.microgrid.bess_controller import BESSController
from digitalmodel.power.microgrid.island_detector import IslandDetector
from digitalmodel.power.microgrid.models import (
    DERAsset,
    DERType,
    MicrogridMode,
    MicrogridState,
)

BLACK_START_MIN_SOC: float = 0.30
"""Minimum BESS SOC required for black-start sequence."""


class MicrogridEMS:
    """Rule-based microgrid energy management system.

    Parameters
    ----------
    assets : list[DERAsset]
        Fleet of distributed energy resources.
    bess : BESSController
        Battery controller instance (manages the BESS asset).
    island_detector : IslandDetector
        Islanding detection instance.
    """

    def __init__(
        self,
        assets: list[DERAsset],
        bess: BESSController,
        island_detector: IslandDetector,
    ) -> None:
        self.assets = assets
        self.bess = bess
        self.island_detector = island_detector
        self.mode = MicrogridMode.GRID_CONNECTED

    def merit_order_dispatch(
        self,
        load_kw: float,
        island_mode: bool = False,
    ) -> dict[str, float]:
        """Dispatch DERs in merit order until load is met.

        Sort available DERs by (dispatch_priority, marginal_cost).
        BESS dispatch is constrained by SOC via BESSController.

        Parameters
        ----------
        load_kw : float
            Total load to serve [kW].
        island_mode : bool
            If True, BESS applies island-mode SOC reserve.

        Returns
        -------
        dict[str, float]
            Mapping of asset_id → dispatched power [kW].
        """
        sorted_assets = sorted(
            self.assets,
            key=lambda a: (a.dispatch_priority, a.marginal_cost),
        )
        remaining = load_kw
        dispatch: dict[str, float] = {}
        for asset in sorted_assets:
            if remaining <= 0:
                dispatch[asset.asset_id] = 0.0
                continue
            if not asset.available:
                dispatch[asset.asset_id] = 0.0
                continue
            if asset.der_type == DERType.BESS:
                max_avail = self.bess.max_discharge_kw(
                    island_mode=island_mode,
                )
            else:
                max_avail = asset.capacity_kw
            dispatched = min(remaining, max_avail)
            dispatch[asset.asset_id] = dispatched
            remaining -= dispatched
        return dispatch

    def update(
        self,
        load_kw: float,
        timestamps: list[float],
        frequencies: list[float],
        voltage_angle_prev: float,
        voltage_angle_curr: float,
    ) -> MicrogridState:
        """Run one control cycle: detect islanding, dispatch, update state.

        Parameters
        ----------
        load_kw : float
            Total electrical load [kW].
        timestamps : list[float]
            Recent frequency measurement timestamps [s].
        frequencies : list[float]
            Recent frequency measurements [Hz].
        voltage_angle_prev : float
            Previous voltage angle [degrees].
        voltage_angle_curr : float
            Current voltage angle [degrees].

        Returns
        -------
        MicrogridState
            Updated system state snapshot.
        """
        # Islanding detection
        detection = self.island_detector.detect(
            timestamps, frequencies, voltage_angle_prev, voltage_angle_curr,
        )
        if detection.tripped and self.mode == MicrogridMode.GRID_CONNECTED:
            self.mode = MicrogridMode.ISLANDED

        is_island = self.mode in (
            MicrogridMode.ISLANDED, MicrogridMode.BLACK_START,
        )
        dispatch = self.merit_order_dispatch(
            load_kw=load_kw, island_mode=is_island,
        )
        total_gen = sum(dispatch.values())
        unserved = max(0.0, load_kw - total_gen)
        grid_import = 0.0 if is_island else max(0.0, load_kw - total_gen)
        if not is_island:
            unserved = 0.0  # grid covers shortfall

        freq = frequencies[-1] if frequencies else 60.0
        return MicrogridState(
            mode=self.mode,
            total_load_kw=load_kw,
            total_generation_kw=total_gen,
            grid_import_kw=grid_import,
            frequency_hz=freq,
            unserved_load_kw=unserved,
        )

    def black_start(self) -> None:
        """Initiate black-start sequence.

        Requires BESS SOC >= BLACK_START_MIN_SOC.

        Raises
        ------
        ValueError
            If BESS SOC is insufficient for black start.
        """
        if self.bess.soc < BLACK_START_MIN_SOC:
            raise ValueError(
                f"SOC {self.bess.soc:.2f} below minimum "
                f"{BLACK_START_MIN_SOC} for black start"
            )
        self.mode = MicrogridMode.BLACK_START

    def reconnect(self) -> None:
        """Transition from black-start to islanded mode.

        Called after generation is stable post-black-start.
        """
        self.mode = MicrogridMode.ISLANDED
