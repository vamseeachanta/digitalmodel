"""Shared constant-property records: metres, Pa, N/m³ and degree angles."""
from dataclasses import dataclass
from enum import Enum

from .cpt import CPTData, _number


class SoilType(str, Enum):
    """Supplied labels; no strength or CPT classification is inferred."""
    CLAY = "clay"
    SAND = "sand"
    UNKNOWN = "unknown"


@dataclass(frozen=True)
class SoilLayer:
    """Depths below mudline; null properties remain unknown, including weights."""
    kind: SoilType
    top_m: float
    bottom_m: float
    su_pa: float | None = None
    cohesion_pa: float | None = None
    friction_deg: float | None = None
    bulk_weight_n_m3: float | None = None
    submerged_weight_n_m3: float | None = None

    def __post_init__(self) -> None:
        object.__setattr__(self, "kind", SoilType(self.kind))
        for name in self.__dataclass_fields__:
            value = getattr(self, name)
            if name != "kind" and (value is not None or name in {"top_m", "bottom_m"}):
                object.__setattr__(self, name, _number(name, value, minimum=0))
        if self.bottom_m <= self.top_m:
            raise ValueError("bottom_m must exceed top_m")
        if self.friction_deg is not None and self.friction_deg >= 90:
            raise ValueError("friction_deg must be below 90")
        if self.su_pa is not None and (self.cohesion_pa is not None or self.friction_deg is not None):
            raise ValueError("choose undrained su_pa or drained cohesion_pa/friction_deg")


@dataclass(frozen=True)
class SoilProfile:
    """Ordered contiguous layers from zero, with an optional existing CPTData."""
    layers: tuple[SoilLayer, ...]
    cpt: CPTData | None = None

    def __post_init__(self) -> None:
        try:
            layers = tuple(self.layers)
        except TypeError as error:
            raise ValueError("layers must be a sequence of SoilLayer records") from error
        if not layers or any(not isinstance(layer, SoilLayer) for layer in layers):
            raise ValueError("layers must contain SoilLayer records")
        bottom = 0
        for layer in layers:
            if layer.top_m != bottom:
                raise ValueError("layers must be contiguous from mudline zero")
            bottom = layer.bottom_m
        if self.cpt is not None and not isinstance(self.cpt, CPTData):
            raise ValueError("cpt must be CPTData or None")
        object.__setattr__(self, "layers", layers)
