"""Explicit unit conversions at existing consumer seams; no solver execution."""
from .cpt import CPTData, _number
from .slope_profile import validate_profile
from .soil_profile import SoilLayer, SoilProfile, SoilType
from .spudcan import SoilLayer as SpudcanLayer


def from_cpt(data: CPTData, layers: tuple[SoilLayer, ...]) -> SoilProfile:
    """Attach measurements to supplied layers without deriving properties."""
    if not isinstance(data, CPTData):
        raise ValueError("data must be CPTData")
    return SoilProfile(layers, data)


def to_cpt(profile: SoilProfile) -> CPTData:
    """Return the original measurements, retaining object identity and Pa arrays."""
    if profile.cpt is None:
        raise ValueError("cpt is missing")
    return profile.cpt


def _required(layer: SoilLayer, name: str) -> float:
    value = getattr(layer, name)
    if value is None:
        raise ValueError(f"{name} is required for this conversion")
    return value


def to_spudcan_layers(profile: SoilProfile) -> tuple[SpudcanLayer, ...]:
    """Return native layers; geometry and bearing-method settings remain external."""
    result = []
    for index, layer in enumerate(profile.layers):
        weight = _required(layer, "submerged_weight_n_m3") / 1000
        if layer.kind == SoilType.CLAY:
            strength = _required(layer, "su_pa") / 1000
            if strength <= 0:
                raise ValueError("spudcan su_pa must be positive")
            native = SpudcanLayer("clay", layer.top_m, layer.bottom_m, weight, su_kpa=strength)
        elif layer.kind == SoilType.SAND:
            angle = _required(layer, "friction_deg")
            if (layer.su_pa is not None or layer.cohesion_pa not in (None, 0)
                    or weight <= 0 or not 20 <= angle <= 40
                    or index != 0 or len(profile.layers) < 2):
                raise ValueError("unsupported spudcan sand properties or arrangement")
            native = SpudcanLayer("sand", layer.top_m, layer.bottom_m, weight, friction_deg=angle)
        else:
            raise ValueError("spudcan kind must be clay or sand")
        result.append(native)
    return tuple(result)


def to_slope_profile(profile: SoilProfile, *, x_extent_m, mudline_elevation_m,
                     water_unit_weight_n_m3, circles) -> dict:
    """Build horizontal, constant-property inputs and call native validation."""
    if not isinstance(x_extent_m, (tuple, list)) or len(x_extent_m) != 2:
        raise ValueError("x_extent_m requires two coordinates")
    xs = [_number("x_extent_m", x) for x in x_extent_m]
    datum = _number("mudline_elevation_m", mudline_elevation_m)
    water = _number("water_unit_weight_n_m3", water_unit_weight_n_m3) / 1000
    layers = []
    for index, layer in enumerate(profile.layers):
        item = dict(unit_weight_kn_m3=_required(layer, "bulk_weight_n_m3") / 1000)
        if layer.su_pa is not None:
            item["su_kpa"] = layer.su_pa / 1000
        else:
            item.update(c_kpa=_required(layer, "cohesion_pa") / 1000,
                        phi_deg=_required(layer, "friction_deg"))
        if index:
            item["top_boundary_m"] = [[x, datum - layer.top_m] for x in xs]
        layers.append(item)
    return validate_profile(dict(units="SI", ground_surface_m=[[x, datum] for x in xs],
        bottom_elevation_m=datum - profile.layers[-1].bottom_m, layers=layers,
        water_unit_weight_kn_m3=water, circles=circles))
