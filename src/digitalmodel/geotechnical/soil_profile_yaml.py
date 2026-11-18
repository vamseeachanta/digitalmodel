"""Version-one SI profile text codec; safe YAML with unique, known keys."""
from dataclasses import asdict, fields

import yaml

from .cpt import CPTData
from .soil_profile import SoilLayer, SoilProfile


class _UniqueLoader(yaml.SafeLoader):
    """Keep duplicate fields from silently replacing supplied measurements."""


def _mapping(loader, node):
    result = {}
    for key_node, value_node in node.value:
        key = loader.construct_object(key_node)
        if not isinstance(key, str) or key in result:
            raise ValueError("mapping keys must be unique strings")
        result[key] = loader.construct_object(value_node)
    return result


_UniqueLoader.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, _mapping)


def _keys(data, allowed, required):
    if not isinstance(data, dict) or set(data) - set(allowed) or set(required) - set(data):
        raise ValueError("unsupported mapping keys or missing required fields")


def load_soil_profile(text: str) -> SoilProfile:
    """Read SI primitives; reconstruct measurements through the CPTData validator."""
    data = yaml.load(text, Loader=_UniqueLoader)
    _keys(data, ("schema_version", "units", "layers", "cpt"), ("schema_version", "units", "layers"))
    if type(data["schema_version"]) is not int or data["schema_version"] != 1 or data["units"] != "SI":
        raise ValueError("schema_version must be 1 and units must be SI")
    if not isinstance(data["layers"], list):
        raise ValueError("layers must be a sequence")
    layers = []
    for layer in data["layers"]:
        _keys(layer, (f.name for f in fields(SoilLayer)), ("kind", "top_m", "bottom_m"))
        layers.append(SoilLayer(**layer))
    cpt = data.get("cpt")
    if cpt is not None:
        _keys(cpt, ("depth_m", "qc_pa", "fs_pa", "u2_pa", "area_ratio"), ("depth_m", "qc_pa", "fs_pa", "area_ratio"))
        cpt = CPTData(cpt["depth_m"], cpt["qc_pa"], cpt["fs_pa"], u2=cpt.get("u2_pa"),
                      area_ratio=cpt["area_ratio"], pressure_unit="Pa")
    return SoilProfile(layers, cpt)


def dump_soil_profile(profile: SoilProfile) -> str:
    """Return deterministic Unicode/LF YAML text; file callers will use UTF-8."""
    data = asdict(profile)
    for layer in data["layers"]:
        layer["kind"] = layer["kind"].value
    return yaml.safe_dump(dict(schema_version=1, units="SI", **data),
                          sort_keys=False, allow_unicode=True, line_break="\n")
