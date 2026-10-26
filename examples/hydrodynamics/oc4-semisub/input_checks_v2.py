# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Fail-closed offline DF0 inputs and DF3 mesh checks; native gates stay open."""

import argparse
import hashlib
import importlib.util
import json
from pathlib import Path

import yaml

HERE = Path(__file__).resolve().parent


def _module(name):
    spec = importlib.util.spec_from_file_location(name, HERE / f"{name}.py")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def sha256(path):
    """Return the digest of retained bytes, without rewriting them."""
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def df0(directory, level, *, expected=None):
    """Reject altered depth, lid, grids, physical basis, inputs or mesh bytes."""
    if level not in ("coarse", "fine"):
        raise ValueError("Unknown mesh level")
    directory = Path(directory)
    canonical = json.loads((HERE / "expected-v2.json").read_text())
    expected = canonical if expected is None else expected
    if expected != canonical:
        raise ValueError("Expected v2 record differs, including historical failures")
    specification = json.loads((HERE / "input-specification-v2.json").read_text())
    check, builder = _module("check_mesh"), _module("build_datafile_v2")
    try:
        manifest = json.loads((directory / f"{level}-manifest-v2.json").read_text())
        if manifest["schema_version"] != 2 or manifest["level"] != level:
            raise ValueError("Wrong mesh manifest version or level")
        inputs = manifest["inputs"]
        for key, value in {"depth_m": 1500, "gravity_m_s2": specification["gravity_m_s2"],
                           "omega_max_rad_s": max(specification["frequencies_rad_s"]),
                           "panels_per_wavelength": 6,
                           "max_span_m": specification["max_panel_span_m"]}.items():
            if inputs[key] != value:
                raise ValueError(f"Mesh input mismatch: {key}")
        for name in ("generate_mesh.py", "check_mesh.py"):
            if inputs["source_sha256"][name] != sha256(HERE / name):
                raise ValueError("Generation source digest mismatch")
        artifacts = manifest["artifacts"]
        if len(artifacts) != 2 or {x["kind"] for x in artifacts} != {"hull", "lid"}:
            raise ValueError("Both hull and lid are required")
        meshes, span_checks = {}, {}
        for artifact in artifacts:
            kind = artifact["kind"]
            filename = f"{level}-{kind}-v2.gdf"
            if artifact["file"] != filename:
                raise ValueError("Mesh filename mismatch")
            path = directory / filename
            if sha256(path) != artifact["sha256"]:
                raise ValueError("Mesh digest mismatch")
            header = path.read_text(encoding="ascii").splitlines()[1].split()
            if float(header[1]) != specification["gravity_m_s2"]:
                raise ValueError("Mesh gravity differs from the input basis")
            meshes[kind] = check.read_gdf(path)
            if len(meshes[kind]) != artifact["panels"]:
                raise ValueError("Mesh panel count mismatch")
            span_checks[kind] = check.df3(meshes[kind], specification["frequencies_rad_s"],
                                          depth=1500, gravity=specification["gravity_m_s2"])
            if not span_checks[kind]["pass"]:
                raise ValueError("DF3 panel span failed")
        lid_check = check.validate_lid(meshes["hull"], meshes["lid"])
        deck = yaml.safe_load((directory / f"{level}-model-v2.yml").read_text())
        required = builder.datafile_model(f"{level}-hull-v2.gdf")
        if deck != required:
            raise ValueError("Datafile differs from the versioned physical and frequency basis")
    except (OSError, KeyError, TypeError, IndexError, json.JSONDecodeError, yaml.YAMLError) as error:
        raise ValueError("Incomplete or malformed input packet") from error
    return {"pass": True, "schema_version": 2, "level": level, "depth_m": 1500,
            "frequency_count": len(specification["frequencies_rad_s"]),
            "deck_sha256": sha256(directory / f"{level}-model-v2.yml"),
            "specification_sha256": sha256(HERE / "input-specification-v2.json"),
            "expected_sha256": sha256(HERE / "expected-v2.json"),
            "df3": span_checks, "lid": lid_check,
            "historical_failures": expected["historical_failures"], "native_acceptance": "OPEN"}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    parser.add_argument("level", choices=("coarse", "fine"))
    args = parser.parse_args()
    try:
        print(json.dumps(df0(args.directory, args.level), indent=2, allow_nan=False))
    except ValueError as error:
        parser.exit(1, f"DF0/DF3 FAIL: {error}\n")


if __name__ == "__main__":
    main()
