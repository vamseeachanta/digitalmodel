# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Check both staged input packets, retaining every historical acceptance gate."""

import argparse
import importlib.util
import json
from pathlib import Path

import yaml


def module(name):
    """Load an adjacent offline module."""
    spec = importlib.util.spec_from_file_location(name, Path(__file__).with_name(f"{name}.py"))
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


def df0(directory):
    """Fail closed on changed bytes, basis, stage grids, lids or declared bands."""
    directory = Path(directory)
    prepare, builder, check = (module(name) for name in ("prepare_staged_v3", "build_datafile_v3", "check_mesh"))
    try:
        manifest = json.loads((directory / "manifest-v3.json").read_text())
        if manifest["schema_version"] != 3 or manifest["source_sha256"] != prepare.source_hashes():
            raise ValueError("Preparation source digest mismatch")
        meshes = manifest["meshes"]
        if set(meshes) not in ({"L1"}, {"L1", "L2"}):
            raise ValueError("Unexpected mesh set")
        for level, record in meshes.items():
            filename = f"{level.lower()}-hull-v3.gdf"
            if record != prepare.mesh_record(directory / filename):
                raise ValueError("Mesh measurements or digest differ")
        specification = builder.resolved_specification(meshes["L1"]["valid_band_rad_s"])
        if json.loads((directory / "resolved-specification-v3.json").read_text()) != specification:
            raise ValueError("Resolved specification differs")
        files = {"l1-hull-v3.gdf", "resolved-specification-v3.json",
                 "stage-s-model-v3.yml", "stage-f-model-v3.yml"}
        if "L2" in meshes:
            files.update({"l2-hull-v3.gdf", "l2-probes-model-v3.yml"})
        lid_check = None
        required_lid = specification["stages"]["F"]["interior_lid"]
        if required_lid:
            files.add("l1-lid-v3.gdf")
            lid = prepare.mesh_record(directory / "l1-lid-v3.gdf", declared_upper=meshes["L1"]["valid_band_rad_s"])
            if lid != manifest["lid"] or not lid["declared_df3"]["pass"]:
                raise ValueError("Reference lid measurement mismatch")
            lid_check = check.validate_lid(check.read_gdf(directory / "l1-hull-v3.gdf"),
                                          check.read_gdf(directory / "l1-lid-v3.gdf"))
        elif manifest["lid"] is not None:
            raise ValueError("Unexpected reference lid")
        if set(manifest["files"]) != files:
            raise ValueError("Incomplete staged packet")
        for filename in files:
            if manifest["files"][filename] != prepare.digest(directory / filename):
                raise ValueError("Packet digest mismatch")
        if manifest["estimates"] != prepare.packet_estimates(meshes, manifest["lid"], specification):
            raise ValueError("Native estimate differs from the bound counts")
        probe_df3 = None
        if "L2" in meshes:
            expected_probe = builder.probe_model(specification)
            if yaml.safe_load((directory / "l2-probes-model-v3.yml").read_text()) != expected_probe:
                raise ValueError("L2 probe datafile differs from the bound basis")
            probe_df3 = module("mesh_band_v3").df3_declared(
                check.read_gdf(directory / "l2-hull-v3.gdf"), max(expected_probe["PeriodOrFrequency"]), depth=1500)
            if not probe_df3["pass"]:
                raise ValueError("L2 probe DF3 failed")
        stages = {}
        for stage in ("S", "F"):
            path = directory / f"stage-{stage.lower()}-model-v3.yml"
            if yaml.safe_load(path.read_text()) != builder.stage_model(stage, specification):
                raise ValueError("Stage datafile differs from the bound basis")
            upper = specification["stages"][stage]["upper_rad_s"]
            span = module("mesh_band_v3").df3_declared(check.read_gdf(directory / "l1-hull-v3.gdf"), upper, depth=1500)
            if not span["pass"]:
                raise ValueError("Declared-band DF3 failed")
            stages[stage] = {"frequency_count": specification["stages"][stage]["frequency_count"],
                             "upper_rad_s": upper, "lid_required": specification["stages"][stage]["interior_lid"],
                             "df3": span}
    except (OSError, KeyError, TypeError, IndexError, json.JSONDecodeError, yaml.YAMLError) as error:
        raise ValueError("Incomplete or malformed staged packet") from error
    return {"pass": True, "schema_version": 3, "stages": stages, "lid": lid_check,
            "l2_probe_df3": probe_df3,
            "historical_failures": {"DF5.2": "FAIL", "DF5.3": "FAIL"}, "native_acceptance": "OPEN"}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    args = parser.parse_args()
    print(json.dumps(df0(args.directory), indent=2))


if __name__ == "__main__":
    main()
