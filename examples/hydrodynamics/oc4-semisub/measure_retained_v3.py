# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Measure supplied inputs without copying retained hull or platform bytes."""

import argparse
import importlib.util
import json
from pathlib import Path

import yaml

HERE = Path(__file__).resolve().parent
INPUTS = ("oc4_L1.gdf", "oc4_L2.gdf", "oc4_L1.yml", "oc4_L1_df4.yml",
          "oc4_L2.yml", "mass-properties.json")


def module(name):
    """Load only adjacent offline code."""
    spec = importlib.util.spec_from_file_location(name, HERE / f"{name}.py")
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


def measure_inputs(inputs, destination):
    """Bind digests, measured bands, generated decks and conditional estimates.

    A missing Gmsh installation leaves the reference lid and full packet DF0
    explicitly OPEN. Hull bytes and supplied platform records stay external.
    Completed receipts are reused only after every bound digest is checked.
    """
    inputs, destination = Path(inputs), Path(destination)
    prepare, builder, band = (module(name) for name in
                              ("prepare_staged_v3", "build_datafile_v3", "mesh_band_v3"))
    checksum_lines = (inputs / "SHA256SUMS.txt").read_text().splitlines()
    expected = dict(line.split(maxsplit=1)[::-1] for line in checksum_lines if line.strip())
    hashes = {name: prepare.digest(inputs / name) for name in (*INPUTS, "SHA256SUMS.txt")}
    if any(expected.get(name) != hashes[name] for name in INPUTS):
        raise ValueError("Input digest mismatch")
    source_hashes = {**prepare.source_hashes(), "measure_retained_v3.py": prepare.digest(Path(__file__))}
    manifest_path = destination / "retained-measurements-v3.json"
    if manifest_path.exists():
        previous = json.loads(manifest_path.read_text())
        if previous["input_sha256"] != hashes or previous["source_sha256"] != source_hashes:
            raise ValueError("Input digest or generator digest mismatch on resume")
        for name, digest in previous["generated_sha256"].items():
            if prepare.digest(destination / name) != digest:
                raise ValueError("Output digest mismatch")
        return previous
    if destination.exists() and any(destination.iterdir()):
        raise ValueError("Partial output retained; use a new empty directory")
    expected_mass = json.loads((HERE / "expected-v2.json").read_text())["mass_properties"]
    supplied_mass = json.loads((inputs / "mass-properties.json").read_text())
    keys = ("mass_kg", "centre_of_mass_m", "inertia_about_com_kg_m2")
    if any(supplied_mass[key] != expected_mass[key] for key in keys):
        raise ValueError("Supplied platform basis differs from the committed basis")
    meshes = {level: prepare.mesh_record(inputs / f"oc4_{level}.gdf") for level in ("L1", "L2")}
    check = module("check_mesh")
    hull_acceptance = {level: check.validate_hull(check.read_gdf(inputs / f"oc4_{level}.gdf"))
                       for level in ("L1", "L2")}
    specification = builder.resolved_specification(meshes["L1"]["valid_band_rad_s"])
    destination.mkdir(parents=True, exist_ok=True)
    builder.build_datafiles(destination, specification)
    probe = builder.probe_model(specification)
    (destination / "l2-probes-model-v3.yml").write_text(yaml.safe_dump(probe, sort_keys=False))
    (destination / "resolved-specification-v3.json").write_text(json.dumps(specification, indent=2) + "\n")
    stages = {}
    panels = module("check_mesh").read_gdf(inputs / "oc4_L1.gdf")
    for stage in ("S", "F"):
        deck = yaml.safe_load((destination / f"stage-{stage.lower()}-model-v3.yml").read_text())
        if deck != builder.stage_model(stage, specification):
            raise ValueError("Stage datafile readback differs")
        stages[stage] = {**specification["stages"][stage],
                         "df3": band.df3_declared(panels, specification["stages"][stage]["upper_rad_s"], depth=1500)}
        if not stages[stage]["df3"]["pass"]:
            raise ValueError("Declared-band DF3 failed")
    lid, lid_record = {"status": "NOT_REQUIRED"}, None
    if stages["F"]["interior_lid"]:
        lid_path = destination / "l1-lid-v3.gdf"
        try:
            checked = prepare.generate_lid(inputs / "oc4_L1.gdf", lid_path,
                                           max_span=meshes["L1"]["max_span_m"])
        except ModuleNotFoundError as error:
            if error.name != "gmsh":
                raise
            lid = {"status": "OPEN_TOOL_UNAVAILABLE", "tool": "gmsh",
                   "reason": "Gmsh package unavailable; no source build attempted"}
        else:
            lid_record = prepare.mesh_record(lid_path, declared_upper=stages["F"]["upper_rad_s"])
            lid = {"status": "REFERENCE_GENERATED_NATIVE_READBACK_OPEN", "mesh": lid_record, "check": checked}
    l2_df3 = band.df3_declared(module("check_mesh").read_gdf(inputs / "oc4_L2.gdf"),
                              max(probe["PeriodOrFrequency"]), depth=1500)
    if not l2_df3["pass"]:
        raise ValueError("L2 probe DF3 failed")
    result = {"schema_version": 3, "status": "OFFLINE_MEASUREMENTS_NATIVE_ACCEPTANCE_OPEN",
              "input_sha256": hashes, "source_sha256": source_hashes,
              "meshes": meshes, "hull_acceptance": hull_acceptance, "mass_basis_matches": True,
              "measured_mass_basis": {key: supplied_mass[key] for key in keys},
              "stages": stages, "l2_probe_count": len(probe["PeriodOrFrequency"]),
              "l2_probe_df3": l2_df3, "lid": lid,
              "packet_df0": ("OPEN_REFERENCE_LID_UNAVAILABLE" if lid["status"] == "OPEN_TOOL_UNAVAILABLE"
                             else "OPEN_ASSEMBLE_EXTERNAL_INPUTS_FOR_FULL_PACKET_CHECK"),
              "benchmark": {"panels": 14064, "time_s": 3044.0, "frequencies": 32,
                            "peak_gb": 35.6, "status": "SUPPLIED_NOT_NEWLY_MEASURED",
                            "panel_count_matches_L1": meshes["L1"]["panels"] == 14064,
                            "mesh_sha256": meshes["L1"]["sha256"],
                            "link_status": "SUPPLIED_BENCHMARK_LINK; no independent native run receipt"},
              "estimates": prepare.packet_estimates(meshes, lid_record, specification),
              "generator_command": "python examples/hydrodynamics/oc4-semisub/measure_retained_v3.py INPUTS OUTPUT",
              "generated_sha256": {p.name: prepare.digest(p) for p in destination.iterdir()}}
    manifest_path.write_text(json.dumps(result, indent=2) + "\n")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("inputs", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    measure_inputs(args.inputs, args.output)


if __name__ == "__main__":
    main()
