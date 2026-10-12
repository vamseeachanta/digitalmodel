"""Fake model CLI; intentionally contains no native API imports."""
import argparse
import hashlib
import json
import os
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument("--manifest", required=True)
parser.add_argument("--phase", required=True)
parser.add_argument("--output", required=True)
args = parser.parse_args()
manifest = Path(args.manifest)
mode = json.loads(manifest.read_text())["mode"]
with Path("phases").open("a") as stream:
    stream.write(args.phase + "\n")
assert os.environ["PYTHONHASHSEED"] == "0"
directory = Path(args.output) / args.phase
directory.mkdir()
proof = dict(ok=mode != "failure", phase=args.phase, thread_count_requested=1,
             thread_count_observed=2 if mode == "badproof" else 1,
             fidelity_verified=args.phase == "readback")
proof.update(stage="complete", version="fake", settings_verified=True,
             input_readback_verified=True, simulation_complete=True, warnings=[],
             manifest_sha256=hashlib.sha256(manifest.read_bytes()).hexdigest(),
             loaded_data_sha256="b" * 64,
             results={"times": [i / 10 for i in range(1001)],
                      "histories": {key: [1.0] * 1001 for key in (
                          "tension_end_a", "tension_end_b", "buoy_x", "buoy_z",
                          "buoy_rotation_2", "sea_surface_z")},
                      "static": {"line_arclength": [0, 620],
                                 "line_position": {key: [0, 1] for key in "XYZ"},
                                 "buoy_position": {key: 1 for key in "XYZ"},
                                 "end_tensions": {key: 1 for key in "AB"}},
                      "units": {"times": "s"}, "frames": {"positions": "global"}})
if mode == "incomplete":
    del proof["results"]
(directory / "loaded.yml").write_text("fake native input export")
proof["loaded_data_sha256"] = hashlib.sha256((directory / "loaded.yml").read_bytes()).hexdigest()
if mode == "inputhash":
    proof["loaded_data_sha256"] = "0" * 64
if mode == "wrongphase":
    proof["phase"] = "readback"
if args.phase == "solve" and mode != "missing_sim":
    (directory / "model.sim").write_bytes(b"fake simulation")
if mode != "missing_sim":
    proof["simulation_sha256"] = hashlib.sha256(
        (Path(args.output) / "solve/model.sim").read_bytes()).hexdigest()
if mode == "simhash":
    proof["simulation_sha256"] = "0" * 64
if mode == "mutate":
    manifest.write_text(manifest.read_text() + " ")
(directory / "results.json").write_text(json.dumps(proof))
print(json.dumps(proof))
raise SystemExit(1 if mode == "failure" else 0)
