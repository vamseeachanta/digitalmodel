"""Worked synthetic proof audit; the supplied directory must not exist."""

import argparse
import json
from pathlib import Path

from digitalmodel.infrastructure.solver_proof import audit_proof, capture_inputs

# NIST SHA-256 worked example, One Block Message Sample (PDF modified 2012).
# https://csrc.nist.gov/CSRC/media/Projects/Cryptographic-Standards-and-Guidelines/documents/examples/SHA256.pdf
ABC_SHA256 = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"


def run_example(directory):
    """Retain inputs, a fake result, and both cleanup dispositions for readback."""
    directory = Path(directory)
    directory.mkdir(parents=True, exist_ok=False)
    required_inputs = {"source.txt": ABC_SHA256}
    (directory / "source.txt").write_bytes(b"abc")
    before = capture_inputs(directory, required_inputs)  # Before any child launch.
    # Synthetic child evidence, not numerical output from a native solver.
    (directory / "result.txt").write_bytes(b"abc")
    child = {"ok": True, "input_hashes": before,
             "output_hashes": {"result.txt": ABC_SHA256}}
    observed = {"scope": "owned_process_tree", "observation_succeeded": True,
                "root_exited": True, "active_processes": 0}
    answers = {
        "observed": audit_proof(directory, required_inputs, ["result.txt"], child, observed),
        "requested_only": audit_proof(directory, required_inputs, ["result.txt"], child,
                                      {"termination_requested": True}),
    }
    destination = directory / "audit.json"
    destination.write_text(json.dumps(answers, indent=2) + "\n", encoding="utf-8")
    if json.loads(destination.read_text(encoding="utf-8")) != answers:
        raise ValueError("audit readback differs")
    return answers


def main():
    """Write the synthetic example to an explicitly supplied scratch directory."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("directory", type=Path)
    args = parser.parse_args()
    print(json.dumps(run_example(args.directory), indent=2))


if __name__ == "__main__":
    main()
