"""Replay a digitalmodel report using its original evaluator and retained Git evidence."""

import argparse
import hashlib
import json
import runpy
from pathlib import Path
from unittest.mock import patch

import evalkit


HERE = Path(__file__).resolve().parent
REPORTS = sorted(path.parent.name for path in HERE.glob("*/last-run-pinned.json"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("report", choices=REPORTS)
    parser.add_argument("--out", type=Path)
    options = parser.parse_args()
    folder = HERE / options.report
    record_path = folder / "last-run-pinned.json"
    problems = evalkit.check_record(folder, record_path)
    if problems:
        parser.error("; ".join(problems))
    record = json.loads(record_path.read_bytes())
    # Load this checkout's stdlib-only reader, including on Python 3.10 without
    # digitalmodel installed (the two offline reports support that configuration).
    reader = runpy.run_path(str(HERE.parents[1] / "src/digitalmodel/legacy_evidence.py"))
    original_digests = record["context"].get("sha256", {})

    def git_blob(repo, commit, path, fetch=True):
        if commit != record["pinned_commit"]:
            raise ValueError("Report revision differs from the recorded run")
        raw = reader["read_evidence"](repo, commit, path)
        if original_digests:
            digest = hashlib.sha256(raw.replace(b"\r\n", b"\n")).hexdigest()
            if digest != original_digests.get(path):
                raise ValueError(f"Recorded report input digest differs for {path}")
        return raw

    report = runpy.run_path(str(folder / "eval.py"))

    def compute(args, values, notes, context):
        context["replay_adapter_sha256"] = evalkit.sha256_text(Path(__file__))
        context["legacy_manifest_sha256"] = reader["MANIFEST_SHA256"]
        report["compute"](args, values, notes, context)

    argv = ["--revision", "pinned"]
    if options.out is not None:
        argv.extend(["--out", str(options.out)])
    # The original evalkit and evaluator remain byte-identical to the run record.
    with patch.object(evalkit, "git_blob", git_blob):
        return evalkit.main(folder / "results.json", compute, report["add_args"], argv)


if __name__ == "__main__":
    raise SystemExit(main())
