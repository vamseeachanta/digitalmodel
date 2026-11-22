"""Audit selected paths and history readers in the pinned checkout using local Git objects."""

import argparse
import fnmatch
import hashlib
import json
from pathlib import Path
import re
import subprocess

BASE = "c90ef11aa6bc77047215e753f5c3290c73e57635"
PATTERN = re.compile(
    r"""\bgit[ "'(,-]+(?:log|show|diff|describe|blame)\b|"""
    r"""\b(?:rev-list|merge-base|cat-file|fetch-depth|ls-tree|--is-shallow-repository|iter_commits)\b|"""
    r"""\bgit(?:\b|_\w+)\s*\(.{0,70}["'](?:show|log|diff|describe|blame)["']|"""
    r"""["']git["'].{0,100}["'](?:show|log|blame|describe|diff)["']|--is-shallow-repository|\bHEAD(?:\^|~\d)""", re.I)
WORKFLOWS = {".github/workflows/full-matrix-sweep.yml", ".github/workflows/quality-gates-by-domain.yml"}


def gate_paths():
    config = subprocess.check_output(["git", "show", f"{BASE}:.claude/quality-gates.yaml"], text=True)
    gates = {}
    for line in config.splitlines():
        match = re.match(r"^  tests-([\w-]+):", line)
        if match:
            shard = match[1]
        if line.strip().startswith("command:") and "python -m pytest " in line:
            args = line.split("python -m pytest ", 1)[1].split()
            paths = [p for p in args if p.startswith("tests/")]
            if shard == "subsea":
                paths.extend(["tests/riser_fatigue/", "tests/riser_bom/"])
            gates[shard] = dict(paths=paths, ignored=[p[9:] for p in args if p.startswith("--ignore=")])
    return gates


def scan_sources(paths):
    matches, tests = {}, {}
    with subprocess.Popen(["git", "cat-file", "--batch"], stdin=subprocess.PIPE, stdout=subprocess.PIPE) as reader:
        try:
            for path in sorted(paths):
                reader.stdin.write(f"{BASE}:{path}\n".encode())
                reader.stdin.flush()
                header = reader.stdout.readline().split()
                if len(header) != 3 or header[1] != b"blob":
                    raise ValueError(f"missing pinned source blob for {path}")
                raw = reader.stdout.read(int(header[2])).decode(errors="replace")
                if reader.stdout.read(1) != b"\n":
                    raise ValueError(f"incomplete pinned source blob for {path}")
                hit = [f"{i}: {s.strip()}" for i, s in enumerate(raw.splitlines(), 1) if PATTERN.search(s)]
                if hit:
                    matches[path] = hit
                if path.startswith("tests/"):
                    tests[path] = raw
        finally:
            reader.stdin.close()
        if reader.wait():
            raise ValueError("historical source read failed")
    return matches, tests


def selected(path, gate):
    if any(path.startswith(p + "/") for p in gate["ignored"]):
        return False
    return path == "tests/conftest.py" or any(
        path.startswith(p) if p.endswith("/") else fnmatch.fnmatchcase(path, p) for p in gate["paths"])


def audit(folder):
    gates = gate_paths()
    if gates.keys() != json.loads((folder / "gate-commands.json").read_text()).keys():
        raise ValueError("history audit and reference sweep shard names differ")
    tree = subprocess.check_output(["git", "ls-tree", "-r", "-z", "--name-only", BASE])
    sources = [f for f in tree.decode().rstrip("\0").split("\0") if f in WORKFLOWS or
               (f.startswith(("scripts/", "tests/", "src/")) and f.endswith((".py", ".sh", ".ps1", ".js")))]
    matches, tests = scan_sources(sources)
    readers = {Path(f).stem for f in matches if f.startswith(("scripts/", "src/"))}
    pattern = re.compile(r"\b(" + "|".join(map(re.escape, sorted(readers))) + r")\b")
    references = {f: sorted(set(pattern.findall(raw))) for f, raw in tests.items()}
    for gate in gates.values():
        files = [f for f in sources if selected(f, gate)]
        gate["selected_source_files"] = len(files)
        gate["source_list_sha256"] = hashlib.sha256("\n".join(sorted(files)).encode()).hexdigest()
        gate["reader_name_references"] = {f: references[f] for f in files if references[f]}
        gate["direct_matches"] = [f for f in files if f in matches]
    return dict(base_sha=BASE, pattern=PATTERN.pattern, shards=gates, source_and_workflow_matches=matches)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("folder", type=Path)
    print(json.dumps(audit(parser.parse_args().folder), indent=2))
