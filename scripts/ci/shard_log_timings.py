"""Project per-attempt Actions logs to the timing evidence used by the offline report."""

import io
import json
import re
import subprocess
from zipfile import ZipFile

if __package__:
    from .geotechnical_timing import command_blocks, pytest_phases, seconds_between
else:
    from geotechnical_timing import command_blocks, pytest_phases, seconds_between


def geo_progress(block):
    """Time contiguous base geotechnical progress rows bracketed by other test paths."""
    start = previous = end = None
    inside = closed = False
    for line in block.splitlines():
        stamp, _, msg = line.partition(" ")
        if not re.search(r"\[\s*\d+%\]\s*$", msg):
            continue
        if msg.startswith("tests/"):
            geo = msg.startswith("tests/geotechnical/")
            if inside and not geo:
                closed = True
            if geo and not inside:
                if closed:
                    return None
                start = previous
            inside = geo
        if inside:
            end = stamp
        previous = stamp
    return seconds_between(start, end) if closed and start else None


def shard_phases(log):
    blocks = command_blocks(log)
    checkouts = [b for b in blocks if "##[group]Run actions/checkout@" in b.partition("\n")[0]]
    inputs = checkouts[0].partition("##[endgroup]")[0] if len(checkouts) == 1 else ""
    depths = re.findall(r"(?m)^\S+ +fetch-depth: (\d+)\s*$", inputs)
    gates = []
    for block in blocks:
        header = block.partition("##[endgroup]")[0]
        if "##[group]Run " in header and "pytest" in header and "--junitxml" in header:
            gates.append(block)
    gate = gates[0] if len(gates) == 1 else ""
    phase = pytest_phases(gate)
    geo = geo_progress(gate) if phase["pytest_s"] is not None else None
    return dict(depth=int(depths[0]) if len(depths) == 1 else None,
                install_s=phase["install_s"], pytest_s=phase["pytest_s"], geo_progress_s=geo)


def fetch_run_phases(folder, run, jobs, get):
    archive_dir = folder / "archives"
    archive_dir.mkdir(exist_ok=True)
    result = {}
    for attempt in sorted({job["run_attempt"] for job in jobs}):
        archive = archive_dir / f"{run['id']}-{attempt}.zip"
        path = f"actions/runs/{run['id']}/attempts/{attempt}/logs"
        try:
            raw = archive.read_bytes() if archive.exists() else get(path)
        except subprocess.CalledProcessError as exc:
            if not re.search(rb"\(HTTP (404|410)\)", exc.stderr):
                raise
            continue
        with ZipFile(io.BytesIO(raw)) as logs:
            if not archive.exists():
                archive.write_bytes(raw)
            files = {re.sub(r"^\d+_", "", p)[:-4]: p for p in logs.namelist()
                     if re.fullmatch(r"\d+_[^/]+\.txt", p)}
            for job in jobs:
                name = files.get(job["name"])
                if job["run_attempt"] == attempt and name:
                    result[str(job["id"])] = shard_phases(logs.read(name).decode("utf-8-sig", errors="replace"))
    (folder / f"{run['id']}-phases.json").write_text(json.dumps(result))
