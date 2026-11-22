"""Summarize testcase seconds in the retained reference sweep's JUnit artifacts."""

import argparse
import json
import math
from pathlib import Path
import xml.etree.ElementTree as ET
from zipfile import ZipFile

RUN = 43998853169


def summarize(folder):
    root = folder / "reference-junit"
    index = json.loads((root / "index.json").read_text())
    if len(index["artifacts"]) != index["total_count"]:
        raise ValueError("incomplete reference artifact index")
    times = {}
    for artifact in index["artifacts"]:
        if not artifact["name"].startswith("full-sweep-"):
            continue
        shard = artifact["name"].removeprefix("full-sweep-")
        if artifact["workflow_run"]["id"] != RUN or shard in times:
            raise ValueError(f"unexpected run or duplicate artifact for {shard}")
        with ZipFile(root / f"{artifact['id']}.zip") as archive:
            xml, = [p for p in archive.namelist() if Path(p).name == f"junit-{shard}.xml"]
            cases = ET.fromstring(archive.read(xml)).findall(".//testcase")
        seconds = [float(case.attrib["time"]) for case in cases]
        if not seconds or any(t < 0 or not math.isfinite(t) for t in seconds):
            raise ValueError(f"missing or invalid testcase times for {shard}")
        times[shard] = math.fsum(seconds)
    snapshot = json.loads((folder / "snapshots" / f"{RUN}.json").read_text())
    expected = {j["name"].removeprefix("sweep-tests-") for j in snapshot["jobs"]}
    if len(expected) != 22 or times.keys() != expected:
        raise ValueError("incomplete reference matrix")
    return dict(run=RUN, case_seconds=times, minutes=math.fsum(times.values()) / 60,
                under_sixty=sum(t < 60 for t in times.values()))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("folder", type=Path)
    print(json.dumps(summarize(parser.parse_args().folder), indent=2))
