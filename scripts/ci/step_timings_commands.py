"""Recover expanded gate commands and their environment arguments from the reference sweep."""

import argparse
import json
from pathlib import Path
import re
import shlex
from zipfile import ZipFile

COMMAND = re.compile(r"^\S+ +((?:PYTHONPATH=src )?uv run [^\n]*?python -m pytest "
                     r"[^\n]+--junitxml=reports/[^\n]+)$", re.M)


def gate_commands(folder):
    reference = json.loads((folder / "reference-junit.json").read_text())
    commands = {}
    with ZipFile(folder / "snapshots" / "archives" / f"{reference['run']}-1.zip") as archive:
        for member in archive.namelist():
            match = re.fullmatch(r"\d+_sweep-tests-([\w-]+)\.txt", member)
            if not match:
                continue
            shard = match[1]
            text = re.sub(r"\x1b\[[0-9;]*m", "", archive.read(member).decode("utf-8-sig"))
            lines = {line.strip() for line in COMMAND.findall(text)
                     if "scripts/ci/run_shard.py" not in line}
            if len(lines) != 1 or shard in commands:
                raise ValueError(f"expected exactly one expanded gate command for {shard}")
            command, = lines
            signature = [t for t in shlex.split(command) if not t.startswith(("tests/", "--junitxml="))]
            commands[shard] = dict(command=command, signature=signature)
    if commands.keys() != reference["case_seconds"].keys():
        raise ValueError("reference commands and JUnit shards differ")
    return commands


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("folder", type=Path)
    print(json.dumps(gate_commands(parser.parse_args().folder), indent=2))
