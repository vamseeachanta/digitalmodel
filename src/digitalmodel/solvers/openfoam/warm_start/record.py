"""YAML records and campaign TSV ledger."""
from __future__ import annotations

import csv
from datetime import datetime, timezone
from pathlib import Path

import yaml

from .decision import PRIORS

LEDGER_COLUMNS = ("timestamp target hop source level event p n_warm_est S n_abort EV "
                  "margin iterations reason").split()


def timestamp() -> str:
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


class RecordStore:
    def __init__(self, directory: Path, hop: str, level: str, n_cold: int):
        self.directory = Path(directory)
        self.path = self.directory / f"record_{hop}_{level}.yml"
        self.hop, self.level, self.n_cold = hop, level, n_cold

    def load(self) -> dict:
        if self.path.exists():
            return yaml.safe_load(self.path.read_text()) or {}
        return {"hop_type": self.hop, "mesh_level": self.level,
                "prior": {"beta_a": 2, "beta_b": 2,
                          "saving_fraction": PRIORS[self.hop]},
                "n_cold": self.n_cold, "hops": [], "summary": {}}

    def append(self, hop: dict) -> dict:
        data = self.load()
        data.setdefault("hops", []).append(hop)
        completed = data["hops"]
        ok = [h for h in completed if h.get("outcome") == "WARM_OK" and
              int(h.get("iterations", self.n_cold + 1)) <= .75 * self.n_cold]
        fail = [h for h in completed if h.get("outcome") in {"WARM_ABORTED", "WARM_FAILED_CAP"}]
        data["summary"] = {"successes": len(ok), "failures": len(fail),
                           "n_warm_mean": (sum(int(h["iterations"]) for h in ok) / len(ok)) if ok else None,
                           "p_posterior": (2 + len(ok)) / (4 + len(ok) + len(fail))}
        self.directory.mkdir(parents=True, exist_ok=True)
        self.path.write_text(yaml.safe_dump(data, sort_keys=False))
        return data


def append_ledger(path: Path, values: dict) -> None:
    path = Path(path); path.parent.mkdir(parents=True, exist_ok=True)
    row = {k: values.get(k, "") for k in LEDGER_COLUMNS}
    row["timestamp"] = row["timestamp"] or timestamp()
    with path.open("a", newline="") as stream:
        csv.DictWriter(stream, LEDGER_COLUMNS, delimiter="\t", lineterminator="\n").writerow(row)
