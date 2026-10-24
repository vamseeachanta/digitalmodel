#!/usr/bin/env python3
"""
ABOUTME: CalculiX result file parser — reads .dat and .frd output files to extract
displacement, stress, and strain fields from FEM solutions.
"""

import math
import re
from pathlib import Path

import numpy as np


def _number(value: str) -> float:
    """Read solver reals, including Fortran double-precision exponents."""
    result = float(value.replace("D", "E").replace("d", "e"))
    if not math.isfinite(result):
        raise ValueError("Non-finite solver result")
    return result


class CalculiXResultParser:
    """
    Parse CalculiX output files (.frd ASCII and .dat).

    Extracts displacement and stress fields for post-processing.
    """

    def __init__(self, job_name: str, work_dir: Path):
        self.job_name = job_name
        self.work_dir = Path(work_dir)
        self._displacements: dict[int, dict] = {}
        self._stresses: dict[int, dict] = {}
        self._strains: dict[int, dict] = {}
        self._plastic_strain: dict[int, float] = {}
        self._parsed = False

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def parse_frd_file(self) -> dict:
        """Parse ASCII .frd file for displacements and stresses."""
        frd_path = self.work_dir / f"{self.job_name}.frd"
        if not frd_path.exists():
            raise FileNotFoundError(f"FRD file not found: {frd_path}")

        self._parsed = False
        self._displacements, self._stresses = {}, {}
        self._strains, self._plastic_strain = {}, {}
        lines = frd_path.read_text().splitlines()
        self._parse_frd_blocks(lines)
        self._parsed = True

        return {
            "displacements": dict(self._displacements),
            "stresses": dict(self._stresses),
            "strains": dict(self._strains),
            "equivalent_plastic_strain": dict(self._plastic_strain),
        }

    def parse_dat_file(self) -> dict:
        """Parse .dat summary file for reaction forces."""
        dat_path = self.work_dir / f"{self.job_name}.dat"
        if not dat_path.exists():
            raise FileNotFoundError(f"DAT file not found: {dat_path}")

        content = dat_path.read_text()
        reactions = self._extract_reactions(content)
        return {"reactions": reactions,
                "buckling_factors": self._extract_buckling_factors(content)}

    def parse_sta_file(self) -> dict:
        """Read converged increments, never equate solver failure to collapse.

        CalculiX 2.22 manual (2024), section 5 troubleshooting: .sta records
        increment convergence. U marks an unsuccessful attempt; this is solver
        evidence, not a strain-limit or physical-collapse acceptance criterion.
        """
        path = self.work_dir / f"{self.job_name}.sta"
        increments, unsuccessful = [], False
        for line in path.read_text().splitlines():
            fields = line.split()
            if not fields or not fields[0].isdigit():
                continue
            if len(fields) != 7:
                raise ValueError("Incomplete STA increment record")
            step, increment, attempt, iterations, total, time, size = fields
            if "U" in attempt.upper():
                unsuccessful = True
                continue
            increments.append({"step": int(step), "increment": int(increment),
                               "attempt": int(attempt), "iterations": int(iterations),
                               "total_time": _number(total), "step_time": _number(time),
                               "increment_time": _number(size)})
        return {"increments": increments,
                "last_converged": increments[-1] if increments else None,
                "has_unconverged_attempts": unsuccessful,
                "collapse_status": "NOT_EVALUATED"}

    def get_max_von_mises(self) -> float:
        """Return maximum von Mises stress across all nodes."""
        if not self._parsed:
            self.parse_frd_file()
        max_vm = 0.0
        for stress in self._stresses.values():
            vm = _von_mises(
                stress["sxx"], stress["syy"], stress["szz"],
                stress["sxy"], stress["syz"], stress["szx"],
            )
            max_vm = max(max_vm, vm)
        return max_vm

    def get_max_displacement(self) -> float:
        """Return maximum total displacement magnitude."""
        if not self._parsed:
            self.parse_frd_file()
        max_d = 0.0
        for disp in self._displacements.values():
            max_d = max(max_d, disp["magnitude"])
        return max_d

    def get_max_stress_component(self, component: str = "sxx") -> float:
        """Return maximum value of a specific stress component across all nodes."""
        if not self._parsed:
            self.parse_frd_file()
        return max(
            abs(s[component]) for s in self._stresses.values()
        )

    def get_stress_at_node(self, node_id: int) -> dict:
        """Return stress components at a specific node (1-based ID)."""
        if not self._parsed:
            self.parse_frd_file()
        if node_id not in self._stresses:
            raise KeyError(f"No stress data for node {node_id}")
        return dict(self._stresses[node_id])

    # ------------------------------------------------------------------
    # FRD parsing internals
    # ------------------------------------------------------------------

    def _parse_frd_blocks(self, lines: list[str]):
        """Keep the latest complete block of each field (not a synchronized frame).

        Labels follow CalculiX 2.22 (2024), section 7.51. TOSTRAIN is total
        Lagrangian strain for incremental plasticity; it is not plastic strain.
        """
        i = 0
        while i < len(lines):
            line = lines[i]
            if not line.startswith(" -4"):
                i += 1
                continue
            header = line.split()
            if len(header) < 2:
                raise ValueError("Missing label in FRD field header")
            label = header[1]
            specs = {"DISP": (3, self._displacements),
                     "STRESS": (6, self._stresses), "TOSTRAIN": (6, self._strains),
                     "PE": (1, self._plastic_strain), "PEEQ": (1, self._plastic_strain)}
            if label not in specs:
                i += 1
                continue
            count, target = specs[label]
            rows, i = self._read_field(lines, i, count)
            target.clear()
            for node, values in rows.items():
                if label == "DISP":
                    target[node] = dict(zip(("dx", "dy", "dz"), values))
                    target[node]["magnitude"] = math.sqrt(sum(v*v for v in values))
                elif label in {"PE", "PEEQ"}:
                    target[node] = values[0]
                else:
                    prefix = "e" if label == "TOSTRAIN" else "s"
                    target[node] = dict(zip((prefix + c for c in ("xx", "yy", "zz", "xy", "yz", "zx")), values))
                    if label == "TOSTRAIN":
                        xx, yy, zz, xy, yz, zx = values
                        tensor = [[xx, xy, zx], [xy, yy, yz], [zx, yz, zz]]
                        target[node]["principal"] = np.linalg.eigvalsh(tensor)[::-1].tolist()

    @staticmethod
    def _read_field(lines, start, count):
        rows, i = {}, start + 1
        while i < len(lines):
            line = lines[i]
            flag = line[:3].strip()
            if flag == "-3":
                return rows, i + 1
            if flag == "-1":
                if len(line) < 13 + 12*count:
                    raise ValueError("Truncated FRD field record")
                node = int(line[3:13])
                if node in rows:
                    raise ValueError("Duplicate node in FRD field")
                rows[node] = [_number(line[13 + 12*j:25 + 12*j]) for j in range(count)]
            elif flag != "-5":
                raise ValueError("Unsupported or incomplete ASCII FRD field")
            i += 1
        raise ValueError("Unterminated FRD field")

    # ------------------------------------------------------------------
    # DAT parsing
    # ------------------------------------------------------------------

    @staticmethod
    def _extract_buckling_factors(content):
        """Read buckling tables only; section 7.5 defines load-factor meaning."""
        factors, active, started = [], False, False
        for line in content.splitlines():
            compact = "".join(line.upper().split())
            if "BUCKLINGFACTOR" in compact:
                active, started = True, False
                continue
            if not active:
                continue
            fields = line.split()
            if len(fields) == 2 and fields[0].isdigit():
                factors.append({"mode": int(fields[0]), "factor": _number(fields[1])})
                started = True
            elif started or (compact and "OUTPUT" in compact):
                active = False
        return factors


    def _extract_reactions(self, content: str) -> list[dict]:
        """Extract reaction force summaries from .dat content."""
        reactions = []
        pattern = re.compile(
            r"total force \(fx,fy,fz\)\s+for set (\S+).*?\n"
            r"\s+([\d.Ee+-]+)\s+([\d.Ee+-]+)\s+([\d.Ee+-]+)",
            re.DOTALL,
        )
        for match in pattern.finditer(content):
            reactions.append({
                "set": match.group(1),
                "fx": float(match.group(2)),
                "fy": float(match.group(3)),
                "fz": float(match.group(4)),
            })
        return reactions


def _von_mises(
    sxx: float, syy: float, szz: float,
    sxy: float, syz: float, szx: float,
) -> float:
    """Compute von Mises equivalent stress from 3D stress components."""
    return math.sqrt(
        0.5 * (
            (sxx - syy) ** 2
            + (syy - szz) ** 2
            + (szz - sxx) ** 2
            + 6.0 * (sxy ** 2 + syz ** 2 + szx ** 2)
        )
    )
