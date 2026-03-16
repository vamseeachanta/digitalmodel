#!/usr/bin/env python3
"""
ABOUTME: CalculiX result file parser — reads .dat and .frd output files to extract
displacement, stress, and strain fields from FEM solutions.
"""

import math
import re
from pathlib import Path


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
        self._parsed = False

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def parse_frd_file(self) -> dict:
        """Parse ASCII .frd file for displacements and stresses."""
        frd_path = self.work_dir / f"{self.job_name}.frd"
        if not frd_path.exists():
            raise FileNotFoundError(f"FRD file not found: {frd_path}")

        lines = frd_path.read_text().splitlines()
        self._parse_frd_blocks(lines)
        self._parsed = True

        return {
            "displacements": dict(self._displacements),
            "stresses": dict(self._stresses),
        }

    def parse_dat_file(self) -> dict:
        """Parse .dat summary file for reaction forces."""
        dat_path = self.work_dir / f"{self.job_name}.dat"
        if not dat_path.exists():
            raise FileNotFoundError(f"DAT file not found: {dat_path}")

        content = dat_path.read_text()
        reactions = self._extract_reactions(content)
        return {"reactions": reactions}

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
        """Walk through FRD lines and dispatch to block parsers."""
        i = 0
        while i < len(lines):
            line = lines[i]
            if line.startswith(" -4") and "DISP" in line:
                i = self._parse_disp_block(lines, i)
            elif line.startswith(" -4") and "STRESS" in line:
                i = self._parse_stress_block(lines, i)
            else:
                i += 1

    def _parse_disp_block(self, lines: list[str], start: int) -> int:
        """Parse a DISP block starting after the -4 header."""
        i = start + 1
        # Skip all -5 component-name header lines
        while i < len(lines) and lines[i].strip().startswith("-5"):
            i += 1
        while i < len(lines):
            line = lines[i]
            if line.strip().startswith("-3"):
                return i + 1
            if line.strip().startswith("-1"):
                self._parse_disp_line(line)
            i += 1
        return i

    def _parse_stress_block(self, lines: list[str], start: int) -> int:
        """Parse a STRESS block starting after the -4 header."""
        i = start + 1
        while i < len(lines) and lines[i].strip().startswith("-5"):
            i += 1
        while i < len(lines):
            line = lines[i]
            if line.strip().startswith("-3"):
                return i + 1
            if line.strip().startswith("-1"):
                self._parse_stress_line(line)
            i += 1
        return i

    def _parse_disp_line(self, line: str):
        """Parse fixed-width FRD displacement line.

        Format: -1  node_id(10)  dx(12)  dy(12)  dz(12)
        """
        if len(line) < 37:
            return
        node_id = int(line[3:13])
        dx = float(line[13:25])
        dy = float(line[25:37])
        dz = float(line[37:49]) if len(line) >= 49 else 0.0
        dmag = math.sqrt(dx * dx + dy * dy + dz * dz)
        self._displacements[node_id] = {
            "dx": dx, "dy": dy, "dz": dz, "magnitude": dmag,
        }

    def _parse_stress_line(self, line: str):
        """Parse fixed-width FRD stress line.

        Format: -1  node_id(10)  sxx(12)  syy(12)  szz(12)  sxy(12)  syz(12)  szx(12)
        """
        if len(line) < 85:
            return
        node_id = int(line[3:13])
        sxx = float(line[13:25])
        syy = float(line[25:37])
        szz = float(line[37:49])
        sxy = float(line[49:61])
        syz = float(line[61:73])
        szx = float(line[73:85])
        self._stresses[node_id] = {
            "sxx": sxx, "syy": syy, "szz": szz,
            "sxy": sxy, "syz": syz, "szx": szx,
        }

    # ------------------------------------------------------------------
    # DAT parsing
    # ------------------------------------------------------------------

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
