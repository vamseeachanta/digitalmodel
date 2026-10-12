#!/usr/bin/env python3
"""
ABOUTME: MYSTRAN result parser — reads the F06 text output (displacements,
SPC forces, element stresses, epsilon error estimate, error messages) and
optionally the OP2 binary via pyNastran.
"""

import math
import re
from pathlib import Path
from typing import Optional

_DISP_HEADER = "D I S P L A C E M E N T S"
_SPCF_HEADER = "S P C   F O R C E S"
_STRESS_HEADER = "E L E M E N T   S T R E S S E S"
_ETYPE_RE = re.compile(r"F O R\s+E L E M E N T\s+T Y P E\s+(.+)$")
_EPSILON_RE = re.compile(r"EPSILON ERROR ESTIMATE\s*=\s*([-+0-9.Ee]+)")
_VEC_KEYS = ("t1", "t2", "t3", "r1", "r2", "r3")


class MystranResultParser:
    """
    Parse MYSTRAN output for a job in ``work_dir``.

    MYSTRAN writes ``<job>.F06`` (upper-case extension); both cases are
    searched so parsing works on case-sensitive filesystems too.
    """

    def __init__(self, job_name: str, work_dir: Path):
        self.job_name = job_name
        self.work_dir = Path(work_dir)
        self._displacements: dict[int, dict[str, float]] = {}
        self._spc_forces: dict[int, dict[str, float]] = {}
        self._stresses: dict[str, dict[int, dict]] = {}
        self._errors: list[str] = []
        self._epsilon: Optional[float] = None
        self._parsed = False

    # ------------------------------------------------------------------
    # File discovery
    # ------------------------------------------------------------------

    def find_output(self, ext: str) -> Optional[Path]:
        """Return the output file with extension ``ext`` (any case) or None."""
        for candidate in (ext.upper(), ext.lower()):
            p = self.work_dir / f"{self.job_name}.{candidate}"
            if p.exists():
                return p
        return None

    @property
    def f06_path(self) -> Path:
        p = self.find_output("F06")
        if p is None:
            raise FileNotFoundError(
                f"F06 file not found for job {self.job_name!r} in {self.work_dir}"
            )
        return p

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def parse_f06(self) -> dict:
        """Parse the F06 file. Returns a dict summary of everything found."""
        text = self.f06_path.read_text(errors="replace")
        return self.parse_f06_text(text)

    def parse_f06_text(self, text: str) -> dict:
        """Parse F06 content given as a string (used by tests)."""
        self._displacements.clear()
        self._spc_forces.clear()
        self._stresses.clear()
        self._errors.clear()
        self._epsilon = None

        lines = text.splitlines()
        i = 0
        n = len(lines)
        while i < n:
            line = lines[i]
            if "*ERROR" in line or "*FATAL" in line.upper():
                self._errors.append(line.strip())
            m = _EPSILON_RE.search(line)
            if m:
                try:
                    self._epsilon = float(m.group(1))
                except ValueError:
                    pass
            if _DISP_HEADER in line:
                i = self._parse_vector_block(lines, i + 1, self._displacements)
                continue
            if _SPCF_HEADER in line:
                i = self._parse_vector_block(lines, i + 1, self._spc_forces)
                continue
            if _STRESS_HEADER in line:
                i = self._parse_stress_block(lines, i + 1)
                continue
            i += 1
        self._parsed = True
        return self.summary()

    def summary(self) -> dict:
        return {
            "displacements": dict(self._displacements),
            "spc_forces": dict(self._spc_forces),
            "element_stresses": {k: dict(v) for k, v in self._stresses.items()},
            "epsilon": self._epsilon,
            "errors": list(self._errors),
        }

    @property
    def has_errors(self) -> bool:
        self._ensure_parsed()
        return bool(self._errors)

    def get_displacement(self, grid_id: int) -> dict[str, float]:
        """Displacement vector at a 1-based grid ID."""
        self._ensure_parsed()
        if grid_id not in self._displacements:
            raise KeyError(f"No displacement for grid {grid_id}")
        return dict(self._displacements[grid_id])

    def get_max_displacement(self, component: str = "t3") -> float:
        """Maximum absolute displacement component (or ``"mag"``) over grids."""
        self._ensure_parsed()
        component = component.lower()
        if component == "mag":
            return max(
                (math.sqrt(d["t1"] ** 2 + d["t2"] ** 2 + d["t3"] ** 2)
                 for d in self._displacements.values()),
                default=0.0,
            )
        if component not in _VEC_KEYS:
            raise ValueError(f"Unknown component {component!r}")
        return max(
            (abs(d[component]) for d in self._displacements.values()),
            default=0.0,
        )

    def get_reaction_sum(self) -> dict[str, float]:
        """Sum of SPC forces over all constrained grids."""
        self._ensure_parsed()
        total = {k: 0.0 for k in _VEC_KEYS}
        for f in self._spc_forces.values():
            for k in _VEC_KEYS:
                total[k] += f[k]
        return total

    def get_max_von_mises(self, element_type: Optional[str] = None) -> float:
        """Max von Mises over parsed solid/shell stress rows (center + grid)."""
        self._ensure_parsed()
        best = 0.0
        for etype, elems in self._stresses.items():
            if element_type and etype != element_type:
                continue
            for rec in elems.values():
                rows = [rec["center"]] if rec.get("center") else []
                rows += list(rec.get("grid", {}).values())
                for row in rows:
                    vm = row.get("von_mises")
                    if vm is not None:
                        best = max(best, abs(vm))
        return best

    def read_op2(self):
        """Load the OP2 with pyNastran (optional dependency). Returns OP2."""
        try:
            from pyNastran.op2.op2 import OP2
        except ImportError as exc:  # pragma: no cover - optional
            raise ImportError(
                "pyNastran is required for OP2 reading: "
                "install digitalmodel[nastran]"
            ) from exc
        p = self.find_output("OP2")
        if p is None:
            raise FileNotFoundError(f"OP2 not found for job {self.job_name!r}")
        model = OP2(debug=False)
        model.read_op2(str(p))
        return model

    # ------------------------------------------------------------------
    # Block parsers
    # ------------------------------------------------------------------

    def _ensure_parsed(self) -> None:
        if not self._parsed:
            self.parse_f06()

    @staticmethod
    def _parse_vector_block(lines: list[str], start: int, target: dict) -> int:
        """Parse GRID/COORD/T1..R3 rows. Returns index after the block."""
        i = start
        n = len(lines)
        started = False
        while i < n:
            tok = lines[i].split()
            is_row = len(tok) == 8 and tok[0].isdigit() and tok[1].isdigit()
            vals: list[float] = []
            if is_row:
                try:
                    vals = [float(v) for v in tok[2:8]]
                except ValueError:
                    is_row = False
            if is_row:
                target[int(tok[0])] = dict(zip(_VEC_KEYS, vals))
                started = True
            elif started:
                return i
            elif "-----" in lines[i] or "MAX*" in lines[i]:
                return i
            i += 1
        return i

    def _parse_stress_block(self, lines: list[str], start: int) -> int:
        """Parse an element stress block with CENTER/GRD rows."""
        i = start
        n = len(lines)
        etype = None
        headers: list[str] = []
        while i < n and etype is None:
            m = _ETYPE_RE.search(lines[i])
            if m:
                etype = m.group(1).replace(" ", "").strip()
            i += 1
        if etype is None:
            return i
        while i < n:
            if "Location" in lines[i]:
                hdr = lines[i].replace("von Mises", "von_Mises")
                tok = hdr.split()
                headers = [_norm(h) for h in tok[tok.index("Location") + 1:]]
                i += 1
                break
            if _DISP_HEADER in lines[i] or _STRESS_HEADER in lines[i]:
                return i  # different layout (e.g. BAR) — not parsed
            i += 1
        if not headers:
            return i
        block = self._stresses.setdefault(etype, {})
        current_eid: Optional[int] = None
        while i < n:
            line = lines[i]
            tok = line.split()
            if "-----" in line or "MAX*" in line:
                return i + 1
            if len(tok) >= 3 and tok[0].isdigit() and tok[1] == "CENTER":
                current_eid = int(tok[0])
                vals = _floats(tok[2:])
                block[current_eid] = {
                    "center": dict(zip(headers, vals)),
                    "grid": {},
                }
            elif (
                len(tok) >= 3 and tok[0] == "GRD" and tok[1].isdigit()
                and current_eid is not None
            ):
                vals = _floats(tok[2:])
                block[current_eid]["grid"][int(tok[1])] = dict(zip(headers, vals))
            elif _STRESS_HEADER in line or _DISP_HEADER in line:
                return i
            i += 1
        return i


def _norm(h: str) -> str:
    return h.strip().lower().replace("-", "_")


def _floats(tokens: list[str]) -> list[float]:
    out = []
    for t in tokens:
        try:
            out.append(float(t))
        except ValueError:
            break
    return out
