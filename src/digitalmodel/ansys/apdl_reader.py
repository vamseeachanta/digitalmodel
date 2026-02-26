# ABOUTME: APDL .inp file reader — extracts materials and beam/rod sections
# ABOUTME: Pure text parsing, no ANSYS software dependency

"""
APDL Reader
===========

Parses ANSYS Parametric Design Language (APDL) .inp files to extract
material definitions and beam/rod section data.

Recognised APDL commands:
    MP,EX,<id>,<val>     → elastic_modulus_mpa
    MP,NUXY,<id>,<val>   → poissons_ratio
    MP,DENS,<id>,<val>   → density_kg_mm3
    SECTYPE,<id>,<type>,<subtype>[,<name>]
    SECDATA,<area>,<Iz>,<Iy>,<J>,...

Lines beginning with ! are APDL comments and are skipped.
"""

from __future__ import annotations

from pathlib import Path

from digitalmodel.ansys.models import APDLMaterial, APDLSection

# Map MP label → field name on APDLMaterial
_MP_FIELD_MAP: dict[str, str] = {
    "EX": "elastic_modulus_mpa",
    "NUXY": "poissons_ratio",
    "DENS": "density_kg_mm3",
}


class APDLReader:
    """Parse APDL .inp files for material and section data."""

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def parse_materials(self, filepath: str | Path) -> list[APDLMaterial]:
        """Parse MP commands from an APDL .inp file.

        Returns a list of APDLMaterial objects, one per unique material ID.
        Properties missing from the file are left as None.
        """
        filepath = Path(filepath)
        materials: dict[int, APDLMaterial] = {}

        for line in self._iter_lines(filepath):
            parts = self._split_apdl(line)
            if not parts or not parts[0] or parts[0].upper() != "MP":
                continue
            if len(parts) < 4:
                continue
            label = parts[1].upper()
            if label not in _MP_FIELD_MAP:
                continue
            try:
                mat_id = int(parts[2])
                value = float(parts[3])
            except ValueError:
                continue

            mat = materials.setdefault(mat_id, APDLMaterial(mat_id=mat_id))
            setattr(mat, _MP_FIELD_MAP[label], value)

        return list(materials.values())

    def parse_sections(self, filepath: str | Path) -> list[APDLSection]:
        """Parse SECTYPE/SECDATA blocks from an APDL .inp file.

        Each SECTYPE command starts a new section record; the following
        SECDATA line provides cross-section dimensions.  Returns a list
        of APDLSection objects.
        """
        filepath = Path(filepath)
        sections: dict[int, APDLSection] = {}
        pending_id: int | None = None

        for line in self._iter_lines(filepath):
            parts = self._split_apdl(line)
            if not parts or not parts[0]:
                continue
            cmd = parts[0].upper()

            if cmd == "SECTYPE":
                if len(parts) < 3:
                    continue
                try:
                    sec_id = int(parts[1])
                except ValueError:
                    continue
                sec_type = parts[2].upper() if len(parts) > 2 else ""
                sec_sub = parts[3].upper() if len(parts) > 3 else ""
                sec = APDLSection(
                    section_id=sec_id,
                    section_type=sec_type,
                    section_subtype=sec_sub,
                )
                sections[sec_id] = sec
                pending_id = sec_id

            elif cmd == "SECDATA":
                if pending_id is None or pending_id not in sections:
                    continue
                sec = sections[pending_id]
                vals = []
                for p in parts[1:]:
                    if p == "":
                        vals.append(None)
                    else:
                        try:
                            vals.append(float(p))
                        except ValueError:
                            vals.append(None)
                if len(vals) >= 1:
                    sec.area_mm2 = vals[0]
                if len(vals) >= 2:
                    sec.iz_mm4 = vals[1]
                if len(vals) >= 3:
                    sec.iy_mm4 = vals[2]
                if len(vals) >= 4:
                    sec.j_mm4 = vals[3]
                pending_id = None

        return list(sections.values())

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _iter_lines(filepath: Path):
        """Yield non-comment, non-empty lines from an APDL file."""
        with filepath.open(encoding="utf-8", errors="replace") as fh:
            for raw in fh:
                line = raw.strip()
                if not line or line.startswith("!"):
                    continue
                # Strip inline comments
                if "!" in line:
                    line = line[: line.index("!")].strip()
                if line:
                    yield line

    @staticmethod
    def _split_apdl(line: str) -> list[str]:
        """Split an APDL command line by comma, strip whitespace.

        Empty fields (e.g. ``SECDATA,1.0,,2.0``) are preserved as ``""``
        so that positional APDL arguments are not shifted.
        """
        return [p.strip() for p in line.split(",")]
