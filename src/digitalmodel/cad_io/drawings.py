# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""LibreDWG conversion and ezdxf drawing inspection."""

from __future__ import annotations

import shutil
import subprocess
import tempfile
from collections import Counter
from io import BytesIO
from pathlib import Path
from typing import Any

DWG_RELEASES = {
    "AC1001": "2.4", "AC1002": "2.5", "AC1003": "2.6", "AC1004": "R9",
    "AC1006": "R10", "AC1009": "R11/R12", "AC1012": "R13",
    "AC1014": "R14", "AC1015": "2000", "AC1018": "2004", "AC1021": "2007",
    "AC1024": "2010", "AC1027": "2013", "AC1032": "2018",
}


def read_dwg_version(path: str | Path) -> dict[str, str]:
    """Read the six-byte DWG tag; reject truncated or unrecognised headers.

    The release identifies a format family, rather than the application version
    that last edited the drawing. Reading the header does not validate a DWG.
    """
    with Path(path).open("rb") as stream:
        tag = stream.read(6).decode("ascii", errors="replace")
    if tag not in DWG_RELEASES:
        raise ValueError("unrecognised or truncated DWG version tag")
    return {"tag": tag, "release": DWG_RELEASES[tag]}


def _convert(tool: str, source: Path, target: Path, *options: str) -> Path:
    """Run one permitted converter with a bounded runtime and a private log."""
    executable = shutil.which(tool)
    if executable is None:
        raise RuntimeError(f"{tool} is absent; install LibreDWG 0.14")
    source, target = source.resolve(), target.resolve()
    target.parent.mkdir(parents=True, exist_ok=True)
    if target.exists():
        raise FileExistsError("converter output already exists; use a fresh directory")
    with (target.parent / f"{tool}.log").open("wb") as log:
        try:
            result = subprocess.run(  # no shell; the input is an absolute path
                [executable, *options, "-o", str(target), str(source)],
                cwd=target.parent, stdout=log, stderr=subprocess.STDOUT,
                # Pilot per-process time budget, not a format limit. Selection
                # basis: REPRODUCE.md, "Pilot settings and selection basis".
                timeout=120, check=False,
            )
        except subprocess.TimeoutExpired as exc:
            raise RuntimeError(f"{tool} timeout after 120 seconds; see tool log") from exc
    if result.returncode:
        raise RuntimeError(f"{tool} exited with code {result.returncode}; see tool log")
    if not target.is_file() or target.stat().st_size == 0:
        raise RuntimeError(f"{tool} produced no output; see tool log")
    return target


def dwg_to_dxf(path: str | Path, out_dir: str | Path) -> Path:
    """Convert a DWG to drawing.dxf without overwriting an existing result."""
    return _convert("dwg2dxf", Path(path), Path(out_dir) / "drawing.dxf")


def _read_dxf(path: str | Path) -> tuple[Any, dict[str, Any]]:
    """Use ezdxf recovery for malformed converter output, recording its audit."""
    import ezdxf
    from ezdxf import recover

    repairs = 0
    try:
        return ezdxf.readfile(path), {
            "used": False, "errors": 0, "fixes": 0, "null_endblk_handles_removed": 0,
        }
    except (ezdxf.DXFError, ValueError):
        try:
            doc, audit = recover.readfile(path)
        except ValueError:
            # LibreDWG 0.14 can emit an ENDBLK with entity handle 0. Null is
            # not a valid entity handle, and ezdxf's recovery also rejects it.
            # Omit only that invalid identity in memory so ezdxf assigns one;
            # preserve the input, geometry and all ownership/reference tags.
            lines = Path(path).read_bytes().splitlines(keepends=True)
            cleaned: list[bytes] = []
            kind = b""
            for index in range(0, len(lines), 2):
                pair = lines[index:index + 2]
                if len(pair) == 2:
                    code, value = (line.strip() for line in pair)
                    if code == b"0":
                        kind = value
                    if kind == b"ENDBLK" and code == b"5" and value == b"0":
                        repairs += 1
                        continue
                cleaned.extend(pair)
            if not repairs:
                raise
            doc, audit = recover.read(BytesIO(b"".join(cleaned)))
        return doc, {
            "used": True, "errors": len(audit.errors), "fixes": len(audit.fixes),
            "null_endblk_handles_removed": repairs,
        }


def dxf_to_json(path: str | Path) -> dict[str, Any]:
    """Extract layers, text, attributes, dimensions and entity counts.

    Counts cover each layout and block definition once, including attached
    attributes. Block instances are not expanded. Metadata retains the owning
    block name so model space and reusable definitions can be distinguished.
    """
    doc, recovery = _read_dxf(path)
    counts: Counter[str] = Counter()
    texts, attributes, dimensions = [], [], []
    for block in doc.blocks:
        for entity in block:
            kind = entity.dxftype()
            counts[kind] += 1
            context = {"owner": block.name, "layer": entity.dxf.layer}
            if kind in {"TEXT", "MTEXT", "ATTDEF"}:
                text = entity.plain_text() if kind == "MTEXT" else entity.dxf.text
                texts.append({**context, "type": kind, "text": text})
            elif kind == "INSERT":
                counts.update(attribute.dxftype() for attribute in entity.attribs)
                attributes.append({
                    **context, "block": entity.dxf.name,
                    "attributes": {a.dxf.tag: a.dxf.text for a in entity.attribs},
                })
            elif kind == "DIMENSION":
                measurement = entity.get_measurement()
                if not isinstance(measurement, (int, float)):
                    measurement = list(measurement)
                dimensions.append({
                    **context, "type": entity.dxf.dimtype,
                    "text": entity.dxf.get("text", ""), "measurement": measurement,
                })
    return {
        "dxf_version": doc.dxfversion,
        "recovery": recovery,
        "units_code": doc.units,
        "layers": [{"name": layer.dxf.name, "color": layer.dxf.color,
                    "linetype": layer.dxf.linetype} for layer in doc.layers],
        "text": texts, "block_attributes": attributes, "dimensions": dimensions,
        "entity_counts": dict(sorted(counts.items())),
        "entity_count_scope": "layouts and block definitions, including attached attributes",
    }


def dxf_to_svg(path: str | Path, out: str | Path) -> Path:
    """Render model space to a standalone SVG through ezdxf's drawing add-on."""
    from ezdxf.addons.drawing import Frontend, RenderContext, layout, svg

    doc, _ = _read_dxf(path)
    backend = svg.SVGBackend()
    Frontend(RenderContext(doc), backend).draw_layout(doc.modelspace(), finalize=True)
    target = Path(out)
    if target.resolve() == Path(path).resolve() or target.exists():
        raise FileExistsError("SVG output already exists; use a fresh path")
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(backend.get_string(layout.Page(0, 0)), encoding="utf-8")
    return target


def roundtrip_check(
    dwg: str | Path, out_dir: str | Path | None = None,
) -> dict[str, Any]:
    """Compare entity counts after a DWG or explicitly labelled DXF round trip.

    LibreDWG writes R2000 for the return DWG. An installed encoder that fails is
    a failure, rather than a reason to silently fall back to DXF. With no output
    directory, artifacts remain in a new directory under the system temp root.
    Count agreement does not establish geometric or visual equivalence.
    """
    folder = Path(out_dir) if out_dir is not None else Path(tempfile.mkdtemp(prefix="cad-"))
    first = dwg_to_dxf(dwg, folder / "first")
    before = dxf_to_json(first)["entity_counts"]
    if shutil.which("dxf2dwg"):
        encoded = _convert(
            "dxf2dwg", first, folder / "encoded" / "drawing.dwg", "--as", "r2000"
        )
        last = dwg_to_dxf(encoded, folder / "last")
        mode = "dwg-dxf-dwg-dxf"
    else:
        last = folder / "resaved.dxf"
        doc, _ = _read_dxf(first)
        doc.saveas(last)
        mode = "dwg-dxf-dxf"
    after = dxf_to_json(last)["entity_counts"]
    types = sorted(before.keys() | after.keys())
    difference = {kind: after.get(kind, 0) - before.get(kind, 0) for kind in types}
    # Matched counts divided by the larger count in each type penalises both
    # newly introduced and lost entities. Two empty drawings agree by definition.
    denominator = sum(max(before.get(k, 0), after.get(k, 0)) for k in types)
    matched = sum(min(before.get(k, 0), after.get(k, 0)) for k in types)
    return {
        "mode": mode, "counts_before": before, "counts_after": after,
        "delta": difference, "equal": before == after,
        "agreement_percent": 100.0 * matched / denominator if denominator else 100.0,
    }
