"""Fail-closed admissibility checks from section 8.2."""
from __future__ import annotations

import hashlib
import json
import math
import re
from dataclasses import dataclass
from pathlib import Path

from ..convergence_audit import audit_one


@dataclass(frozen=True)
class GateCheck:
    identifier: str
    passed: bool | None
    detail: str
    state: str | None = None


@dataclass(frozen=True)
class GateVerdict:
    checks: tuple[GateCheck, ...]

    @property
    def passed(self):
        return all(c.passed is not False for c in self.checks)

    @property
    def first_failure(self):
        return next((c.identifier for c in self.checks if c.passed is False), None)

    def render(self):
        def state(check):
            return check.state or ("PENDING" if check.passed is None else
                                   ("PASS" if check.passed else "REFUSE"))

        rows = [f"  {c.identifier} {state(c)}: {c.detail}" for c in self.checks]
        return "admissibility verdict: " + ("PASS" if self.passed else f"REFUSE ({self.first_failure})") + "\n" + "\n".join(rows)


def _provenance(case: Path) -> dict:
    path = case / "case_provenance.json"
    return json.loads(path.read_text()) if path.exists() else {}


def _dig(data: dict, *names, default=None):
    for name in names:
        value = data
        for part in name.split("."):
            value = value.get(part) if isinstance(value, dict) else None
        if value is not None:
            return value
    return default


def case_speed(case: Path) -> float:
    p = _provenance(case)
    value = _dig(p, "speed", "speed_mps", "physics.speed_mps", "hull.speed_mps", "Umean")
    if value is not None:
        return abs(float(value))
    for path in (case / "0.orig" / "U", case / "0" / "U"):
        if path.exists():
            match = re.search(r"\bUmean\s+([-+0-9.eE]+)", path.read_text(errors="ignore"))
            if match:
                return abs(float(match.group(1)))
    raise ValueError(f"cannot determine speed for {case}")


def rank_count(case: Path) -> int:
    processors = list(case.glob("processor[0-9]*"))
    if processors:
        return len(processors)
    path = case / "system" / "decomposeParDict"
    if path.exists():
        match = re.search(r"numberOfSubdomains\s+(\d+)", path.read_text())
        if match:
            return int(match.group(1))
    return 1


def mesh_digest(case: Path) -> str:
    digest = hashlib.sha256()
    mesh = case / "constant" / "polyMesh"
    for name in ("points", "faces", "owner", "neighbour", "boundary"):
        path = mesh / name
        if path.exists():
            digest.update(name.encode()); digest.update(path.read_bytes())
    return digest.hexdigest()


def _has_mesh(case: Path) -> bool:
    return (case / "constant" / "polyMesh" / "owner").is_file()


def _mesh_cells(case: Path) -> int | None:
    owner = case / "constant" / "polyMesh" / "owner"
    if not owner.is_file():
        return None
    match = re.search(r"\bnCells\s*:\s*(\d+)", owner.read_text(errors="ignore"))
    return int(match.group(1)) if match else None


def _mesh_identity(case: Path) -> str:
    mesh = case / "constant" / "polyMesh"
    link = f", link={mesh.resolve()}" if mesh.is_symlink() else ""
    return f"sha256={mesh_digest(case)[:12]}{link}"


def _level_class(value) -> str | None:
    if value is None:
        return None
    text = str(value).strip().lower()
    aliases = {
        "r3": "80-class", "l3": "80-class", "80": "80-class", "80-class": "80-class",
        "r2": "40-class", "l2": "40-class", "40": "40-class", "40-class": "40-class",
        "r1": "20-class", "l1": "20-class", "20": "20-class", "20-class": "20-class",
    }
    if text in aliases:
        return aliases[text]
    try:
        number = float(text)
    except ValueError:
        return text
    return "80-class" if number >= 60 else ("40-class" if number >= 30 else "20-class")


def _finest_refinement_class(provenance: dict) -> str | None:
    levels = _dig(provenance, "refinement.levels")
    if levels is None:
        return None
    values = levels if isinstance(levels, list) else list(levels.values()) if isinstance(levels, dict) else [levels]
    resolutions = []
    for item in values:
        if isinstance(item, dict):
            item = _dig(item, "cells_per_wavelength", "cells-per-wavelength", "cpw", "resolution")
        try:
            resolutions.append(float(item))
        except (TypeError, ValueError):
            continue
    return _level_class(max(resolutions)) if resolutions else None


def _normal(path: Path) -> str | None:
    return re.sub(r"\s+", " ", path.read_text(errors="ignore")).strip() if path.exists() else None


def _foam_entries(path: Path) -> dict[str, str] | None:
    """Return a formatting-, comment-, and FoamFile-header-independent dictionary."""
    if not path.exists():
        return None
    text = re.sub(r"/\*.*?\*/", " ", path.read_text(errors="ignore"), flags=re.S)
    text = re.sub(r"//[^\n]*", " ", text)
    header = re.search(r"\bFoamFile\s*\{", text)
    if header:
        start = text.find("{", header.start())
        depth = 0
        for pos in range(start, len(text)):
            depth += text[pos] == "{"
            depth -= text[pos] == "}"
            if depth == 0:
                text = text[:header.start()] + text[pos + 1:]
                break
    tokens = re.findall(r'"(?:\\.|[^"\\])*"|[{};()]|[^\s{};()]+', text)
    entries: dict[str, str] = {}
    stack: list[str] = []
    index = 0
    while index < len(tokens):
        token = tokens[index]
        if token == "}":
            if stack:
                stack.pop()
            index += 1
            continue
        if index + 1 < len(tokens) and tokens[index + 1] == "{":
            stack.append(token)
            index += 2
            continue
        if token in {"{", ";"}:
            index += 1
            continue
        end = index + 1
        while end < len(tokens) and tokens[end] not in {";", "{", "}"}:
            end += 1
        if end < len(tokens) and tokens[end] == ";":
            entries[".".join((*stack, token))] = " ".join(tokens[index + 1:end])
            index = end + 1
        else:
            index += 1
    return entries


def _numeric_dictionary_match(source: Path, target: Path) -> tuple[bool, str]:
    for name in ("fvSchemes", "fvSolution"):
        left = _foam_entries(source / "system" / name)
        right = _foam_entries(target / "system" / name)
        if left == right:
            continue
        for key in sorted(set(left or {}) | set(right or {})):
            if not _foam_values_equal((left or {}).get(key), (right or {}).get(key)):
                return False, (f"{name} first difference at {key}: "
                               f"source={(left or {}).get(key)!r}, target={(right or {}).get(key)!r}")
        continue
    return True, "fvSchemes/fvSolution match semantically"


def _numeric_value(value: str | None) -> tuple[float, ...] | None:
    if value is None:
        return None
    stripped = value.strip()
    if stripped.startswith("(") and stripped.endswith(")"):
        stripped = stripped[1:-1].strip()
    elif " " in stripped:
        return None
    parts = stripped.split()
    try:
        return tuple(float(part) for part in parts)
    except ValueError:
        return None


def _foam_values_equal(left: str | None, right: str | None) -> bool:
    if left == right:
        return True
    left_numbers, right_numbers = _numeric_value(left), _numeric_value(right)
    return (left_numbers is not None and right_numbers is not None and
            len(left_numbers) == len(right_numbers) and
            all(math.isclose(a, b, rel_tol=1e-12, abs_tol=0.0)
                for a, b in zip(left_numbers, right_numbers)))


def _wall_signature(path: Path) -> str | None:
    """Compare wall treatment while deliberately excluding speed-dependent inlet data."""
    if not path.exists():
        return None
    text = path.read_text(errors="ignore")
    boundary = re.search(r"boundaryField\s*\{(.*)\}\s*$", text, re.S)
    text = boundary.group(1) if boundary else text
    blocks = []
    for match in re.finditer(r"(?m)^\s*([\w.-]+)\s*\{", text):
        patch = match.group(1).lower()
        if not any(token in patch for token in ("hull", "rudder", "boss", "wall")):
            continue
        start, depth, end = text.find("{", match.start()), 0, None
        for pos in range(start, len(text)):
            depth += text[pos] == "{"; depth -= text[pos] == "}"
            if depth == 0:
                end = pos + 1; break
        if end:
            blocks.append(re.sub(r"\bvalue\s+[^;]+;", "", text[match.start():end]))
    # Minimal test cases may not name a hull patch; BC types still expose a mismatch.
    if not blocks:
        blocks = re.findall(r"\btype\s+[\w.-]+\s*;", text)
    return re.sub(r"\s+", " ", " ".join(blocks)).strip()


def _log_settled(source: Path) -> tuple[bool, str]:
    logs = sorted(source.glob("log*"))
    ended = any("End" in p.read_text(errors="ignore") and "FOAM FATAL" not in p.read_text(errors="ignore") for p in logs)
    try:
        audit = audit_one(source.name, source, 400, 1.0, 1.0)
        verdict = audit.get("verdict")
    except Exception as exc:  # defensive: audit is a numerical fit
        return False, str(exc)
    audit_ok = verdict in {"settled", "extrapolable"}
    fit, cycle = audit.get("fit_total"), audit.get("cycle_total")
    fit_cycle_delta = (abs(fit - cycle) / max(abs(cycle), 1e-9)
                       if fit is not None and cycle is not None else None)
    fit_cycle_ok = fit_cycle_delta is not None and fit_cycle_delta <= .02
    ok = ended and (audit_ok or fit_cycle_ok)
    criterion = ("audit verdict" if audit_ok else
                 "fit-vs-cycle agreement" if fit_cycle_ok else "none")
    delta = "unavailable" if fit_cycle_delta is None else f"{100 * fit_cycle_delta:.3g}%"
    return ok, (f"criterion={criterion}, audit={verdict}, fit-vs-cycle={delta}, "
                f"clean End={ended}")


def evaluate(source: Path | None, target: Path, hop: str, *, max_du=.10,
             ranks: int | None = None, level: str | None = None,
             source_level: str | None = None,
             allow_pending_mesh: bool = False) -> GateVerdict:
    checks: list[GateCheck] = []
    if hop in {"speed", "geometry"}:
        if source is None:
            return GateVerdict((GateCheck("A1", False, "source required"),))
        ok, detail = _log_settled(source)
        checks.append(GateCheck("A1", ok, detail))
        # Comparing complete turbulence templates also compares types and coefficients.
        names = ("nut", "omega", "k")
        wall_ok = all(_wall_signature(source / "0.orig" / n) == _wall_signature(target / "0.orig" / n) for n in names)
        sp, tp = _provenance(source), _provenance(target)
        for key in ("yplus_target", "wall_function", "wall_treatment"):
            if key in sp or key in tp:
                wall_ok &= sp.get(key) == tp.get(key)
        checks.append(GateCheck("A2", wall_ok, "wall turbulence dictionaries/provenance match"))
        numeric, numeric_detail = _numeric_dictionary_match(source, target)
        checks.append(GateCheck("A3", numeric, numeric_detail))
        frame = all(_normal(source / p) == _normal(target / p) for p in
                    (Path("constant/hRef"), Path("constant/g"), Path("system/blockMeshDict")))
        checks.append(GateCheck("A4", frame, "reference-frame dictionaries match"))
        sr, tr = rank_count(source), ranks or rank_count(target)
        serial_fields = any((source / p).is_dir() and all((source / p / f).exists() for f in
                            ("alpha.water", "U", "p_rgh", "k", "omega", "nut"))
                            for p in source.iterdir() if p.is_dir() and p.name.replace(".", "", 1).isdigit())
        decomp_ok = sr == tr or serial_fields
        checks.append(GateCheck("A5", decomp_ok, f"source ranks={sr}, target ranks={tr}, reconstructed={serial_fields}"))
        try:
            u1, u2 = case_speed(source), case_speed(target); du = abs(u2-u1)/u1
            source_mesh, target_mesh = _has_mesh(source), _has_mesh(target)
            if not target_mesh and allow_pending_mesh:
                hop_ok = None
                detail = "target mesh is not staged (no constant/polyMesh or mesh-store link); comparison deferred"
            elif not source_mesh or not target_mesh:
                hop_ok = False
                detail = f"mesh unavailable: source={source_mesh}, target={target_mesh}"
            else:
                same = mesh_digest(source) == mesh_digest(target)
                hop_ok = (hop == "speed" and same and du <= max_du) or (hop == "geometry" and not same and du <= 1e-6)
                detail = (f"same_mesh={same}, source {_mesh_identity(source)}, target {_mesh_identity(target)}, "
                          f"U1={u1:g}, U2={u2:g}, |dU/U|={du:.4f}")
        except Exception as exc:
            hop_ok, detail, du = False, str(exc), 0
        checks.append(GateCheck("A6", hop_ok, detail))
        source_class = _level_class(source_level or _dig(sp, "mesh_level", "mesh.level"))
        source_reason = "explicit/provenance"
        if source_class is None:
            source_class = _finest_refinement_class(sp)
            source_reason = "refinement.levels finest resolution"
        target_class = _level_class(level or _dig(tp, "mesh_level", "mesh.level"))
        source_cells, target_cells = _mesh_cells(source), _mesh_cells(target)
        cells_match = (source_cells is not None and target_cells is not None and target_cells > 0
                       and abs(source_cells - target_cells) / target_cells <= .10)
        if source_class is None and target_class is not None and cells_match:
            source_class = target_class
            source_reason = f"mesh cells {source_cells}/{target_cells} within 10%"
        checks.append(GateCheck("A7", source_class == target_class and source_class is not None,
                                f"source={source_class} ({source_reason}), target={target_class}"))
    else:
        checks.extend(GateCheck(i, True, "not applicable to initial-field mode")
                      for i in ("A1", "A5", "A6"))
    pcorr = target / "system" / "fvSolution"
    pcorr_ok = pcorr.exists() and re.search(r"pcorr(?:\.\*)?", pcorr.read_text()) is not None
    checks.append(GateCheck("A9", pcorr_ok, 'fvSolution contains "pcorr.*"'))
    return GateVerdict(tuple(checks))
