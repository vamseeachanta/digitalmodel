"""Fail-closed admissibility checks from section 8.2."""
from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass
from pathlib import Path

from ..convergence_audit import audit_one


@dataclass(frozen=True)
class GateCheck:
    identifier: str
    passed: bool
    detail: str


@dataclass(frozen=True)
class GateVerdict:
    checks: tuple[GateCheck, ...]

    @property
    def passed(self):
        return all(c.passed for c in self.checks)

    @property
    def first_failure(self):
        return next((c.identifier for c in self.checks if not c.passed), None)

    def render(self):
        rows = [f"  {c.identifier} {'PASS' if c.passed else 'REFUSE'}: {c.detail}" for c in self.checks]
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


def _normal(path: Path) -> str | None:
    return re.sub(r"\s+", " ", path.read_text(errors="ignore")).strip() if path.exists() else None


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
    return ended and verdict in {"settled", "extrapolable"}, f"audit={verdict}, clean End={ended}"


def evaluate(source: Path | None, target: Path, hop: str, *, max_du=.10,
             ranks: int | None = None, level: str | None = None) -> GateVerdict:
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
        numeric = all(_normal(source / "system" / n) == _normal(target / "system" / n)
                      for n in ("fvSchemes", "fvSolution"))
        checks.append(GateCheck("A3", numeric, "fvSchemes/fvSolution match"))
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
            same = mesh_digest(source) == mesh_digest(target)
            hop_ok = (hop == "speed" and same and du <= max_du) or (hop == "geometry" and not same and du <= 1e-6)
            detail = f"same_mesh={same}, U1={u1:g}, U2={u2:g}, |dU/U|={du:.4f}"
        except Exception as exc:
            hop_ok, detail, du = False, str(exc), 0
        checks.append(GateCheck("A6", hop_ok, detail))
        source_level = _dig(sp, "mesh_level", "mesh.level")
        target_level = level or _dig(tp, "mesh_level", "mesh.level")
        checks.append(GateCheck("A7", source_level == target_level and source_level is not None,
                                f"source={source_level}, target={target_level}"))
    else:
        checks.extend(GateCheck(i, True, "not applicable to initial-field mode")
                      for i in ("A1", "A5", "A6"))
    pcorr = target / "system" / "fvSolution"
    pcorr_ok = pcorr.exists() and re.search(r"pcorr(?:\.\*)?", pcorr.read_text()) is not None
    checks.append(GateCheck("A9", pcorr_ok, 'fvSolution contains "pcorr.*"'))
    return GateVerdict(tuple(checks))
