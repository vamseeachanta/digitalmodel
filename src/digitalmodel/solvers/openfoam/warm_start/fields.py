"""OpenFOAM field preparation; external utilities are deliberately subprocess boundaries."""
from __future__ import annotations

import re
import shutil
import subprocess
from pathlib import Path

import numpy as np

KEEP_FIELDS = ("alpha.water", "U", "p_rgh", "k", "omega", "nut")
DROP_FIELDS = ("phi", "alphaPhi0.water", "rDeltaT", "p")


def run(command: list[str], cwd: Path, dry_run=False) -> None:
    print("COMMAND:", " ".join(command))
    if not dry_run:
        subprocess.run(command, cwd=cwd, check=True)


def clean_restart(source_time: Path, target: Path) -> None:
    zero = target / "0"
    if zero.exists() and not (target / "0.cold").exists():
        shutil.copytree(zero, target / "0.cold")
    zero.mkdir(parents=True, exist_ok=True)
    for name in KEEP_FIELDS:
        src = source_time / name
        if not src.exists():
            raise FileNotFoundError(f"required warm field missing: {src}")
        shutil.copy2(src, zero / name)
    cleanup(zero)


def cleanup(zero: Path, *, keep_phi=False) -> None:
    for name in DROP_FIELDS + (() if keep_phi else ("phi",)):
        path = zero / name
        if path.exists():
            path.unlink()
    for path in zero.glob("*.unmapped"):
        path.unlink()
    if (zero / "uniform").exists():
        shutil.rmtree(zero / "uniform")


def _balanced_block(text: str, key: str) -> tuple[int, int] | None:
    match = re.search(r"(?m)^\s*" + re.escape(key) + r"\s*\{", text)
    if not match:
        return None
    start = text.find("{", match.start()); depth = 0
    for pos in range(start, len(text)):
        depth += text[pos] == "{"; depth -= text[pos] == "}"
        if depth == 0:
            return match.start(), pos + 1
    return None


def _boundary(text: str) -> str:
    span = _balanced_block(text, "boundaryField")
    if not span:
        raise ValueError("field has no boundaryField")
    return text[span[0]:span[1]]


def _replace_boundaries(path: Path, template: Path) -> None:
    raw = path.read_bytes()
    # Binary internal fields can contain arbitrary bytes. changeDictionary is the
    # supported v2312 boundary editor and preserves the internal list.
    if b"format      binary" in raw or b"format binary" in raw:
        return
    text, wanted = raw.decode(), template.read_text()
    old_span = _balanced_block(text, "boundaryField")
    if not old_span:
        raise ValueError(f"{path}: boundaryField missing")
    path.write_text(text[:old_span[0]] + _boundary(wanted) + text[old_span[1]:])


def _change_dictionary_dict(target: Path) -> Path:
    lines = ["dictionaryReplacement\n{\n"]
    for field in ("U", "k", "omega"):
        source = target / "0.orig" / field
        if source.exists():
            lines += [f"  {field}\n  {{\n    ", _boundary(source.read_text()), "\n  }\n"]
    lines.append("}\n")
    path = target / "system" / "changeDictionaryDict"
    path.parent.mkdir(parents=True, exist_ok=True); path.write_text("".join(lines))
    return path


def rewrite_speed_fields(target: Path, dry_run=False) -> None:
    for field in ("U", "k", "omega"):
        current, template = target / "0" / field, target / "0.orig" / field
        if current.exists() and template.exists():
            _replace_boundaries(current, template)
    _change_dictionary_dict(target)
    run(["changeDictionary", "-time", "0"], target, dry_run=dry_run)


def reset_control(target: Path, n_cold: int) -> None:
    path = target / "system" / "controlDict"
    text = path.read_text()
    values = {"startFrom": "startTime", "startTime": "0", "endTime": str(n_cold),
              "stopAt": "endTime", "runTimeModifiable": "true"}
    for key, value in values.items():
        pattern = rf"(?m)^\s*{key}\s+[^;]+;"
        replacement = f"{key} {value};"
        text, count = re.subn(pattern, replacement, text)
        if not count:
            text += "\n" + replacement + "\n"
    path.write_text(text)


def prepare_geometry(source: Path, source_time: str, target: Path, ranks: int | None,
                     dry_run=False) -> None:
    command = ["mapFieldsPar", str(source), "-sourceTime", source_time, "-consistent",
               "-mapMethod", "cellVolumeWeight", "-fields", "(" + " ".join(KEEP_FIELDS) + ")"]
    if ranks:
        command = ["mpirun", "-np", str(ranks), *command, "-parallel"]
    run(command, target, dry_run)
    if not dry_run:
        cleanup(target / "0")


def prepare_potential(target: Path, dry_run=False) -> None:
    solution = target / "system" / "fvSolution"
    original = solution.read_text()
    additions = """
Phi
{
    solver PCG;
    preconditioner DIC;
    tolerance 1e-08;
    relTol 0;
}
potentialFlow
{
    nNonOrthogonalCorrectors 5;
    PhiRefCell 0;
    PhiRefValue 0;
}
"""
    if dry_run:
        print("FV_SOLUTION: Phi solver; potentialFlow { nNonOrthogonalCorrectors 5; PhiRefCell 0; PhiRefValue 0; }")
        run(["potentialFoam", "-writephi"], target, True)
        return
    solution.write_text(original + additions)
    try:
        run(["potentialFoam", "-writephi"], target)
    finally:
        solution.write_text(original)


def _table(path: Path) -> np.ndarray:
    return np.genfromtxt(path, delimiter=",", names=True)


def prepare_analytic(target: Path, eta_file: Path, velocity_file: Path, dry_run=False) -> None:
    run(["postProcess", "-func", "writeCellCentres", "-time", "0"], target, dry_run)
    if dry_run:
        return
    centres = np.column_stack([_read_internal(target / "0" / f"C{x}") for x in "xyz"])
    eta, velocity = _table(eta_file), _table(velocity_file)
    # Nearest neighbour is deterministic and is the specified fallback for irregular tables.
    xy = np.column_stack([eta[eta.dtype.names[0]], eta[eta.dtype.names[1]]])
    ei = np.argmin(((centres[:, None, :2] - xy[None, :, :]) ** 2).sum(axis=2), axis=1)
    surface = eta[eta.dtype.names[2]][ei]
    xyz = np.column_stack([velocity[n] for n in velocity.dtype.names[:3]])
    ui = np.argmin(((centres[:, None, :] - xyz[None, :, :]) ** 2).sum(axis=2), axis=1)
    perturbation = np.column_stack([velocity[n] for n in velocity.dtype.names[3:6]])[ui]
    perturbation[centres[:, 2] > surface] = 0
    _replace_internal(target / "0" / "alpha.water", (centres[:, 2] < surface).astype(float), False)
    _replace_internal(target / "0" / "U", perturbation, True)
    for path in (target / "0").glob("C*"):
        path.unlink()


def _replace_internal(path: Path, values: np.ndarray, vector: bool) -> None:
    text = path.read_text()
    kind = "vector" if vector else "scalar"
    rendered = "\n".join("(" + " ".join(map(str, row)) + ")" if vector else str(v)
                         for row, v in zip(values if vector else np.zeros((len(values), 1)), values))
    block = f"internalField nonuniform List<{kind}>\n{len(values)}\n(\n{rendered}\n);"
    text, n = re.subn(r"internalField\s+.*?;", block, text, count=1, flags=re.S)
    if not n:
        raise ValueError(f"{path}: internalField missing")
    path.write_text(text)


def _read_internal(path: Path) -> np.ndarray:
    text = path.read_text()
    match = re.search(r"internalField\s+nonuniform\s+List<(?:scalar|vector)>\s+(\d+)\s*\((.*?)\)\s*;", text, re.S)
    if not match:
        uniform = re.search(r"internalField\s+uniform\s+(\([^)]*\)|[-+0-9.eE]+)\s*;", text)
        if not uniform:
            raise ValueError(f"{path}: readable internalField missing")
        value = np.fromstring(uniform.group(1).strip("()"), sep=" ")
        return value[0] if len(value) == 1 else value
    count = int(match.group(1)); body = match.group(2)
    vectors = re.findall(r"\(([^()]*)\)", body)
    values = np.array([np.fromstring(v, sep=" ") for v in vectors]) if vectors else np.fromstring(body, sep=" ")
    if len(values) != count:
        raise ValueError(f"{path}: expected {count} internal values, found {len(values)}")
    return values


def resharpen_alpha(path: Path, expected_volume: float, cell_volumes=None) -> float:
    """Clip/reinitialise alpha at its 0.5 iso-surface and enforce the 0.5% mass gate."""
    alpha = np.asarray(_read_internal(path), dtype=float)
    sharp = (np.clip(alpha, 0, 1) >= .5).astype(float)
    volumes = np.ones(len(sharp)) if cell_volumes is None else np.asarray(cell_volumes)
    volume = float(np.sum(sharp * volumes))
    if abs(volume - expected_volume) / max(abs(expected_volume), 1e-12) > .005:
        raise ValueError(f"A8 alpha mass differs by {abs(volume-expected_volume)/abs(expected_volume):.3%} (>0.5%)")
    _replace_internal(path, sharp, False)
    return volume
