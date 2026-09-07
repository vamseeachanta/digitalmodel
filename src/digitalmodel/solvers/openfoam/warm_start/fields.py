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


def _processor_dirs(case: Path) -> tuple[Path, ...]:
    return tuple(sorted(
        (path for path in case.glob("processor[0-9]*") if path.is_dir()),
        key=lambda path: int(path.name.removeprefix("processor")),
    ))


def _backup_zero(zero: Path) -> None:
    cold = zero.with_name("0.cold")
    if zero.exists() and not cold.exists():
        shutil.copytree(zero, cold)


def restore_cold_restart(target: Path) -> None:
    processor_dirs = _processor_dirs(target)
    zeros = tuple(path / "0" for path in processor_dirs) if processor_dirs else (target / "0",)
    for zero in zeros:
        cold = zero.with_name("0.cold")
        if not cold.exists():
            continue
        if zero.exists():
            shutil.rmtree(zero)
        shutil.copytree(cold, zero)


def _copy_fields(source_time: Path, zero: Path) -> None:
    zero.mkdir(parents=True, exist_ok=True)
    for name in KEEP_FIELDS:
        src = source_time / name
        if not src.exists():
            raise FileNotFoundError(f"required warm field missing: {src}")
        shutil.copy2(src, zero / name)
    cleanup(zero)


def clean_restart(source_time: Path, target: Path) -> tuple[Path, ...]:
    """Install warm fields, preferring an identical decomposed layout."""
    source = source_time.parent
    source_processors = _processor_dirs(source)
    target_processors = _processor_dirs(target)
    if (source_processors and target_processors
            and tuple(path.name for path in source_processors)
            == tuple(path.name for path in target_processors)):
        zeros = tuple(path / "0" for path in target_processors)
        for source_processor, zero in zip(source_processors, zeros):
            _backup_zero(zero)
            _copy_fields(source_processor / source_time.name, zero)
        return zeros

    # A serial time can be partially reconstructed (for example, contain only p).
    # Reconstruct the complete allow-list before trusting it as a warm source.
    missing = [name for name in KEEP_FIELDS if not (source_time / name).exists()]
    if missing and source_processors:
        run(["reconstructPar", "-time", source_time.name, "-fields",
             "(" + " ".join(KEEP_FIELDS) + ")"], source)
    zero = target / "0"
    _backup_zero(zero)
    _copy_fields(source_time, zero)
    return (zero,)


def verify_warm_fields(zeros: tuple[Path, ...]) -> None:
    for zero in zeros:
        for name in KEEP_FIELDS:
            path = zero / name
            if not path.exists():
                raise FileNotFoundError(f"required warm field missing: {path}")
            raw = path.read_bytes()
            internal = re.search(rb"internalField\s+nonuniform\s+List", raw)
            if not internal:
                raise ValueError(f"warm field internalField is not nonuniform List: {path}")
            # A binary List can contain any byte, including '$'. Match a macro
            # only in the syntax of a complete dictionary entry line.
            macro_entry = re.search(
                rb"(?m)^[ \t]*[A-Za-z_]\w*[ \t]+[^\r\n;{}]*"
                rb"\$[A-Za-z_]\w*[^\r\n;{}]*;[ \t]*\r?$",
                raw,
            )
            if macro_entry:
                raise ValueError(f"warm field contains an unexpanded macro: {path}")


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


def _block_text(text: str, key: str) -> str:
    span = _balanced_block(text, key)
    if not span:
        raise ValueError(f"dictionary has no {key} block")
    return text[span[0]:span[1]]


def _entry(text: str, key: str) -> str:
    match = re.search(r"(?:^|[;{])\s*" + re.escape(key) + r"\s+([^;]+);", text)
    if not match:
        raise ValueError(f"dictionary entry {key} is missing")
    return re.sub(r"\s+", " ", match.group(1).strip())


def _python_expand(text: str) -> str:
    """Expand simple OpenFOAM ``$name`` variables when foamDictionary is absent."""
    definitions = {
        match.group(1): match.group(2).strip()
        for match in re.finditer(r"(?m)^\s*([A-Za-z_]\w*)\s+([^;{}]+);", text)
    }

    def resolve(value: str, trail: tuple[str, ...] = ()) -> str:
        def replacement(match: re.Match) -> str:
            name = match.group(1)
            if name in trail or name not in definitions:
                raise ValueError(f"cannot expand OpenFOAM macro ${name}")
            return resolve(definitions[name], (*trail, name))
        previous = None
        while "$" in value and value != previous:
            previous = value
            value = re.sub(r"\$([A-Za-z_]\w*)", replacement, value)
        return value

    return re.sub(r"\$([A-Za-z_]\w*)", lambda match: resolve(definitions.get(
        match.group(1), match.group(0)), (match.group(1),)), text)


def _expanded_field(path: Path) -> str:
    foam_dictionary = shutil.which("foamDictionary")
    if foam_dictionary:
        completed = subprocess.run(
            [foam_dictionary, str(path), "-expand"], cwd=path.parent.parent,
            check=True, capture_output=True, text=True,
        )
        return completed.stdout
    return _python_expand(path.read_text())


def _canonical_literal(value: str) -> str:
    value = re.sub(r"\s+", " ", value.strip())
    value = re.sub(r"\(\s+", "(", value)
    value = re.sub(r"\s+\)", ")", value)
    return re.sub(r"(?<![\w.])([+-])\s+(?=\d|\.)", r"\1", value)


def _patches(boundary: str) -> list[tuple[str, str]]:
    outer = boundary[boundary.find("{") + 1:boundary.rfind("}")]
    patches = []
    position = 0
    pattern = re.compile(r"(?m)^\s*([\w.-]+)\s*\{")
    while match := pattern.search(outer, position):
        span = _balanced_block(outer[match.start():], match.group(1))
        if not span:
            raise ValueError(f"unbalanced boundary patch {match.group(1)}")
        end = match.start() + span[1]
        patches.append((match.group(1), outer[match.start():end]))
        position = end
    return patches


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


def _change_dictionary_dict(target: Path) -> tuple[Path, dict[tuple[str, str], str]]:
    lines = ["""FoamFile
{
    version 2.0;
    format ascii;
    class dictionary;
    object changeDictionaryDict;
}

"""]
    intended: dict[tuple[str, str], str] = {}
    for field in ("U", "k", "omega"):
        source = target / "0.orig" / field
        if not source.exists():
            continue
        expanded = _expanded_field(source)
        boundary = _block_text(expanded, "boundaryField")
        changes: dict[str, dict[str, str]] = {}
        for patch, block in _patches(boundary):
            patch_changes = changes.setdefault(patch, {})
            patch_type = _entry(block, "type")
            if field == "U" and patch == "inlet":
                speed = abs(float(_entry(expanded, "Umean")))
                patch_changes["value"] = f"uniform (-{speed:g} 0 0)"
            if field == "U" and patch_type == "outletPhaseMeanVelocity":
                patch_changes["Umean"] = _canonical_literal(_entry(block, "Umean"))
                patch_changes["value"] = _canonical_literal(_entry(block, "value"))
            if field in {"k", "omega"} and patch == "inlet":
                patch_changes["value"] = _canonical_literal(_entry(block, "value"))
            if patch_type == "inletOutlet":
                patch_changes["inletValue"] = _canonical_literal(_entry(block, "inletValue"))
            if not patch_changes:
                changes.pop(patch)
        if not changes:
            continue
        lines += [f"{field}\n{{\n    boundaryField\n    {{\n"]
        for patch, entries in changes.items():
            lines += [f"        {patch}\n        {{\n"]
            for key, value in entries.items():
                if "$" in value or value.startswith("#"):
                    raise ValueError(f"{source}: {patch}.{key} did not expand to a literal")
                lines.append(f"            {key} {value};\n")
                intended[(field, f"boundaryField.{patch}.{key}")] = value
            lines.append("        }\n")
        lines += ["    }\n}\n\n"]
    path = target / "system" / "changeDictionaryDict"
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(lines))
    return path, intended


def _validate_dictionary(path: Path) -> None:
    text = path.read_text()
    required = ("FoamFile", "version 2.0;", "format ascii;", "class dictionary;",
                "object changeDictionaryDict;")
    if any(item not in text for item in required) or text.count("{") != text.count("}"):
        raise ValueError(f"invalid {path}")
    if "$" in text or "#include" in text:
        raise ValueError(f"{path} contains an unexpanded macro or include")
    foam_dictionary = shutil.which("foamDictionary")
    if foam_dictionary:
        subprocess.run([foam_dictionary, str(path), "-expand"], cwd=path.parent.parent,
                       check=True, capture_output=True, text=True)


def _normal_literal(value: str) -> str:
    return _canonical_literal(value)


def _verify_changes(target: Path, intended: dict[tuple[str, str], str],
                    zeros: tuple[Path, ...]) -> None:
    foam_dictionary = shutil.which("foamDictionary")
    if not foam_dictionary:
        raise RuntimeError("foamDictionary is required to verify changeDictionary output")
    for zero in zeros:
        for (field, entry), expected in intended.items():
            completed = subprocess.run(
                [foam_dictionary, str(zero / field), "-entry", entry, "-value"],
                cwd=target, check=True, capture_output=True, text=True,
            )
            actual = _normal_literal(completed.stdout)
            if actual != _normal_literal(expected):
                raise RuntimeError(f"changeDictionary verification failed for {zero / field}.{entry}: "
                                   f"expected {expected!r}, got {actual!r}")


def rewrite_speed_fields(target: Path, dry_run=False) -> None:
    dictionary, intended = _change_dictionary_dict(target)
    _validate_dictionary(dictionary)
    processors = _processor_dirs(target)
    zeros = tuple(path / "0" for path in processors) if processors else (target / "0",)
    command = ["changeDictionary", "-time", "0", "-enableFunctionEntries"]
    if processors:
        command = ["mpirun", "-np", str(len(processors)), *command, "-parallel"]
    run(command, target, dry_run=dry_run)
    if not dry_run:
        _verify_changes(target, intended, zeros)
        verify_warm_fields(zeros)


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
