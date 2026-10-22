"""Text-only evidence audit for the floating-wind worked example.

No YAML evaluation, embedded-code execution, solver import or dependency fetch
is performed. Exit success means documentation consistency, never run readiness.
"""

from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
import os
import posixpath
from pathlib import Path, PureWindowsPath
import re
import stat
from html.parser import HTMLParser
from urllib.parse import unquote

DOMAIN = Path("docs/domains/orcaflex")
DOCUMENTS = DOMAIN / "wind/fowt-worked-example"
# Source identities: issue 2423, Models; repository model headers.
MODELS = {
    "L02": DOMAIN / "examples/raw/L02/L02 OC4 Semi-sub.yml",
    "K03": DOMAIN / "examples/raw/K03/K03 15MW semi-sub FOWT.yml",
}
LIBRARIES = {
    "L02": "l02_oc4_semi_sub",
    "K03": "k03_15mw_semi_sub_fowt",
}
# Keys observed in the two raw models and their modular masters. This is not a
# general OrcaFlex parser; newly introduced dependency keys require review.
REFERENCE = re.compile(
    r"^\s*(?:-\s*)?(FileName|ControllerDLL|InputFile|includefile):\s*(.+?)\s*$",
    re.IGNORECASE,
)
# Required source keys in the Phase A report's two as-held model columns.
INPUT_KEYS = {
    "UnitsSystem",
    "WholeSystemStaticsEnabled",
    "DynamicsSolutionMethod",
    "ImplicitConstantTimeStep",
    "TargetLogSampleInterval",
    "SeabedOriginDepth",
    "WaveType",
    "WaveHs",
    "WaveTz",
    "WaveGamma",
    "WaveDirection",
    "WaveNumberOfComponents",
    "UserSpecifiedRandomWaveSeeds",
    "RefCurrentSpeed",
    "RefCurrentDirection",
    "WindType",
    "WindSpeed",
    "WindDirection",
    "StageDuration",
}


def _directory_link(path: Path) -> bool:
    """Recognize reparse directories on Python versions without is_junction."""
    return (
        path.is_symlink()
        or getattr(path, "is_junction", lambda: False)()
        or bool(
            getattr(path.lstat(), "st_file_attributes", 0)
            & stat.FILE_ATTRIBUTE_REPARSE_POINT
        )
    )


def _holdings(root: Path) -> dict[str, list[str]]:
    holdings: dict[str, list[str]] = {}
    for directory, subdirs, files in os.walk(root, followlinks=False):
        subdirs[:] = sorted(
            d
            for d in subdirs
            if d
            not in {
                ".git",
                ".venv",
                "__pycache__",
                ".pytest_cache",
                ".ruff_cache",
            }
            and not _directory_link(Path(directory) / d)
        )
        for filename in sorted(files):
            path = Path(directory) / filename
            # A symlink outside this checkout is not a repository holding.
            if path.is_file() and path.resolve().is_relative_to(root):
                holdings.setdefault(filename.casefold(), []).append(
                    path.relative_to(root).as_posix()
                )
    return holdings


def external_references(root: Path, model: Path, holdings=None) -> list[dict]:
    """Record each supported dependency occurrence and repository holdings."""
    root, model = root.resolve(), model.resolve()
    if not model.is_relative_to(root):
        raise ValueError("model is outside repository root")
    holdings = _holdings(root) if holdings is None else holdings
    rows = []
    for line_number, line in enumerate(
        model.read_text(encoding="utf-8").lstrip("\ufeff").splitlines(), 1
    ):
        match = REFERENCE.match(line)
        if not match:
            continue
        key, value = match.groups()
        value = PureWindowsPath(value.strip().strip("\"'")).as_posix()
        target = model.parent / value
        local = target.resolve().is_relative_to(root) and target.is_file()
        paths = holdings.get(PureWindowsPath(value).name.casefold(), [])
        if key.casefold() == "includefile":
            paths = [target.relative_to(root).as_posix()] if local else []
        status = "MISSING"
        if local:
            status = "HELD_RELATIVE_COMPATIBILITY_UNVERIFIED"
        elif paths:
            status = "HELD_ELSEWHERE_COMPATIBILITY_UNVERIFIED"
        rows.append(
            {
                "file": value,
                "key": key,
                "line": line_number,
                "held_paths": sorted(paths),
                "status": status,
            }
        )
    return rows


def cited_value(lines: list[str], line: int, end: int | None = None) -> str:
    """Read a scalar or StageDuration sequence at a one-based text locator."""
    if not 1 <= line <= len(lines):
        raise ValueError(f"source line {line} is absent")
    if end is not None and not line <= end <= len(lines):
        raise ValueError(f"source line {line}: citation range is invalid")
    key, separator, value = lines[line - 1].strip().partition(":")
    if not separator:
        raise ValueError(f"source line {line} has no key")
    if key == "StageDuration":
        durations = []
        for following in lines[line:]:
            match = re.fullmatch(r"\s*-\s+([\d.eE+-]+)\s*", following)
            if not match:
                break
            durations.append(match[1])
        if not durations:
            raise ValueError(f"source line {line}: stage durations are absent")
        if end is not None and end != line + len(durations):
            raise ValueError(
                f"source line {line}: citation range differs from sequence"
            )
        return "; ".join(durations)
    if end is not None and end != line:
        raise ValueError(f"source line {line}: citation range differs from scalar")
    return value.strip()


class _TableRows(HTMLParser):
    """Retain cell text, section membership and result-class rows structurally."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.sections = []
        self.has_basis = False
        self.rows = []
        self.result_rows = []
        self.unmarked_results = False
        self.is_result = False
        self.row = None
        self.cell = None

    def handle_starttag(self, tag, attrs):
        if tag == "section":
            section = dict(attrs).get("id")
            self.sections.append(section)
            self.has_basis |= section == "as-held"
        elif tag == "tr":
            self.row = []
            self.is_result = "result" in (dict(attrs).get("class") or "").split()
        elif tag in {"td", "th"} and self.row is not None:
            self.cell = (tag, [])

    def handle_data(self, data):
        if self.cell is not None:
            self.cell[1].append(data)

    def handle_endtag(self, tag):
        if tag in {"td", "th"} and self.cell is not None:
            kind, text = self.cell
            self.row.append((kind, " ".join("".join(text).split())))
            self.cell = None
        elif tag == "tr" and self.row is not None:
            self.rows.append(("as-held" in self.sections, self.row))
            in_results = "results" in self.sections and any(
                kind == "td" for kind, _ in self.row
            )
            self.unmarked_results |= in_results and not self.is_result
            if self.is_result or in_results:
                self.result_rows.append(self.row)
            self.row = None
        elif tag == "section" and self.sections:
            self.sections.pop()


def audit_input_echo(html: str, sources: dict[str, list[str]]) -> list[str]:
    """Require and validate a source citation in every model-value cell."""
    parser = _TableRows()
    parser.feed(html)
    parser.close()
    errors, checked, observed = [], 0, Counter()
    pattern = r"(.+?) \((L02|K03) lines? (\d+)(?:-(\d+))?: (\w+)\)"
    for in_basis, row in parser.rows:
        if parser.has_basis and not in_basis:
            continue
        if not row:
            continue
        if all(kind == "th" for kind, _ in row) and [text for _, text in row[1:]] == [
            "L02 as held",
            "K03 as held",
        ]:
            continue
        if len(row) < 2 or (parser.has_basis and len(row) != 3):
            errors.append("input echo row requires every model-value cell")
        for column, (kind, text) in enumerate(row[1:], 1):
            checked += 1
            match = re.fullmatch(pattern, text)
            if kind != "td" or not match:
                errors.append("input echo model-value cell requires a source citation")
                continue
            value, model, number, end, key = match.groups()
            observed[model, key] += 1
            if parser.has_basis and model != {1: "L02", 2: "K03"}.get(column):
                errors.append("input echo citation differs from model column")
            line = int(number)
            try:
                actual = cited_value(sources[model], line, int(end) if end else None)
                actual_key = sources[model][line - 1].strip().partition(":")[0]
                if actual_key != key:
                    errors.append(f"{model} line {line}: source key differs from {key}")
                if actual != value:
                    errors.append(f"{model} line {line}: transcribed value differs")
            except (KeyError, ValueError) as error:
                errors.append(str(error))
    if not checked:
        errors.append("input echo has no source-cited cells")
    expected = Counter({(model, key): 1 for model in MODELS for key in INPUT_KEYS})
    if parser.has_basis and observed != expected:
        errors.append("input echo requires each expected model input exactly once")
    return errors


def audit_result_rows(html: str) -> list[str]:
    """Require all result cells to remain explicit nonnumeric placeholders."""
    parser = _TableRows()
    parser.feed(html)
    parser.close()
    if not parser.result_rows:
        return ["report has no marked result rows"]
    errors = []
    if parser.unmarked_results:
        errors.append("report result row contains an unmarked or numeric result")
    for row in parser.result_rows:
        if len(row) < 2 or any(
            kind != "td"
            or not cell.startswith("[RESULT:")
            or not cell.endswith("]")
            or re.search(r"\d", cell)
            for kind, cell in row[1:]
        ):
            errors.append("report result row contains an unmarked or numeric result")
    return errors


def audit_inventory(inventory: str, model: str, references: list[dict]) -> list[str]:
    """Reconcile dependencies, raw occurrences and holdings in both directions."""
    errors, rows, expected = [], {}, {}
    for reference in references:
        expected.setdefault(reference["file"], []).append(reference)
    for line in inventory.splitlines():
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if cells[0] != model:
            continue
        if len(cells) != 6 or not re.fullmatch(r"`[^`]+`", cells[1]):
            errors.append(f"{model}: malformed dependency inventory row")
            continue
        filename = cells[1].strip("`")
        if filename in rows:
            errors.append(f"{model}: dependency requires one inventory row")
        rows[filename] = cells
    if rows.keys() != expected.keys():
        errors.append(f"{model}: inventory dependencies differ from raw model")
    for filename in rows.keys() & expected.keys():
        cells, uses = rows[filename], expected[filename]
        locators = re.findall(rf"\b{re.escape(model)} L(\d+)\b", cells[2])
        if Counter(map(int, locators)) != Counter(use["line"] for use in uses):
            errors.append(
                f"{model}: inventory occurrence locators differ for {filename}"
            )
        status = cells[4].casefold()
        if uses[0]["status"] == "MISSING":
            if "not held" not in status:
                errors.append(f"{model}: inventory falsely claims dependency held")
        elif "held" not in status or "not held" in status:
            errors.append(f"{model}: inventory omits dependency holding")
        stated_paths = {
            posixpath.normpath(posixpath.join(DOCUMENTS.as_posix(), unquote(target)))
            for target in re.findall(r"\[[^\]]+\]\(([^)]+)\)", cells[4])
        }
        held_paths = {path for use in uses for path in use["held_paths"]}
        if stated_paths != held_paths:
            errors.append(f"{model}: inventory holding paths differ for {filename}")
    return errors


def _case_families(worksheet: str) -> list[dict]:
    section = worksheet.split("## Proposed cases", 1)[1].split("## Results", 1)[0]
    rows = []
    for line in section.splitlines():
        if not line.startswith("| "):
            continue
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        if cells[0] == "Case":
            continue
        rows.append(
            {
                "case": cells[0],
                "status": "NOT_EVALUATED",
                "reference": cells[-1],
                "required_inputs": cells[2],
            }
        )
    return rows


def audit_modular_inventory(
    inventory: str, model: str, includes: list[dict]
) -> list[str]:
    """Bind modular inventory rows to their master locators and include targets."""
    master = DOMAIN / "library/model_library" / LIBRARIES[model] / "modular/master.yml"
    expected = Counter(
        (
            f"master L{row['line']}",
            f"{master.as_posix()}#L{row['line']}",
            row["file"],
            posixpath.normpath(posixpath.join(master.parent.as_posix(), row["file"])),
        )
        for row in includes
    )
    observed = Counter()
    for line in inventory.splitlines():
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if cells[0] != f"{model} modular":
            continue
        if len(cells) != 5:
            return [f"{model}: malformed modular inventory row"]
        links = [re.fullmatch(r"\[([^\]]+)\]\(([^)]+)\)", cell) for cell in cells[1:3]]
        if not all(links):
            return [f"{model}: modular inventory requires source and target links"]
        source, target = links
        observed[
            source[1],
            posixpath.normpath(
                posixpath.join(DOCUMENTS.as_posix(), unquote(source[2]))
            ),
            target[1],
            posixpath.normpath(
                posixpath.join(DOCUMENTS.as_posix(), unquote(target[2]))
            ),
        ] += 1
    if observed != expected:
        return [f"{model}: modular inventory differs from include references"]
    return []


def audit_phase_a(root: Path) -> dict:
    """Audit existing Phase A documents and return portable evidence as data."""
    root = root.resolve()
    holdings = _holdings(root)
    models, sources, errors = [], {}, []
    inventory = (root / DOCUMENTS / "external-files-inventory.md").read_text(
        encoding="utf-8"
    )
    for name, relative in MODELS.items():
        path = root / relative
        payload = path.read_bytes()
        sources[name] = payload.decode("utf-8-sig").splitlines()
        references = external_references(root, path, holdings)
        master = (
            root
            / DOMAIN
            / "library/model_library"
            / LIBRARIES[name]
            / "modular/master.yml"
        )
        includes = external_references(root, master, holdings)
        errors.extend(audit_inventory(inventory, name, references))
        errors.extend(audit_modular_inventory(inventory, name, includes))
        if any(row["status"] == "MISSING" for row in includes):
            errors.append(f"{name}: modular include is missing")
        models.append(
            {
                "model": name,
                "source": relative.as_posix(),
                "sha256": hashlib.sha256(payload).hexdigest(),
                "references": references,
                "modular_includes": includes,
            }
        )
    worksheet = (root / DOCUMENTS / "load-case-table.md").read_text(encoding="utf-8")
    html = (root / DOCUMENTS / "report-skeleton.html").read_text(encoding="utf-8")
    errors.extend(audit_input_echo(html, sources))
    errors.extend(audit_result_rows(html))
    cases = _case_families(worksheet)
    # Scope: issue 2423, Phase A; no count is an engineering acceptance limit.
    expected = {
        "Statics",
        *(
            f"Free decay: {axis}"
            for axis in ("surge", "sway", "heave", "roll", "pitch", "yaw")
        ),
        "Operating: source environment",
        "Operating: environmental variation",
        "Parked: source environment",
        "Parked: environmental variation",
    }
    if len(cases) != len(expected) or {case["case"] for case in cases} != expected:
        errors.append("worksheet case families differ from Phase A scope")
    for case in cases:
        if "[INPUT:" not in case["required_inputs"] or not case["reference"]:
            errors.append(
                f"{case['case']}: missing required input or source disposition"
            )
    return {
        "schema_version": "phase-a-v1",
        "solver_status": "NOT_EVALUATED",
        "execution_ready": False,
        "models": models,
        "case_families": cases,
        "errors": errors,
    }


def main(argv=None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True, help="repository root")
    args = parser.parse_args(argv)
    try:
        evidence = audit_phase_a(args.root)
    except (OSError, ValueError, IndexError) as error:
        # Do not echo absolute paths from OS exception strings into portable output.
        print(json.dumps({"errors": [f"source audit failed: {type(error).__name__}"]}))
        return 1
    print(json.dumps(evidence, indent=2, sort_keys=True))
    return int(bool(evidence["errors"]))


if __name__ == "__main__":
    raise SystemExit(main())
