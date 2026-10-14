# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Keep committed OrcaFlex defaults compatible without changing model values."""

import os
import re
import subprocess
from pathlib import Path

import pytest
import yaml

ROOT = Path(__file__).resolve().parents[2]
MODEL_SECTION = re.compile(
    r"^(?:LineTypes|Lines|Vessels|VesselTypes|6DBuoys|"
    r"3DBuoys|Winches|Shapes|Supports|MorisonElementTypes|Groups|VariableData|"
    r"BaseFile|includefile):|^  (?:UnitsSystem|WaterDepth|WaveTrains):", re.MULTILINE,
)


def git_output(*arguments):
    """Read this checkout without inherited Git repository bindings."""
    environment = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        environment.pop(key, None)
    return subprocess.check_output(
        ["git", *arguments], cwd=ROOT, env=environment,
    )


def model_files():
    """Enumerate tracked model text, including modular section fragments.

    Native section keys identify executable input rather than lowercase specs,
    basis records, parameters, manifests or source catalogs. OrcaWave exports
    are excluded by their program header.
    """
    names = git_output("ls-files", "-z", "*.yml", "*.yaml", "*.dat").decode().split("\0")
    for name in filter(None, names):
        path = ROOT / name
        if path.is_symlink():
            continue
        try:
            text = path.read_text(encoding="utf-8-sig")
        except UnicodeError:
            continue
        if re.search(r"^(?:basename|metadata|basis|cases):", text, re.MULTILINE):
            continue
        native_header = re.search(r"^# Type: Model\s*$", text, re.MULTILINE)
        if "OrcaWave" not in text[:500] and (native_header or MODEL_SECTION.search(text)):
            yield path, text


def null_values(text):
    """Locate explicit plain null values, excluding comments and quoted text."""
    node = yaml.compose(text)
    pending = [node]
    while pending:
        item = pending.pop()
        if isinstance(item, yaml.MappingNode):
            pending.extend(value for _, value in item.value)
        elif isinstance(item, yaml.SequenceNode):
            pending.extend(item.value)
        elif (isinstance(item, yaml.ScalarNode)
              and item.tag == "tag:yaml.org,2002:null"
              and item.value.lower() == "null"):
            yield item


def test_committed_models_have_no_null_value_tokens():
    """A default shall use a tilde in every tracked executable model fragment."""
    scanned = []
    findings = []
    for path, text in model_files():
        scanned.append(path)
        if re.search(r"(?i)(?<!\w)null(?!\w)", text):
            findings.extend(
                f"{path.relative_to(ROOT)}:{node.start_mark.line + 1}"
                for node in null_values(text)
            )
    assert scanned, "No executable model text was discovered"
    assert not findings, "\n".join(findings)


@pytest.mark.parametrize("text,count", [
    ("General:\n  Default: null\n", 1),
    ("Lines:\n  - Defaults: [null, ~, NULL]\n", 2),
    ("General:\n  Text: 'null'\n  Other: nullify\n  # null\n", 0),
])
def test_null_detection_recognizes_only_values(text, count):
    """Quoted strings and comments shall retain their existing meaning."""
    assert len(list(null_values(text))) == count


def test_model_changes_preserve_parsed_base_values():
    """Every changed model shall equal its immutable pre-change YAML values."""
    base = os.environ.get(
        "ORCAFLEX_MODEL_BASE", "e92bcfe4d4e8df898ca79cc968ba682f1248636e",
    )
    changed = git_output("diff", "--name-only", "-z", base, "--").decode().split("\0")
    checked = []
    for path, text in model_files():
        relative = path.relative_to(ROOT).as_posix()
        if relative in changed:
            original = git_output("show", f"{base}:{relative}")
            assert yaml.safe_load(text) == yaml.safe_load(original), relative
            checked.append(relative)
    assert checked, "No regenerated models were compared with the base"
