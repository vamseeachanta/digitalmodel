"""Check tracked YAML include targets against a shrinking defect baseline."""

import argparse
import json
import os
from pathlib import Path
import posixpath
import subprocess
import sys

import yaml


REPO_ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO_ROOT / "src"))

from digitalmodel.solvers.orcaflex.modular_input_validation.utils import (
    extract_includefiles,
)


def scalar_value(tokens, index, anchors):
    """Return scalar values, including tagged, anchored and aliased strings."""
    while isinstance(tokens[index], (yaml.tokens.AnchorToken, yaml.tokens.TagToken)):
        index += 1
    token = tokens[index]
    if isinstance(token, yaml.tokens.ScalarToken):
        return token.value
    if isinstance(token, yaml.tokens.AliasToken):
        if token.value not in anchors:
            raise ValueError("undefined YAML alias")
        return anchors[token.value]
    return None


def extract_references(raw):
    """Adapt lexical scalar pairs to the shared extractor without parsing model data."""
    legacy = False
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError:
        text = raw.decode("latin-1")
        legacy = True
    tokens = list(yaml.scan(text, Loader=getattr(yaml, "CBaseLoader", yaml.BaseLoader)))
    anchors = {}
    pairs = []
    for index, token in enumerate(tokens):
        if isinstance(token, yaml.tokens.DocumentStartToken):
            anchors.clear()
        elif isinstance(token, yaml.tokens.AnchorToken):
            anchors[token.value] = None
            anchors[token.value] = scalar_value(tokens, index + 1, anchors)
        if not isinstance(token, yaml.tokens.KeyToken):
            continue
        key, separator = tokens[index + 1:index + 3]
        if not isinstance(key, yaml.tokens.ScalarToken) or not isinstance(
            separator, yaml.tokens.ValueToken
        ):
            continue
        name = "includefile" if key.value == "BaseFile" else key.value
        pairs.append({name: scalar_value(tokens, index + 3, anchors)})
    references = extract_includefiles(pairs)
    if legacy and any(not reference.isascii() for reference in references):
        raise ValueError("non-ASCII references require a verified source encoding")
    return references


def git(root, *args):
    """Git will resolve the supplied root without inherited repository bindings."""
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR", "GIT_INDEX_FILE"):
        env.pop(key, None)
    return subprocess.run(
        ["git", *args], cwd=root, env=env, capture_output=True, text=True,
    )


def tracked_files(root):
    result = git(root, "ls-files", "-z")
    if result.returncode:
        raise ValueError("git ls-files failed: " + result.stderr.strip())
    return set(result.stdout.split("\0")) - {""}


def resolve_target(source, reference):
    """Relative references will use repository paths, including Windows separators."""
    return posixpath.normpath(
        posixpath.join(posixpath.dirname(source), reference.replace("\\", "/"))
    )


def broken_references(root, tracked):
    """Return broken file/reference pairs; the excluded project tree stays outside scope."""
    broken = set()
    errors = []
    for source in sorted(tracked):
        if source.startswith("projects/") or Path(source).suffix.lower() not in (
            ".yml", ".yaml",
        ):
            continue
        try:
            for reference in extract_references((root / source).read_bytes()):
                if resolve_target(source, reference) not in tracked:
                    broken.add((source, reference))
        except (OSError, UnicodeError, yaml.YAMLError, ValueError, TypeError) as exc:
            errors.append(f"{source}: reference extraction failed: {exc}")
    return broken, errors


def read_baseline(path):
    """Baseline rows will identify the referring file and reference as written."""
    entries = set()
    for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not line or line.startswith("#"):
            continue
        entry = json.loads(line)
        if not isinstance(entry, list) or len(entry) != 2 or not all(
            isinstance(item, str) for item in entry
        ):
            raise ValueError(f"invalid baseline entry on line {number}")
        pair = tuple(entry)
        if pair in entries:
            raise ValueError(f"duplicate baseline entry on line {number}")
        entries.add(pair)
    return entries


def write_baseline(path, broken):
    """Initial baseline generation will refuse to overwrite an existing file."""
    with path.open("x", encoding="utf-8", newline="\n") as handle:
        handle.write("# Existing broken references: [referring file, reference].\n")
        for entry in sorted(broken):
            handle.write(json.dumps(entry, ensure_ascii=False) + "\n")


def report_broken(root, source, reference):
    target = resolve_target(source, reference)
    print(f"untracked include target: {source} -> {reference} (target: {target})")
    if git(root, "check-ignore", "-q", "--", target).returncode == 0:
        ignored = git(root, "check-ignore", "-v", "--", target)
        if ignored.returncode:
            raise ValueError("git check-ignore failed: " + ignored.stderr.strip())
        print("  ignore rule: " + ignored.stdout.strip())


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=REPO_ROOT)
    parser.add_argument("--baseline", type=Path)
    parser.add_argument("--write-baseline", action="store_true")
    args = parser.parse_args(argv)
    root = args.root.resolve()
    location = git(root, "rev-parse", "--show-toplevel")
    if location.returncode or Path(location.stdout.strip()).resolve() != root:
        parser.error("--root must name the repository root")
    baseline = args.baseline or root / "scripts/ci/include_target_baseline.txt"
    try:
        broken, errors = broken_references(root, tracked_files(root))
        for error in errors:
            print(error)
        if errors:
            return 1
        if args.write_baseline:
            write_baseline(baseline, broken)
            print(f"Wrote {len(broken)} baseline entries")
            return 0
        entries = read_baseline(baseline) if baseline.exists() else set()
        if args.baseline and not baseline.exists():
            raise ValueError("the specified baseline does not exist")
        for source, reference in sorted(broken - entries):
            report_broken(root, source, reference)
        for source, reference in sorted(entries - broken):
            print(f"stale baseline entry: {source} -> {reference}")
        print(f"Broken references: {len(broken)}; baseline entries: {len(entries)}")
        return int(broken != entries)
    except (OSError, ValueError) as exc:
        print(f"include target check failed: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
