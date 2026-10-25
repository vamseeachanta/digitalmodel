# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Inventory report evidence and literal test selection, without running reports.

Usage: uv run --no-project python scripts/reporting/report_evidence_audit.py
--root . --observations docs/audits/module-report-evidence.json --out-dir OUTPUT

Only a small, documented command/configuration subset is recognized. EXPLICIT
means literal whole-file selection, never collection, execution or qualification.
Observations are regeneration input; summaries and HTML are derived. No source
text or shell command is executed. Engineering citations are metadata only.
"""

from __future__ import annotations

import argparse
from collections import Counter
import configparser
from datetime import datetime
import hashlib
import html
import json
from pathlib import Path, PurePosixPath
import re
import shlex
import subprocess

OUTPUT_NAMES = ("module-report-evidence.json", "module-report-evidence.html")
RULES = {
    "L1": "UTF-8 text reads, LF writes and a test run with PYTHONUTF8 unset",
    "L2": "External equation, coefficient and factor citations with edition and clause",
    "L3": "Three reported numbers with literal, first-principles hand workings",
    "L4": "Numerical tolerances or displayed rounding; actionable rebuild failures",
    "L5": "Claims bounded by retained revision and collected-test evidence",
    "L6": "Draft for maintainer review and public wording",
    "L7": "Displayed inputs reproduce displayed results",
    "L8": "Literal check-command selection of the report tests",
}
UNESTABLISHED = {
    "L1": "Explicit text I/O settings and execution with PYTHONUTF8 unset are not established.",
    "L2": "External equation, coefficient and factor basis is not established from citation metadata.",
    "L3": "Three independently hand-worked reported-number anchors are not established.",
    "L4": "Numerical comparison tolerances and actionable rebuild failures are not established.",
    "L5": "Retained revision-pinned output naming collected tests is unavailable.",
    "L6": "Draft wording and identifier review have not been established.",
    "L7": "Displayed-input and displayed-result consistency has not been established.",
    "L8": "Static command selection is unresolved.",
}
SIMPLE_OPTIONS = {"-q", "-v", "-vv", "-x", "-s", "-ra", "-rA", "-rfE",
                  "--no-header", "--disable-warnings", "--strict-markers"}
VALUE_OPTIONS = {"-p", "--tb", "--basetemp", "--maxfail", "--color", "--import-mode"}
UNCERTAIN_COLLECTION = re.compile(
    r"collect_ignore|pytest_(?:ignore_collect|collection|collect_file)|pytest_plugins"
)


def safe_path(root, value):
    """Validate a canonical relative path and reject escaping symlinks."""
    if not isinstance(value, str) or not value or "\\" in value:
        raise ValueError("invalid source path")
    pure = PurePosixPath(value)
    if pure.is_absolute() or ".." in pure.parts or pure.as_posix() != value:
        raise ValueError("invalid source path")
    path = root / value
    if not path.resolve().is_relative_to(root.resolve()):
        raise ValueError("source path escapes root")
    return path


def read_text(root, path):
    """Read source text explicitly as UTF-8; missing components stay missing."""
    target = safe_path(root, path)
    if not target.is_file():
        return ""
    try:
        return target.read_text(encoding="utf-8")
    except UnicodeDecodeError:
        return ""


def source_digest(root, path):
    """Hash actual source bytes, preserving encoding and line-ending changes."""
    target = safe_path(root, path)
    return hashlib.sha256(target.read_bytes()).hexdigest() if target.is_file() else None


def is_report(path):
    """Select tracked report candidates, including historical supporting pages.

    The inventory is deliberately broader than qualified module analyses. It
    excludes change pages and navigation indexes except module report indexes.
    No content is read to discover or qualify a report.
    """
    p = PurePosixPath(path)
    if p.suffix != ".html" or not path.startswith(("docs/domains/", "docs/reports/", "docs/api/")):
        return False
    if p.name.lower() in {"changes.html", "plan.html", "provenance.html", "fixup-plan.html"}:
        return False
    if p.name == "index.html":
        return path.startswith("docs/reports/")
    return (path.startswith("docs/reports/") or
            bool(re.search(r"report|analysis|study|assessment|verification|diffraction", p.name, re.I)))


def is_config(path):
    """Identify check declarations and collection-control files."""
    return (path.startswith(".github/workflows/") and path.endswith((".yml", ".yaml"))
            or path == ".claude/quality-gates.yaml"
            or path in {"pytest.ini", "pyproject.toml", ".pre-commit-config.yaml"}
            or path.endswith("conftest.py"))


def freeze_paths(paths):
    """Exclude this audit and its outputs from the inspected source set."""
    return sorted({p for p in paths if not (
        p.startswith("docs/audits/") or "report_evidence_audit" in p
    )})


def parse_command(command):
    """Parse a restricted literal pytest invocation, returning no shell payload."""
    if any(c in command for c in ("$", "`", ";", "&", "|", "<", ">", "\\", "\n")):
        return [], "UNKNOWN", "dynamic or compound shell syntax"
    try:
        tokens = shlex.split(command)
    except ValueError:
        return [], "UNKNOWN", "malformed command"
    if not tokens:
        return [], "UNKNOWN", "empty command"
    # Recognize direct pytest, python -m pytest, or an uv run prefix. Other
    # executables (including echo) cannot turn embedded words into commands.
    if tokens[:2] == ["uv", "run"]:
        index = 2
        uv_flags = {"--no-project", "--no-sources", "--no-sync", "--frozen", "--locked", "--offline"}
        uv_values = {"--with", "--with-editable", "--with-requirements", "--python", "--extra"}
        while index < len(tokens):
            token = tokens[index]
            if token in uv_flags:
                index += 1
            elif token in uv_values:
                index += 2
            elif any(token.startswith(flag + "=") for flag in uv_values):
                index += 1
            else:
                break
        tokens = tokens[index:]
        if not tokens:
            return [], "UNKNOWN", "unrecognized uv invocation"
    if tokens[:3] in (["python", "-m", "pytest"], ["python3", "-m", "pytest"]):
        args = tokens[3:]
    elif tokens[0] == "pytest":
        args = tokens[1:]
    else:
        return [], "UNKNOWN", "unrecognized command prefix"
    selected = []
    uncertain = False
    index = 0
    while index < len(args):
        arg = args[index]
        if arg in VALUE_OPTIONS:
            index += 1
            if index == len(args):
                uncertain = True
        elif arg in SIMPLE_OPTIONS or any(arg.startswith(x + "=") for x in VALUE_OPTIONS):
            pass
        elif arg.startswith("-"):
            uncertain = True
        elif (arg == "tests" or arg.startswith("tests/")) and not any(c in arg for c in "*?[]:"):
            clean = arg.rstrip("/")
            if ".." in PurePosixPath(clean).parts:
                uncertain = True
            else:
                selected.append(clean)
        else:
            uncertain = True
        index += 1
    return sorted(set(selected)), ("UNKNOWN" if uncertain or not selected else "LITERAL"), (
        "bare, filtered or unfamiliar pytest arguments" if uncertain or not selected else "literal paths only"
    )


def inspect_commands(root, configs):
    """Extract a conservative YAML command subset without a YAML evaluator.

    Single-line command/run scalars and single-command literal blocks are
    supported. Conditions, disabled declarations, alternate working directories,
    anchors, tags, multiline shell and unfamiliar YAML remain UNKNOWN. A condition
    anywhere in a declaration file conservatively affects that entire file.
    """
    result = []
    for path in configs:
        if not path.endswith((".yaml", ".yml")) or path == ".pre-commit-config.yaml":
            continue
        lines = read_text(root, path).splitlines()
        active = [line for line in lines if line.strip() and not line.lstrip().startswith("#")]
        uncertain_file = any(re.search(
            r"(?:^|\s)(?:if|when|working-directory):|enabled:\s*false|:\s*[!&*]|\t", line
        ) for line in active)
        for index, line in enumerate(lines):
            match = re.match(r"^(\s*)(?:-\s*)?(command|run):\s*(.*)$", line)
            if not match:
                continue
            value = match[3].strip()
            if value in {"|", "|-", ">", ">-"}:
                block = []
                for following in lines[index + 1:]:
                    if following.strip() and len(following) - len(following.lstrip()) <= len(match[1]):
                        break
                    if following.strip() and not following.lstrip().startswith("#"):
                        block.append(following.strip())
                value = "\n".join(block)
            elif value.startswith('"'):
                try:
                    value = json.loads(value)
                except (ValueError, TypeError):
                    value = ""
            elif value.startswith("'"):
                value = value[1:-1].replace("''", "'") if value.endswith("'") else ""
            else:
                value = value.split(" #", 1)[0]
            paths, status, reason = parse_command(value)
            parse_status = status
            if uncertain_file:
                status, reason = "UNKNOWN", "conditional, disabled or unfamiliar declaration context"
            result.append({"locator": {"path": path, "line": index + 1},
                           "explicit_command_paths": paths, "status": status, "parse_status": parse_status, "reason": reason})
    return result


def collection_uncertainty(root, test, configs):
    """Name collection controls that prevent unconditional static selection."""
    reasons = []
    for config in configs:
        text = read_text(root, config)
        if config.endswith("conftest.py"):
            parent = PurePosixPath(config).parent
            if parent != PurePosixPath(".") and not PurePosixPath(test).is_relative_to(parent):
                continue
            if UNCERTAIN_COLLECTION.search(text):
                # Root declarative exclusions outside tests do not affect these
                # paths. Any other collection hook or dynamic list is unresolved.
                stripped = text
                static = re.fullmatch(r'\s*(?:""".*?"""\s*)?collect_ignore\s*=\s*\[([^\]]*)\]\s*', stripped, re.S)
                if static:
                    try:
                        import ast
                        values = ast.literal_eval("[" + static[1] + "]")
                        if all(isinstance(v, str) and not PurePosixPath(v).is_absolute()
                               and ".." not in PurePosixPath(v).parts
                               and not PurePosixPath(test).is_relative_to(parent / v)
                               and not any(c in v for c in "*?[]") for v in values):
                            continue
                    except (ValueError, SyntaxError, TypeError):
                        pass
                reasons.append(config)
        elif config == "pytest.ini":
            parser = configparser.ConfigParser(interpolation=None)
            try:
                parser.read_string(text)
                options = parser["pytest"]
                if options.get("python_files", "test_*.py *_test.py").split() != ["test_*.py", "*_test.py"]:
                    reasons.append(config)
                addopts = options.get("addopts", "")
                # Only known non-selecting pytest arguments are recognized.
                _, option_status, _ = parse_command("pytest tests " + addopts.replace("\n", " "))
                if option_status != "LITERAL":
                    reasons.append(config)
                import fnmatch
                for pattern in options.get("norecursedirs", "").split():
                    parents = [p.as_posix() for p in PurePosixPath(test).parents]
                    if any(fnmatch.fnmatch(p, pattern) or fnmatch.fnmatch(p, "*/" + pattern)
                           or fnmatch.fnmatch(PurePosixPath(p).name, pattern) for p in parents):
                        reasons.append(config)
            except (configparser.Error, KeyError):
                reasons.append(config)
        elif config == "pyproject.toml" and "pytest.ini" not in configs:
            if "[tool.pytest.ini_options]" in text:
                reasons.append(config)
    return sorted(set(reasons))


def covered(test, path):
    """Match a whole test path or its parent, respecting segment boundaries."""
    return test == path or test.startswith(path.rstrip("/") + "/")


def selection(root, tests, commands, configs):
    """Require literal coverage for every associated test, independent of execution."""
    if not tests:
        return "UNKNOWN", []
    statuses, controls = [], []
    for test in tests:
        uncertainty = collection_uncertainty(root, test, configs)
        controls.extend(uncertainty)
        if uncertainty:
            statuses.append("UNKNOWN")
        elif any(c["status"] == "LITERAL" and any(covered(test, p) for p in c["explicit_command_paths"])
                 for c in commands):
            statuses.append("EXPLICIT")
        elif any(c["status"] == "UNKNOWN" for c in commands) or not commands:
            statuses.append("UNKNOWN")
        else:
            statuses.append("MISSING")
    status = "MISSING" if "MISSING" in statuses else "UNKNOWN" if "UNKNOWN" in statuses else "EXPLICIT"
    return status, sorted(set(controls))


def default_observation(path, generators, tests, texts):
    """Associate only literal references; unresolved relationships remain missing."""
    matches = [p for p in generators if path in texts[p]]
    generator = matches[0] if len(matches) == 1 else None
    test_paths = [p for p in tests if path in texts[p] or generator and (
        generator in texts[p] or PurePosixPath(generator).name in texts[p]
    )]
    return {"report_id": path, "report_path": path, "generator_path": generator,
            "test_paths": sorted(test_paths), "support_paths": [], "source_digests": {},
            "observation_time": None, "observation_basis": "Literal repository references only; no manual qualification.",
            "rule_findings": []}


def validate_observation(root, observation, sources):
    """Validate locators and forbid qualifying statuses in manual input."""
    paths = [observation["report_path"], *observation["test_paths"], *observation.get("support_paths", [])]
    if observation["generator_path"]:
        paths.append(observation["generator_path"])
    for path in paths:
        safe_path(root, path)
        if path not in sources:
            raise ValueError("observation source outside frozen source set")
    time = observation.get("observation_time")
    if time is not None:
        datetime.strptime(time, "%Y-%m-%dT%H:%M:%SZ")
    for finding in observation["rule_findings"]:
        if finding["rule"] not in RULES or finding["status"] not in {
            "UNESTABLISHED", "NOT_EVALUATED", "OBSERVED_GAP", "OBSERVED_METADATA"
        }:
            raise ValueError("invalid finding rule or status")
        if not finding.get("detail") or not finding.get("locators"):
            raise ValueError("finding needs detail and locator")
        for locator in finding["locators"]:
            if (locator["path"] not in paths or type(locator["line"]) is not int
                    or locator["line"] < 1
                    or locator["line"] > max(1, len(read_text(root, locator["path"]).splitlines()))):
                raise ValueError("invalid finding locator")
    return sorted(set(paths))


def build_audit(root, paths, revision, observations=None):
    """Build stable generated summaries from a frozen source list and manual input."""
    root = Path(root).resolve()
    available = freeze_paths(paths)
    for path in available:
        if PurePosixPath(path).is_absolute() or ".." in PurePosixPath(path).parts:
            raise ValueError("invalid source path")
    if observations is not None:
        if observations.get("schema_version") != 1:
            raise ValueError("unsupported observation schema")
        sources = freeze_paths(observations["source_files"])
        if any(p not in available for p in sources):
            raise ValueError("frozen source not tracked at inspected revision")
        required = {p for p in available if is_report(p) or is_config(p)}
        if not required.issubset(sources):
            raise ValueError("incomplete frozen source set")
        revision = observations["inspection_revision"]
    else:
        sources = [p for p in available if is_report(p) or is_config(p)
                   or p.endswith(".py") and (p.startswith(("scripts/", "tests/", "src/", "docs/")))
                   and ("report" in p.lower() or "build_" in PurePosixPath(p).name)]
    for path in sources:
        safe_path(root, path)
    reports = sorted(p for p in sources if is_report(p))
    configs = sorted(p for p in sources if is_config(p))
    texts = {p: read_text(root, p) for p in sources if p.endswith(".py") and not is_config(p)}
    generators = [p for p in texts if p.startswith(("scripts/", "src/", "docs/"))]
    tests = [p for p in texts if p.startswith("tests/") and PurePosixPath(p).name.startswith("test_")]
    declared = {p: default_observation(p, generators, tests, texts) for p in reports}
    manual_ids = set()
    if observations:
        for observation in observations["observations"]:
            identity = observation["report_id"]
            if identity in manual_ids:
                raise ValueError("duplicate report id")
            manual_ids.add(identity)
            validate_observation(root, observation, sources)
            if observation["report_path"] not in reports or identity != observation["report_path"]:
                raise ValueError("report id must be its discovered source path")
            declared[identity] = observation
    commands = inspect_commands(root, configs)
    records, preserved = [], []
    config_digests = {p: source_digest(root, p) for p in configs}
    configs_current = observations is None or observations.get("check_config_digests") == config_digests
    for identity, observation in sorted(declared.items()):
        dependencies = validate_observation(root, observation, sources)
        digests = {p: source_digest(root, p) for p in dependencies}
        fresh = observation["source_digests"] == digests and configs_current
        initial = not observations
        status, controls = selection(root, observation["test_paths"], commands, configs)
        literal_coverage = bool(observation["test_paths"]) and all(any(
            c["parse_status"] == "LITERAL" and any(covered(test, p) for p in c["explicit_command_paths"])
            for c in commands) for test in observation["test_paths"])
        missing = []
        if digests[observation["report_path"]] is None:
            missing.append("report file")
        if not observation["generator_path"]:
            missing.append("generator mapping")
        elif digests[observation["generator_path"]] is None:
            missing.append("generator file")
        if not observation["test_paths"]:
            missing.append("report test mapping")
        elif any(digests[p] is None for p in observation["test_paths"]):
            missing.append("test file")
            status = "UNKNOWN"
        if not fresh and not initial:
            missing.append("stale manual observations")
        findings = []
        for rule in RULES:
            saved = [f for f in observation["rule_findings"] if f["rule"] == rule] if fresh else []
            findings.extend(saved or [{"rule": rule, "status": "UNESTABLISHED",
                                      "detail": UNESTABLISHED[rule],
                                      "locators": [{"path": observation["report_path"], "line": 1}]}])
        findings = [f for f in findings if f["rule"] != "L8"] + [{
            "rule": "L8", "status": "OBSERVED_METADATA",
            "detail": "Static selection: " + status + "; collection and execution are not established.",
            "locators": [c["locator"] for c in commands if any(
                covered(t, p) for t in observation["test_paths"] for p in c["explicit_command_paths"]
            )] or [{"path": observation["report_path"], "line": 1}],
        }]
        missing.extend(["revision-pinned collected-test output", "independent engineering-basis review"])
        related = [c for c in commands if any(
            covered(t, p) for t in observation["test_paths"] for p in c["explicit_command_paths"]
        )]
        records.append({**{k: observation[k] for k in (
            "report_id", "report_path", "generator_path", "test_paths", "observation_time", "observation_basis"
        )}, "source_digests": digests, "check_config_refs": [c["locator"] for c in related],
            "explicit_command_paths": sorted({p for c in related for p in c["explicit_command_paths"]}),
            "selection_status": status, "literal_whole_file_coverage": literal_coverage,
            "collection_control_refs": controls,
            "execution_evidence": {"status": "NOT_RUN", "reason": "No retained output establishes this revision and collected tests."},
            "observation_status": "CURRENT" if fresh else "DISCOVERED" if initial else "STALE",
            "rule_findings": findings, "missing_inputs": sorted(set(missing))})
        preserved.append({**observation, "source_digests": digests} if initial else observation)
    if not any(r["test_paths"] for r in records):
        raise ValueError("no repository-local report/test pair discoverable")
    summary = {"report_candidates": len(records),
               "selection": {s: sum(r["selection_status"] == s for r in records)
                             for s in ("EXPLICIT", "MISSING", "UNKNOWN")},
               "execution": {"NOT_RUN": len(records)},
               "literal_whole_file_coverage": sum(r["literal_whole_file_coverage"] for r in records),
               "observations": dict(sorted(Counter(r["observation_status"] for r in records).items()))}
    return {"schema_version": 1, "inspection_revision": revision,
            "status": "Draft for maintainer review", "source_files": sources,
            "source_digests": [{"path": p, "sha256": source_digest(root, p)} for p in sources],
            "discovery_basis": "Tracked HTML under docs/reports and report, analysis, study, assessment, verification or diffraction HTML under docs/domains and docs/api; change pages and navigation excluded. Supporting and historical pages are candidates, not qualified module analyses.",
            "rule_basis": RULES, "observations": preserved, "reports": records,
            "commands": commands,
            "check_config_digests": observations.get("check_config_digests", {}) if observations else config_digests,
            "current_check_config_digests": config_digests, "summary": summary,
            "revision_history": observations.get("revision_history", []) if observations else [],
            "next_actions": observations.get("next_actions", []) if observations else [],
            "limits": ["No report generator, engineering module, solver or report-specific test was executed.",
                       "Conditions and unfamiliar syntax are not evaluated; UNKNOWN is not evidence of omission.",
                       "Citation metadata does not establish source rights or physical correctness. The filename inventory excludes other document formats and test golden fixtures.",
                       "Literals, independence labels and static EXPLICIT selection do not qualify engineering results.",
                       "Dashboard integration, report fixes, capability inventory and knowledge-pack acceptance are SKIPPED."]}


def render_html(document):
    """Render escaped source locators and all findings in a standalone page."""
    esc = lambda value: html.escape(str(value), quote=True)
    parts = ['<!doctype html>\n<html lang="en"><meta charset="utf-8">',
             '<meta name="viewport" content="width=device-width, initial-scale=1">',
             '<title>Module report evidence audit</title>',
             '<style>body{font:16px/1.5 system-ui;max-width:1100px;margin:2rem auto;padding:0 1rem;color:#182b3a}table{border-collapse:collapse;width:100%}td,th{border:1px solid #bccad4;padding:.5rem;text-align:left;vertical-align:top}code{overflow-wrap:anywhere}summary{cursor:pointer}details{border-bottom:1px solid #bccad4;padding:.7rem 0}h1,h2{color:#153c53}</style>',
             '<h1>Module report evidence audit</h1><p>Draft for maintainer review</p>',
             '<h2>Summary</h2>', '<p>Inspected revision: <code>' + esc(document["inspection_revision"]) + '</code></p>',
             '<p>' + esc(document["discovery_basis"]) + '</p>',
             '<pre>' + esc(json.dumps(document["summary"], indent=2)) + '</pre>',
             '<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>What changed and why</th></tr>']
    for row in document["revision_history"]:
        parts.append('<tr>' + ''.join('<td>' + esc(row[k]) + '</td>' for k in ("revision", "date", "change")) + '</tr>')
    parts.append('</table><h2>Rule basis</h2><ul>')
    for key, value in RULES.items():
        parts.append('<li>' + esc(key + ': ' + value) + '</li>')
    parts.append('</ul><h2>Prioritized next actions</h2><table><tr><th>Priority</th><th>Owning file</th><th>Missing evidence and action</th></tr>')
    for action in document["next_actions"]:
        parts.append('<tr><td>' + esc(action["priority"]) + '</td><td><code>' + esc(action["owning_file"]) + '</code></td><td>' + esc(action["detail"]) + '</td></tr>')
    parts.append('</table><h2>Findings</h2><p>EXPLICIT describes literal command selection only. All execution evidence remains NOT_RUN.</p>')
    for report in document["reports"]:
        parts.append('<details><summary><code>' + esc(report["report_id"]) + '</code> — ' + esc(report["selection_status"]) + ' / NOT_RUN</summary>')
        parts.append('<p>' + esc(report["observation_basis"]) + '</p>')
        parts.append('<pre>' + esc(json.dumps({k: report[k] for k in (
            "generator_path", "test_paths", "observation_status", "observation_time", "check_config_refs",
            "literal_whole_file_coverage",
            "explicit_command_paths", "collection_control_refs", "missing_inputs"
        )}, indent=2)) + '</pre>')
        for heading, rules in (
            ("Report content", {"L1", "L4", "L6", "L7"}),
            ("Engineering basis", {"L2", "L3"}),
            ("Claims and execution evidence", {"L5"}),
            ("Test selection", {"L8"}),
        ):
            parts.append('<h3>' + heading + '</h3><table><tr><th>Rule</th><th>Status</th><th>Evidence and limits</th></tr>')
            for finding in report["rule_findings"]:
                if finding["rule"] not in rules:
                    continue
                locations = '; '.join(p["path"] + ':' + str(p["line"]) for p in finding["locators"])
                parts.append('<tr><td>' + esc(finding["rule"]) + '</td><td>' + esc(finding["status"]) + '</td><td>' + esc(finding["detail"]) + '<br><code>' + esc(locations) + '</code></td></tr>')
            parts.append('</table>')
        parts.append('</details>')
    parts.append('<h2>Known limits</h2><ul>')
    parts.extend('<li>' + esc(limit) + '</li>' for limit in document["limits"])
    parts.append('</ul><p>The JSON companion retains the frozen file set, source digests, command locators and manual observations separately from generated summaries.</p></html>\n')
    return '\n'.join(parts)


def write_outputs(document, out_dir):
    """Write only the two named artifacts, in UTF-8 with LF line endings."""
    out_dir = Path(out_dir)
    out_dir.mkdir(parents=True, exist_ok=True)
    for name in OUTPUT_NAMES:
        if (out_dir / name).is_symlink():
            raise ValueError("output path must not be a symlink")
    (out_dir / OUTPUT_NAMES[0]).write_text(
        json.dumps(document, indent=1, ensure_ascii=False, allow_nan=False) + "\n", encoding="utf-8", newline="\n"
    )
    (out_dir / OUTPUT_NAMES[1]).write_text(render_html(document), encoding="utf-8", newline="\n")


def main():
    """Read observations before writing, and inspect tracked sources offline."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path("."))
    parser.add_argument("--observations", type=Path)
    parser.add_argument("--out-dir", type=Path, required=True)
    args = parser.parse_args()
    root = args.root.resolve()
    revision = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root, text=True, encoding="utf-8").strip()
    # On a later commit, the original inspected revision remains the inventory
    # boundary. Current bytes, rather than Git object bytes, detect stale input.
    observations = json.loads(args.observations.read_text(encoding="utf-8")) if args.observations else None
    frozen_revision = observations["inspection_revision"] if observations else revision
    if not re.fullmatch(r"[0-9a-f]{40}", frozen_revision):
        raise ValueError("invalid inspected revision")
    paths = subprocess.check_output(
        ["git", "ls-tree", "-r", "--name-only", "-z", frozen_revision], cwd=root
    ).decode("utf-8").strip("\0").split("\0")
    document = build_audit(root, paths, revision, observations)
    write_outputs(document, args.out_dir)
    print(json.dumps(document["summary"], sort_keys=True))


if __name__ == "__main__":
    main()
