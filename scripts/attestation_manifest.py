# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Offline specification attestation; never executes or imports a native solver.

Scope: issue 2428 named roots. Claim policy: SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY
sections 1.1 and 4; recorded load/statics evidence alone cannot promote L2/L3.
"""

from __future__ import annotations

import argparse
from collections.abc import Callable, Sequence
import copy
from datetime import datetime
import hashlib
import html
import json
from pathlib import Path, PurePosixPath

import yaml

# Issue 2428, Models (paths); campaign inputs are deliberately outside this set.
SPEC_ROOTS = (
    "library/model_library",
    "library/templates",
    "library/tier2_fast",
    "installation",
    "jumper",
    "subsea/jumper/installation",
    "pipeline/installation",
    "reference",
    "regional",
    "mooring/reference",
    "passing_ship",
    "training",
)
STATES = {"PASS", "FAIL", "NOT_ATTEMPTED", "NOT_EVALUATED"}


def relative_path(value: str) -> Path:
    """Reject ambiguous paths, including Windows spellings on POSIX hosts."""
    path = PurePosixPath(value)
    if (
        not value
        or path.is_absolute()
        or ".." in path.parts
        or "\\" in value
        or ":" in value
        or str(path) != value
        or value == "."
    ):
        raise ValueError("expected canonical relative path")
    return Path(*path.parts)


def contained(root: Path, value: str) -> Path:
    """Resolve an unlinked path within the supplied evidence root."""
    path = root / relative_path(value)
    if not path.resolve().is_relative_to(root.resolve()):
        raise ValueError("path escapes root")
    if any(p.is_symlink() for p in (path, *path.parents) if p != root.parent):
        raise ValueError("linked paths are not evidence")
    return path


def digest(path: Path) -> str:
    """Return the SHA-256 digest of the supplied file bytes."""
    return hashlib.sha256(path.read_bytes()).hexdigest()


def dumps(value: dict) -> str:
    """Serialize a manifest deterministically with a final newline."""
    return json.dumps(value, indent=2, sort_keys=True, ensure_ascii=True) + "\n"


def build_manifest(
    root: Path, solver_version: str, *, paths: Sequence[str] | None = None
) -> dict:
    """Inventory source bytes and intended outputs, with no execution claims."""
    root = Path(root).resolve(strict=True)
    if not solver_version.strip():
        raise ValueError("a pinned solver version is required")
    if paths is None:
        paths = []
        for folder in SPEC_ROOTS:
            directory = contained(root, folder)
            if not directory.is_dir():
                raise ValueError(f"missing specification root: {folder}")
            paths.extend(
                p.relative_to(root).as_posix() for p in directory.rglob("spec.yml")
            )
    paths = sorted(set(paths))
    if not paths:
        raise ValueError("empty specification inventory")
    cases = []
    for name in paths:
        source = contained(root, name)
        if source.name != "spec.yml" or not source.is_file():
            raise ValueError("expected an existing spec.yml")
        cases.append(
            {
                "case_id": name,
                "source_sha256": digest(source),
                "master": relative_path(name).with_name("master.yml").as_posix(),
                "generation": {"status": "NOT_ATTEMPTED"},
                "files": {},
                "load": {"status": "NOT_EVALUATED"},
                "statics": {"status": "NOT_EVALUATED"},
            }
        )
    return {
        "schema_version": "1",
        "solver_version": solver_version,
        "claim_update": "NOT_EVALUATED",
        "root_counts": {
            folder: sum(name.startswith(f"{folder}/") for name in paths)
            for folder in SPEC_ROOTS
        },
        "cases": cases,
    }


def generate_one(source: Path, output: Path) -> None:
    """Use only the existing offline YAML generator, not the solver adapter."""
    from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

    ModularModelGenerator(source).generate(output)


def _alternate_schema(source: Path) -> bool:
    # Same discriminator as scripts/audit_all_specs.py::_is_different_schema.
    data = yaml.safe_load(source.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        return False
    env = data.get("environment", {})
    return (
        any(k in data for k in ("moored_vessel", "passing_ship", "pipe", "jumper"))
        or isinstance(env, dict)
        and "metocean" in env
    )


def _bundle_digests(output: Path, master: str) -> dict:
    directory = contained(output, master).parent
    files = {}
    for path in sorted(directory.rglob("*")):
        relative = path.relative_to(output).as_posix()
        contained(output, relative)
        if path.is_file():
            files[relative] = digest(path)
    return files


def _generated_files(output: Path, master: str) -> dict:
    """Hash the full generated bundle and check recursive YAML include closure."""
    master_path = contained(output, master)
    pending, visited = [master_path], set()
    while pending:
        current = pending.pop()
        if current in visited:
            continue
        visited.add(current)
        data = yaml.safe_load(current.read_text(encoding="utf-8"))
        nodes, containers = [data], set()
        while nodes:
            node = nodes.pop()
            if isinstance(node, (list, dict)):
                if id(node) in containers:
                    continue
                containers.add(id(node))
            if isinstance(node, list):
                nodes.extend(node)
            elif isinstance(node, dict):
                nodes.extend(node.values())
                for key in ("includefile", "BaseFile"):
                    if key in node:
                        target = current.parent / relative_path(node[key])
                        pending.append(
                            contained(output, target.relative_to(output).as_posix())
                        )
    return _bundle_digests(output, master)


def _validate_manifest(manifest: dict) -> None:
    if (
        manifest.get("schema_version") != "1"
        or not manifest.get("solver_version", "").strip()
    ):
        raise ValueError("invalid manifest header")
    seen = set()
    for case in manifest["cases"]:
        name = case["case_id"]
        if name in seen or relative_path(name).name != "spec.yml":
            raise ValueError("duplicate or invalid case identity")
        seen.add(name)
        if case["master"] != relative_path(name).with_name("master.yml").as_posix():
            raise ValueError("master does not match case identity")
        sha = case["source_sha256"]
        if len(sha) != hashlib.sha256().digest_size * 2 or any(
            c not in "0123456789abcdef" for c in sha
        ):
            raise ValueError("invalid source digest")
    if not seen:
        raise ValueError("empty specification inventory")


def generate_cases(
    manifest: dict,
    root: Path,
    output: Path,
    generator: Callable[[Path, Path], None] | None = None,
) -> dict:
    """Generate independent cases; refuse stale outputs and changed source bytes."""
    _validate_manifest(manifest)
    root, output = Path(root).resolve(), Path(output).absolute()
    if output.exists() or output.is_symlink():
        raise ValueError("output already exists; choose a fresh directory")
    for case in manifest["cases"]:
        if digest(contained(root, case["case_id"])) != case["source_sha256"]:
            raise ValueError("source digest mismatch")
    # Validate parents before any write, so symlinked output roots cannot escape.
    if output.parent.resolve() != output.parent:
        raise ValueError("linked output parent")
    output.mkdir(parents=True)
    result = copy.deepcopy(manifest)
    for case in result["cases"]:
        source = contained(root, case["case_id"])
        case.update(
            files={},
            load={"status": "NOT_EVALUATED"},
            statics={"status": "NOT_EVALUATED"},
        )
        try:
            if _alternate_schema(source):
                case["generation"] = {
                    "status": "NOT_EVALUATED",
                    "error": "alternate schema requires an explicit adapter",
                }
                continue
            (generator or generate_one)(
                source, output / relative_path(case["master"]).parent
            )
            case["generation"] = {"status": "PASS"}
        except ImportError:
            raise
        except Exception as exc:
            error = f"{type(exc).__name__}: {exc}"
            error = error.replace(str(root), "<spec-root>").replace(
                str(output), "<output-root>"
            )
            case["generation"] = {"status": "FAIL", "error": error}
        if digest(source) != case["source_sha256"]:
            raise ValueError("source digest changed during generation")
    # Nested cases share directories. Freeze every map against the final tree.
    for case in result["cases"]:
        if case["generation"]["status"] != "PASS":
            continue
        try:
            case_dir = contained(output, case["master"]).parent
            files = _generated_files(case_dir, "master.yml")
            case["files"] = {
                (case_dir / path).relative_to(output).as_posix(): sha
                for path, sha in files.items()
            }
        except (ValueError, TypeError, KeyError, OSError, yaml.YAMLError) as exc:
            error = f"{type(exc).__name__}: {exc}"
            error = error.replace(str(root), "<spec-root>").replace(
                str(output), "<output-root>"
            )
            case["generation"] = {"status": "FAIL", "error": error}
    return result


def _stage(stage: dict) -> None:
    if (
        not isinstance(stage, dict)
        or stage.get("status") not in STATES
        or not isinstance(stage.get("warnings"), list)
    ):
        raise ValueError("invalid stage status or warnings")
    if not all(isinstance(w, str) for w in stage["warnings"]):
        raise ValueError("warnings must contain text")
    if stage["status"] == "FAIL":
        if not isinstance(stage.get("error"), str) or not stage["error"].strip():
            raise ValueError("failure requires the exact error")
        object_name = stage.get("failing_object")
        reason = stage.get("object_unavailable_reason")
        named = isinstance(object_name, str) and bool(object_name.strip())
        unavailable = (
            object_name is None and isinstance(reason, str) and bool(reason.strip())
        )
        if not (named or unavailable):
            raise ValueError("failure requires object or explicit unavailable reason")
    elif stage.get("error") is not None:
        raise ValueError("non-failure cannot carry an error")


def verify_generated(manifest: dict, root: Path, output: Path) -> None:
    """Read back source and generated digests before joining a later native run."""
    _validate_manifest(manifest)
    for case in manifest["cases"]:
        if (
            digest(contained(Path(root).resolve(), case["case_id"]))
            != case["source_sha256"]
        ):
            raise ValueError("source digest mismatch")
        if case["generation"]["status"] != "PASS":
            continue
        directory = contained(Path(output).resolve(), case["master"]).parent
        expected = {
            (directory / p).relative_to(Path(output).resolve()).as_posix(): sha
            for p, sha in _bundle_digests(directory, "master.yml").items()
        }
        if expected != case["files"]:
            raise ValueError("generated bundle digest mismatch")
        files = _generated_files(directory, "master.yml")
        actual = {
            (directory / p).relative_to(Path(output).resolve()).as_posix(): sha
            for p, sha in files.items()
        }
        if actual != case["files"]:
            raise ValueError("generated bundle digest mismatch")


def _timestamps(record: dict) -> None:
    try:
        start = datetime.fromisoformat(record["started_at"].replace("Z", "+00:00"))
        finish = datetime.fromisoformat(record["finished_at"].replace("Z", "+00:00"))
        if start.tzinfo is None or finish.tzinfo is None or finish < start:
            raise ValueError("invalid run interval")
    except (KeyError, TypeError, AttributeError) as exc:
        raise ValueError("invalid run timestamps") from exc


def _validate_record(case: dict, record: dict, version: str) -> None:
    if case["generation"]["status"] != "PASS":
        raise ValueError("native results require successful generation")
    if (
        record["source_sha256"] != case["source_sha256"]
        or record["files"] != case["files"]
        or not case["files"]
    ):
        raise ValueError("input digest mismatch")
    if (
        record["solver_version"] != version
        or not isinstance(record["api_version"], str)
        or not record["api_version"].strip()
    ):
        raise ValueError("solver/API version mismatch or missing version")
    _timestamps(record)
    for stage in ("load", "statics"):
        _stage(record[stage])
    if (
        record["load"]["status"] == "FAIL"
        and record["statics"]["status"] != "NOT_ATTEMPTED"
    ):
        raise ValueError("load FAIL requires statics NOT_ATTEMPTED")
    if (
        record["statics"]["status"] == "PASS"
        and record["statics"].get("converged") is not True
    ):
        raise ValueError("statics PASS requires explicit convergence")
    if record["load"]["status"] != "PASS" and record["statics"]["status"] in {
        "PASS",
        "FAIL",
    }:
        raise ValueError("statics requires a passing load")
    if not record["evidence"]:
        raise ValueError("native evidence is required")


def reconcile_results(manifest: dict, records: list, evidence_root: Path) -> dict:
    """Join stored evidence without making a semantic-equivalence claim.

    File hashes bind the supplied records, not their authorship or physical truth.
    Independent native readback and claim-boundary review remain necessary.
    """
    _validate_manifest(manifest)
    result = copy.deepcopy(manifest)
    cases = {case["case_id"]: case for case in result["cases"]}
    for case in cases.values():
        case.update(
            load={"status": "NOT_EVALUATED"}, statics={"status": "NOT_EVALUATED"}
        )
        for field in ("api_version", "started_at", "finished_at", "evidence"):
            case.pop(field, None)
    seen = set()
    for record in records:
        name = record["case_id"]
        if name not in cases or name in seen:
            raise ValueError("unknown or duplicate result identity")
        seen.add(name)
        case = cases[name]
        _validate_record(case, record, manifest["solver_version"])
        for path, expected in record["evidence"].items():
            if digest(contained(Path(evidence_root).resolve(), path)) != expected:
                raise ValueError("native evidence digest mismatch")
        case.update(
            {
                key: copy.deepcopy(record[key])
                for key in (
                    "load",
                    "statics",
                    "api_version",
                    "started_at",
                    "finished_at",
                    "evidence",
                )
            }
        )
    # Policy: claim-boundary sections 1.1 and 4.2-4.3, not native-load shorthand.
    result["claim_update"] = "NOT_EVALUATED"
    return result


def render_summary(manifest: dict, report_date: str) -> str:
    """Render only recorded outcomes; HTML-escape all supplied text."""

    def esc(value: object) -> str:
        return html.escape(str(value))

    rows = []
    statuses = ("PASS", "FAIL", "NOT_ATTEMPTED", "NOT_EVALUATED")
    count_rows = []
    for stage in ("generation", "load", "statics"):
        counts = [
            sum(c[stage]["status"] == status for c in manifest["cases"])
            for status in statuses
        ]
        count_rows.append(
            f"<tr><th>{stage.title()}</th>"
            + "".join(f"<td>{count}</td>" for count in counts)
            + "</tr>"
        )
    for case in manifest["cases"]:
        cells = [case["case_id"]]
        for stage in ("generation", "load", "statics"):
            cells.append(json.dumps(case[stage], sort_keys=True))
        rows.append("<tr>" + "".join(f"<td>{esc(c)}</td>" for c in cells) + "</tr>")
    return (
        '<!doctype html><html lang="en"><meta charset="utf-8">'
        "<title>Specification library attestation analysis</title><body>"
        "<h1>Specification library attestation analysis</h1><h2>Summary</h2>"
        f"<p>{len(rows)} cases. "
        f"Requested solver version: {esc(manifest['solver_version'])}.</p>"
        "<h2>Revision history</h2><table><tr><th>Revision</th><th>Date</th><th>Change and reason</th></tr>"
        f"<tr><td>Initial</td><td>{esc(report_date)}</td><td>Recorded outcomes to expose missing evidence.</td></tr></table>"
        "<h2>Results</h2><table><tr><th>Specification</th><th>Generation</th><th>Load</th><th>Statics</th></tr>"
        + "".join(rows)
        + "</table><h2>Recorded status counts</h2><table><tr><th>Stage</th>"
        + "".join(f"<th>{status}</th>" for status in statuses)
        + "</tr>"
        + "".join(count_rows)
        + "</table><h2>Limitations and way forward</h2>"
        "<p>Claim update: NOT_EVALUATED. No native solver is run by this workflow. "
        "Stored records and digest checks do not establish native authenticity. "
        "Obtain reviewed native evidence, then baseline comparisons and difference classification "
        "per SEMANTIC_EQUIVALENCE_CLAIM_BOUNDARY sections 1.1 and 4 before any registry promotion.</p></body></html>\n"
    )


def _argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path("docs/domains/orcaflex"))
    parser.add_argument("--solver-version", required=True)
    parser.add_argument("--manifest", type=Path, required=True)
    parser.add_argument("--generate", type=Path, help="fresh offline output directory")
    parser.add_argument(
        "--input-manifest", type=Path, help="previous generation manifest"
    )
    parser.add_argument(
        "--generated-root", type=Path, help="previous output for digest readback"
    )
    parser.add_argument(
        "--results", type=Path, help="JSON list of stored native records"
    )
    parser.add_argument("--evidence-root", type=Path)
    parser.add_argument("--summary", type=Path)
    parser.add_argument("--report-date", help="explicit ISO date for the summary")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    """Inventory, generate or reconcile evidence from command-line arguments."""
    parser = _argument_parser()
    args = parser.parse_args(argv)
    if args.generate and args.input_manifest:
        parser.error("--generate and --input-manifest are mutually exclusive")
    if args.input_manifest and not args.generated_root:
        parser.error("--input-manifest requires --generated-root")
    if args.results and (
        not (args.generate or args.input_manifest) or not args.evidence_root
    ):
        parser.error("--results requires generated inputs and --evidence-root")
    if args.summary and not args.report_date:
        parser.error("--summary requires --report-date")
    try:
        destinations = [p for p in (args.manifest, args.summary) if p is not None]
        if len({p.resolve() for p in destinations}) != len(destinations):
            raise ValueError("output destinations must differ")
        if any(p.exists() or p.is_symlink() for p in destinations):
            raise ValueError("output destination already exists")
        if args.input_manifest:
            manifest = json.loads(args.input_manifest.read_text(encoding="utf-8"))
            if manifest["solver_version"] != args.solver_version:
                raise ValueError("pinned solver version mismatch")
            verify_generated(manifest, args.root, args.generated_root)
            # Reissued outcomes require fresh evidence readback via --results.
            manifest = reconcile_results(manifest, [], args.evidence_root)
        else:
            manifest = build_manifest(args.root, args.solver_version)
        if args.generate:
            manifest = generate_cases(manifest, args.root, args.generate)
        if args.results:
            manifest = reconcile_results(
                manifest, json.loads(args.results.read_text()), args.evidence_root
            )
        with args.manifest.open("x", encoding="utf-8") as stream:
            stream.write(dumps(manifest))
        if json.loads(args.manifest.read_text(encoding="utf-8")) != manifest:
            raise ValueError("manifest readback mismatch")
        if args.summary:
            with args.summary.open("x", encoding="utf-8") as stream:
                stream.write(render_summary(manifest, args.report_date))
    except (ValueError, OSError, KeyError, TypeError) as exc:
        parser.exit(1, f"Attestation failed: {type(exc).__name__}\n")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
