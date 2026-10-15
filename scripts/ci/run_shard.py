# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Retry a bounded set of failed pytest IDs once and record recovered failures.

The register is informational and is never read by this module. Child output is
inherited so both attempts appear in full in the calling workflow log.
"""

import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import time
import xml.etree.ElementTree as ET

TIMEOUT_CODES = {124, 137, 143, 130}
VALUE_FLAGS = {
    "-p", "-o", "-c", "-m", "-k", "--override-ini", "--rootdir", "--confcutdir",
    "--basetemp", "--tb", "--capture", "--maxfail", "--durations", "--durations-min",
    "--color", "--code-highlight", "--import-mode", "--junit-prefix", "--junit-suite-name",
    "--log-level", "--log-format", "--log-date-format", "--log-file", "--log-file-level",
    "--log-cli-level", "--timeout", "--timeout-method", "--cov", "--cov-report",
    "--cov-config", "--cov-fail-under", "--ignore", "--ignore-glob", "--deselect",
    "--verbosity", "--assert", "--show-capture", "--pythonwarnings", "-W",
}


def decision(ids: list[str], code: int, maximum: int, collected: bool,
             collection_error: bool = False, timeout: bool = False) -> dict:
    """Return a conservative decision shared by XML and log readers."""
    reason = "eligible"
    if code == 5:
        reason = "no-tests"
    elif code in TIMEOUT_CODES or timeout:
        reason = "timeout-or-termination"
    elif len(ids) > maximum:
        reason = "over-limit"
    elif collection_error:
        reason = "collection-error"
    elif not collected or not ids or code != 1:
        reason = "unproven-test-failure"
    return {"retry": reason == "eligible", "kind": "test-ids", "ids": ids, "reason": reason}


def node_id(case: ET.Element) -> str:
    """Recover the node ID from pytest xunit1 or xunit2 testcase attributes."""
    name = case.get("name", "")
    classname = case.get("classname", "").split(".")
    file = case.get("file")
    if file:
        parts = Path(file).with_suffix("").parts
        classes = classname[len(parts):]
    else:
        for end in range(len(classname), 0, -1):
            candidate = Path(*classname[:end]).with_suffix(".py")
            if candidate.is_file():
                file = candidate.as_posix()
                classes = classname[end:]
                break
        else:
            raise ValueError("testcase path cannot be established")
    if not name or not Path(file).is_file():
        raise ValueError("testcase identity cannot be established")
    return "::".join([file, *classes, name])


def read_cases(path: Path) -> list[ET.Element]:
    """Read testcase elements from a pytest JUnit document."""
    root = ET.parse(path).getroot()
    if root.tag not in {"testsuite", "testsuites"}:
        raise ValueError("not a JUnit document")
    return list(root.iter("testcase"))


def junit_decision(path: Path, code: int, maximum: int) -> dict:
    """Require a parsed report, collected tests and bounded concrete failures."""
    try:
        cases = read_cases(path)
        failures = [case for case in cases if case.find("failure") is not None or case.find("error") is not None]
        collection = any(
            case.find("error") is not None and (
                not case.get("classname") or
                "collect" in ET.tostring(case, encoding="unicode").lower()
            ) for case in failures
        )
        timeout = any(re.search(r"\b(?:timeout|timed out)\b", ET.tostring(case, encoding="unicode"), re.I) for case in failures)
        ids = sorted({node_id(case) for case in failures}) if not collection else []
        return decision(ids, code, maximum, bool(cases), collection, timeout)
    except (OSError, ET.ParseError, ValueError):
        return {"retry": False, "kind": "test-ids", "ids": [], "reason": "unreadable-report"}


def log_decision(content: str, maximum: int) -> dict:
    """Inspect cached hosted logs without executing commands from their text."""
    content = re.sub(r"\x1b\[[0-9;]*m", "", content)
    content = re.sub(r"(?m)^\d{4}-\d{2}-\d{2}T\S+\s+", "", content)
    codes = re.findall(r"Process completed with exit code (\d+)\.", content)
    code = int(codes[-1]) if codes else -1
    timeout = bool(re.search(r"job.*(?:timed out|timeout)|operation was canceled", content, re.I))
    if code >= 128 and code not in TIMEOUT_CODES and not timeout:
        return {"retry": True, "kind": "signal-exit", "ids": [], "reason": "signal-exit"}
    ids = sorted(set(re.findall(r"(?m)^(?:FAILED|ERROR) (\S.*?)(?: - .*|\r?$)", content)))
    collection = bool(re.search(r"ERROR collecting|(?m:^ERROR [^\n]*\.py\s*$)", content))
    collected = bool(re.search(r"collected [1-9]\d* items|[1-9]\d* passed", content))
    return decision(ids, code, maximum, collected, collection, timeout)


def retry_command(command: list[str], ids: list[str], report: Path) -> list[str]:
    """Retain the original interpreter, environment launcher and non-path flags."""
    try:
        start = command.index("pytest") + 1
    except ValueError:
        raise ValueError("pytest command is required for an ID retry") from None
    prefix = command[:start]
    flags: list[str] = []
    args = iter(command[start:])
    for arg in args:
        if arg in {"--junitxml", "--junit-xml"}:
            next(args)
        elif arg.startswith(("--junitxml=", "--junit-xml=")):
            continue
        elif arg in VALUE_FLAGS:
            flags.extend([arg, next(args)])
        elif arg.startswith("-"):
            flags.append(arg)
        elif arg.startswith("tests/") or Path(arg.split("::")[0]).exists():
            continue
        else:
            raise ValueError("ambiguous positional pytest argument")
    return prefix + ids + flags + [f"--junitxml={report}"]


def execute(command: list[str]) -> int:
    """Run once with inherited output and normalize POSIX signal exit codes."""
    print("Running shard attempt", flush=True)
    code = subprocess.run(command, check=False).returncode
    return 128 - code if code < 0 else code


def fresh(path: Path, started: int) -> bool:
    """Reject a report left by an earlier invocation."""
    return path.is_file() and path.stat().st_mtime_ns >= started


def record(path: Path, ids: list[str], message: str, kind: str) -> None:
    """Write compact evidence with run identity and observed runner CPU model."""
    cpu = "unknown"
    try:
        for line in Path("/proc/cpuinfo").read_text().splitlines():
            if line.startswith("model name"):
                cpu = line.split(":", 1)[1].strip()
                break
    except OSError:
        pass
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {"ids": ids, "first_failure_message": message[:500],
               "sha": os.environ.get("GITHUB_SHA", "unknown"),
               "run_id": os.environ.get("GITHUB_RUN_ID", "unknown"),
               "runner_cpu": cpu, "kind": kind}
    path.write_text(json.dumps(payload, indent=2) + "\n")
    for test_id in ids:
        escaped = test_id.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")
        print(f"::warning::flaky, passed on retry: {escaped}", flush=True)


def retry_tests(args: argparse.Namespace, command: list[str], code: int, started: int) -> int:
    """Retry each proven failed ID once; missing or nonpassing results stay red."""
    if not fresh(args.junit, started):
        return 1
    result = junit_decision(args.junit, code, args.max_retry_ids)
    if not result["retry"]:
        return 1
    retry_report = args.junit.with_name(args.junit.stem + "-retry.xml")
    try:
        retry = retry_command(command, result["ids"], retry_report)
    except (ValueError, StopIteration):
        return 1
    failures = [case for case in read_cases(args.junit) if case.find("failure") is not None or case.find("error") is not None]
    failure = next(child for child in failures[0] if child.tag in {"failure", "error"})
    message = (failure.get("message", "") + "\n" + (failure.text or "")).strip()
    retry_started = time.time_ns()
    retry_code = execute(retry)
    passed: set[str] = set()
    try:
        if fresh(retry_report, retry_started):
            passed = {node_id(case) for case in read_cases(retry_report)
                      if not any(case.find(tag) is not None for tag in ("failure", "error", "skipped"))}
    except (OSError, ET.ParseError, ValueError):
        passed = set()
    recovered = sorted(set(result["ids"]) & passed)
    record(args.junit.parent / f"flaky-{args.shard}.json", recovered, message, "retry-pass")
    return 0 if retry_code == 0 and set(result["ids"]) == set(recovered) else 1


def main() -> int:
    """Parse the wrapper interface and run at most two attempts."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--shard", default="shard")
    parser.add_argument("--junit", type=Path)
    parser.add_argument("--max-retry-ids", type=int, default=20)
    parser.add_argument("--signal-retry", action="store_true")
    parser.add_argument("--decide-only", action="store_true")
    parser.add_argument("--from-log", type=Path)
    parser.add_argument("command", nargs=argparse.REMAINDER)
    args = parser.parse_args()
    if not 1 <= args.max_retry_ids <= 20 or not re.fullmatch(r"[A-Za-z0-9_.-]+", args.shard):
        parser.error("retry limit must be 1 to 20 and shard must be a simple name")
    if args.decide_only:
        if not args.from_log:
            parser.error("--from-log is required for --decide-only")
        print(json.dumps(log_decision(args.from_log.read_text(), args.max_retry_ids), sort_keys=True))
        return 0
    command = args.command[1:] if args.command[:1] == ["--"] else args.command
    if not command or args.junit is None:
        parser.error("--junit and a command after -- are required")
    started = time.time_ns()
    code = execute(command)
    if code == 0 or code == 5 or code in TIMEOUT_CODES:
        return code
    if args.signal_retry:
        if code < 128:
            return code
        retry_code = execute(command)
        record(args.junit.parent / f"flaky-{args.shard}.json", [args.shard] if retry_code == 0 else [],
               f"signal exit {code}", "signal-exit")
        return retry_code
    return retry_tests(args, command, code, started)


if __name__ == "__main__":
    sys.exit(main())
