"""Read phase boundaries within timestamped Actions command blocks."""

import re
from datetime import datetime

ANSI = re.compile(r"\x1b\[[0-9;]*m")
GROUP = re.compile(r"(?m)(?=^\S+ ##\[group\])")
SOLVER = re.compile(r"##\[group\]Run [^\n]*xslope[^\n]*\bpytest\s+tests/geotechnical\b")
INSTALL_END = re.compile(r"^(?:Installed \d+ packages? in |Successfully installed )")
SUMMARY = re.compile(
    r"^(?:\d+ (?:passed|failed|skipped|errors?|xfailed|xpassed|deselected|warnings?)"
    r"(?:, )?)+ in (\d+(?:\.\d+)?)s(?: \([\d:]+\))?$"
)


def command_blocks(log):
    return GROUP.split(ANSI.sub("", log.lstrip("\ufeff")))


def pytest_phases(block):
    """Measure launch through final install and pytest's independent terminal time."""
    result = dict.fromkeys(("install_s", "pytest_s", "collected", "passed"))
    launched = installed = None
    in_pytest = False
    summaries = []
    for line in block.splitlines():
        stamp, _, msg = line.partition(" ")
        msg = msg.strip()
        if msg == "##[endgroup]":
            launched = stamp
        if launched is None:
            continue
        if not in_pytest and INSTALL_END.match(msg):
            installed = stamp
        collected = re.match(r"^collected (\d+) items?\b", msg)
        summary = SUMMARY.fullmatch(msg.strip("= "))
        if collected:
            result["collected"] = int(collected[1])
        if summary:
            passed = re.search(r"\b(\d+) passed\b", msg)
            result["passed"] = int(passed[1]) if passed else 0
            summaries.append(float(summary[1]))
        if ("test session starts" in msg or collected or summary or
                re.search(r"\[\s*\d+%\]$", msg) or msg.startswith("====")):
            in_pytest = True
    if len(summaries) == 1:
        result["pytest_s"] = summaries[0]
    elif summaries:  # A wrapper may retry. A single terminal report is required.
        result["passed"] = result["collected"] = None
    if in_pytest and installed and len(summaries) <= 1:
        result["install_s"] = seconds_between(launched, installed)
    return result


def seconds_between(start, end):
    try:
        dt = datetime.fromisoformat(end.replace("Z", "+00:00")) - datetime.fromisoformat(
            start.replace("Z", "+00:00"))
        if dt.total_seconds() >= 0:
            return round(dt.total_seconds(), 3)
    except (TypeError, ValueError):
        pass
    return None


def solver_timing(log_text):
    blocks = [block for block in command_blocks(log_text) if SOLVER.search(block.partition("\n")[0])]
    return pytest_phases(blocks[0] if len(blocks) == 1 else "")
