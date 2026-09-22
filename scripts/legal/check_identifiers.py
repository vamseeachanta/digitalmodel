"""Fail a commit that adds a client identifier to this public repository.

This repository had no value-matching identifier gate. The hook that was
supposed to be one pointed at a script outside the repository and was fail-open
-- `.pre-commit-config.yaml` records it "printing a PASS result with exit 0 over
a worktree that contained a live leak" -- and its replacement deliberately
matches no values, because no rule value was committed here. The result was 146
lines on the public remote binding a client to a job code, a CTR number or an
internal path.

The obstacle was real: a deny list of client names, committed to a public
repository, is a list of clients. `.legal-deny-list.yaml` resolves it by
carrying structural patterns in the clear and names as salted hashes, so a name
is matched without being published. An optional private list, pointed at by
``DIGITALMODEL_DENY_LIST``, extends it.

Design rules this follows, because the gate it replaces broke all three:

* **Fail closed.** A missing rules file, an unreadable file, a private list
  named but absent -- each exits non-zero. A gate that degrades quietly is not
  a gate.
* **Resolve its own inputs.** Paths are resolved against the repository root,
  not the working directory.
* **Never claim a pass it did not earn.** The summary states how many files
  were scanned and how many were skipped, and why.

Usage::

    python scripts/legal/check_identifiers.py [paths...]   # default: staged
    python scripts/legal/check_identifiers.py --all        # whole tracked tree
    python scripts/legal/check_identifiers.py --hash TOKEN # to extend the list
"""

from __future__ import annotations

import argparse
import hashlib
import os
import re
import subprocess
import sys

try:
    import yaml
except ImportError:  # pragma: no cover
    sys.exit("check_identifiers: PyYAML is required")

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
RULES = os.path.join(ROOT, ".legal-deny-list.yaml")
MAX_BYTES = 8 * 1024 * 1024
WORD = re.compile(r"[A-Za-z][A-Za-z0-9-]{3,}")

#: A vendor or interpreter install path discloses nothing about an engagement.
VENDOR_PATH = re.compile(
    r"(?i)[A-Za-z]:\\+(?:program files|programdata|windows|python\d*"
    r"|miniconda|anaconda)")


def load_rules() -> dict:
    if not os.path.isfile(RULES):
        sys.exit(f"check_identifiers: rules file missing: {RULES}")
    try:
        with open(RULES, encoding="utf-8") as fh:
            rules = yaml.safe_load(fh)
    except Exception as exc:  # noqa: BLE001
        sys.exit(f"check_identifiers: rules file unreadable: {exc}")
    if not isinstance(rules, dict) or "structural" not in rules:
        sys.exit("check_identifiers: rules file has no 'structural' section")

    private = os.environ.get("DIGITALMODEL_DENY_LIST")
    extra: list[str] = []
    if private:
        if not os.path.isfile(private):
            sys.exit(
                f"check_identifiers: DIGITALMODEL_DENY_LIST is set to "
                f"{private!r}, which does not exist. Refusing to continue "
                f"without the rules it names.")
        with open(private, encoding="utf-8") as fh:
            extra = [ln.strip().lower() for ln in fh
                     if ln.strip() and not ln.startswith("#")]
    rules["_private_names"] = extra
    return rules


def token_hash(token: str, salt: str) -> str:
    return hashlib.sha256(f"{salt}:{token.strip().lower()}".encode()).hexdigest()


def staged_files() -> list[str]:
    out = subprocess.run(
        ["git", "diff", "--cached", "--name-only", "--diff-filter=ACMR", "-z"],
        cwd=ROOT, capture_output=True, text=True)
    return [p for p in out.stdout.split("\0") if p]


def tracked_files() -> list[str]:
    out = subprocess.run(["git", "ls-files", "-z"], cwd=ROOT,
                         capture_output=True, text=True)
    return [p for p in out.stdout.split("\0") if p]


def check(paths: list[str], rules: dict) -> tuple[list[str], int, int]:
    salt = str(rules.get("salt", ""))
    hashed = {str(h).lower() for h in rules.get("hashed_names") or []}
    private = set(rules.get("_private_names") or [])
    excluded = {str(e["path"]).replace("\\", "/")
                for e in (rules.get("exclusions") or []) if "path" in e}
    compiled = []
    for rule in rules["structural"]:
        try:
            compiled.append((rule["id"], re.compile(rule["pattern"]),
                             rule.get("message", "")))
        except re.error as exc:
            sys.exit(f"check_identifiers: rule {rule.get('id')!r} "
                     f"has an invalid pattern: {exc}")

    findings: list[str] = []
    scanned = skipped = 0
    for rel in paths:
        norm = rel.replace("\\", "/")
        if norm in excluded:
            continue
        full = os.path.join(ROOT, rel.replace("/", os.sep))
        if not os.path.isfile(full):
            continue
        if os.path.getsize(full) > MAX_BYTES:
            skipped += 1
            continue
        with open(full, "rb") as fh:
            blob = fh.read()
        if b"\x00" in blob[:4096]:
            skipped += 1
            continue
        scanned += 1
        try:
            text = blob.decode("utf-8")
        except UnicodeDecodeError:
            text = blob.decode("latin-1", errors="replace")

        for n, line in enumerate(text.splitlines(), start=1):
            for rid, rx, msg in compiled:
                if rid == "mapped-drive-path" and VENDOR_PATH.search(line):
                    continue
                if rx.search(line):
                    findings.append(
                        f"{norm}:{n}: [{rid}] {msg.strip()}\n    {line.strip()[:160]}")
            if hashed or private:
                for word in WORD.findall(line):
                    low = word.lower()
                    if low in private or token_hash(low, salt) in hashed:
                        findings.append(
                            f"{norm}:{n}: [denied-name] a name on the deny "
                            f"list appears here\n    {line.strip()[:160]}")
                        break
    return findings, scanned, skipped


def main() -> int:
    ap = argparse.ArgumentParser(description="Client identifier gate")
    ap.add_argument("paths", nargs="*")
    ap.add_argument("--all", action="store_true",
                    help="scan the whole tracked tree, not just staged files")
    ap.add_argument("--hash", metavar="TOKEN",
                    help="print the salted hash of TOKEN, to extend the list")
    args = ap.parse_args()

    rules = load_rules()

    if args.hash:
        print(token_hash(args.hash, str(rules.get("salt", ""))))
        return 0

    paths = args.paths or (tracked_files() if args.all else staged_files())
    if not paths:
        print("check_identifiers: nothing to scan")
        return 0

    findings, scanned, skipped = check(paths, rules)
    print(f"check_identifiers: scanned {scanned} file(s), "
          f"skipped {skipped} (binary or over {MAX_BYTES // 1024 // 1024} MB)")
    if not findings:
        print("check_identifiers: no client identifier found")
        return 0

    print()
    print(f"check_identifiers: {len(findings)} finding(s) — this repository is "
          f"PUBLIC")
    for f in findings:
        print(f"  {f}")
    print()
    print("  Replace the identifier with a neutral placeholder. If a finding is "
          "a false positive, add a justified entry to the exclusions in "
          ".legal-deny-list.yaml rather than widening a pattern.")
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
