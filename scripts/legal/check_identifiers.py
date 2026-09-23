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
#: Text is read up to this size. Above it the file is uninspectable, which
#: fails: GitHub refuses files over 100 MB, so this bounds nothing real.
MAX_BYTES = 64 * 1024 * 1024
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


class Uninspectable(Exception):
    """Content the gate could not read. Never a pass."""


#: Office formats are zip archives of XML; their text is read, not skipped.
OFFICE = frozenset({".docx", ".docm", ".dotx", ".xlsx", ".xlsm", ".pptx",
                    ".pptm", ".odt", ".ods", ".odp"})


def _git(args: list[str], stdin: bytes | None = None) -> bytes:
    """Run git at the repository root. A failure is an error, never "nothing".

    An empty result from a failed enumeration used to read as "nothing to
    scan", which is a pass the gate did not earn.
    """
    out = subprocess.run(["git", *args], cwd=ROOT, capture_output=True,
                         input=stdin)
    if out.returncode != 0:
        sys.exit(f"check_identifiers: `git {' '.join(args)}` failed "
                 f"(exit {out.returncode}): "
                 f"{out.stderr.decode(errors='replace').strip()}")
    return out.stdout


def staged_files() -> list[str]:
    out = _git(["diff", "--cached", "--name-only", "--diff-filter=ACMR", "-z"])
    return [p for p in out.decode("utf-8", "surrogateescape").split("\0") if p]


def tracked_files() -> list[str]:
    out = _git(["ls-files", "-z"])
    return [p for p in out.decode("utf-8", "surrogateescape").split("\0") if p]


def index_blobs(paths: list[str]) -> dict[str, bytes]:
    """The staged content of each path -- what the commit will contain.

    Reading the working tree instead checks bytes the commit does not carry.
    """
    if not paths:
        return {}
    raw = _git(["cat-file", "--batch"],
               stdin="".join(f":{p}\n" for p in paths).encode())
    blobs, pos = {}, 0
    for p in paths:
        end = raw.index(b"\n", pos)
        header = raw[pos:end].decode(errors="replace").split()
        pos = end + 1
        if len(header) < 3 or header[1] != "blob":
            blobs[p] = None
            continue
        size = int(header[2])
        blobs[p] = raw[pos:pos + size]
        pos += size + 1
    return blobs


def _office_text(blob: bytes) -> str:
    import io
    import zipfile

    try:
        with zipfile.ZipFile(io.BytesIO(blob)) as z:
            parts = [z.read(n).decode("utf-8", "replace") for n in z.namelist()
                     if n.endswith((".xml", ".rels"))]
    except Exception as exc:  # noqa: BLE001
        raise Uninspectable(f"office archive unreadable: {exc}") from exc
    # Tags become spaces so words in adjacent runs stay separate.
    return "\n".join(re.sub(r"<[^>]+>", " ", x) for x in parts)


def _looks_utf16(blob: bytes) -> str | None:
    if blob[:2] in (b"\xff\xfe", b"\xfe\xff"):
        return "utf-16"
    sample = blob[:4096]
    if len(sample) >= 4:
        even, odd = sample[0::2], sample[1::2]
        if odd.count(0) > 0.9 * len(odd) and even.count(0) < 0.1 * len(even):
            return "utf-16-le"
        if even.count(0) > 0.9 * len(even) and odd.count(0) < 0.1 * len(odd):
            return "utf-16-be"
    return None


def text_of(blob: bytes, ext: str) -> str:
    """Decode content for inspection, or raise Uninspectable."""
    if len(blob) > MAX_BYTES:
        raise Uninspectable(f"over {MAX_BYTES // 1024 // 1024} MB")
    if ext in OFFICE:
        return _office_text(blob)
    enc = _looks_utf16(blob)
    if enc:
        try:
            return blob.decode(enc)
        except UnicodeDecodeError as exc:
            raise Uninspectable(f"undecodable {enc}: {exc}") from exc
    if b"\x00" in blob[:4096]:
        raise Uninspectable("binary content")
    try:
        return blob.decode("utf-8-sig")
    except UnicodeDecodeError:
        return blob.decode("latin-1", errors="replace")


def _candidates(word: str) -> set[str]:
    """The word, and its parts either side of hyphens and digits.

    A denied name with a suffix attached -- ``name-archive``, ``name2`` -- is
    one word to the tokenizer, and its hash matches nothing.
    """
    low = word.lower()
    out = {low}
    out.update(p for p in re.split(r"[-\d]+", low) if len(p) >= 4)
    return out


def check(paths: list[str], rules: dict, staged: bool = False
          ) -> tuple[list[str], int, list[str], list[str]]:
    """Returns findings, files scanned, media skipped, and uninspectable."""
    salt = str(rules.get("salt", ""))
    hashed = {str(h).lower() for h in rules.get("hashed_names") or []}
    private = set(rules.get("_private_names") or [])
    excluded = {str(e["path"]).replace("\\", "/")
                for e in (rules.get("exclusions") or []) if "path" in e}
    media = {str(x).lower() for x in rules.get("binary_media_extensions") or []}
    compiled = []
    for rule in rules["structural"]:
        try:
            compiled.append((rule["id"], re.compile(rule["pattern"]),
                             rule.get("message", "")))
        except re.error as exc:
            sys.exit(f"check_identifiers: rule {rule.get('id')!r} "
                     f"has an invalid pattern: {exc}")

    blobs = index_blobs(paths) if staged else {}
    findings: list[str] = []
    media_skipped: list[str] = []
    uninspectable: list[str] = []
    scanned = 0
    for rel in paths:
        norm = rel.replace("\\", "/")
        if norm in excluded:
            continue
        ext = os.path.splitext(norm)[1].lower()
        if ext in media:
            media_skipped.append(norm)
            continue
        try:
            if staged:
                blob = blobs.get(rel)
                if blob is None:
                    raise Uninspectable("not in the index")
            else:
                full = os.path.join(ROOT, rel.replace("/", os.sep))
                if not os.path.isfile(full):
                    raise Uninspectable("file does not exist")
                if os.path.getsize(full) > MAX_BYTES and ext not in OFFICE:
                    raise Uninspectable(
                        f"over {MAX_BYTES // 1024 // 1024} MB")
                with open(full, "rb") as fh:
                    blob = fh.read()
            text = text_of(blob, ext)
        except Uninspectable as exc:
            uninspectable.append(f"{norm}: {exc}")
            continue
        scanned += 1

        for n, line in enumerate(text.splitlines(), start=1):
            for rid, rx, msg in compiled:
                pos = 0
                while (m := rx.search(line, pos)) is not None:
                    # A vendor install path exempts itself, not its neighbours.
                    # The pattern admits spaces, so one match can run on into
                    # the next path; resume just past the exempt match's start
                    # rather than after its end.
                    if (rid == "mapped-drive-path"
                            and VENDOR_PATH.match(line, m.start())):
                        pos = m.start() + 1
                        continue
                    findings.append(
                        f"{norm}:{n}: [{rid}] {msg.strip()}\n"
                        f"    {line.strip()[:160]}")
                    break
            if hashed or private:
                for word in WORD.findall(line):
                    if any(c in private or token_hash(c, salt) in hashed
                           for c in _candidates(word)):
                        findings.append(
                            f"{norm}:{n}: [denied-name] a name on the deny "
                            f"list appears here\n    {line.strip()[:160]}")
                        break
    return findings, scanned, media_skipped, uninspectable


def main() -> int:
    # A finding line can carry any character from the file it quotes. On a
    # console whose encoding cannot represent it (cp1252 on Windows) printing
    # raised mid-report and the report was lost.
    for stream in (sys.stdout, sys.stderr):
        try:
            stream.reconfigure(encoding="utf-8", errors="replace")
        except (AttributeError, ValueError):
            pass
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

    staged = not args.paths and not args.all
    paths = args.paths or (tracked_files() if args.all else staged_files())
    if not paths:
        # Enumeration succeeded (a failure exits above) and found nothing.
        print("check_identifiers: nothing to scan")
        return 0

    findings, scanned, media, uninspectable = check(paths, rules, staged=staged)
    print(f"check_identifiers: scanned {scanned} file(s); "
          f"{len(media)} declared binary media not inspected; "
          f"{len(uninspectable)} uninspectable")
    status = 0
    if uninspectable:
        status = 2
        print()
        print(f"check_identifiers: {len(uninspectable)} file(s) could not be "
              f"inspected — content that was not read has not passed:")
        for u in uninspectable:
            print(f"  {u}")
        print("  Convert to text, remove the file, or declare its type in "
              "binary_media_extensions with a reason.")
    if findings:
        status = 1
        print()
        print(f"check_identifiers: {len(findings)} finding(s) — this repository "
              f"is PUBLIC")
        for f in findings:
            print(f"  {f}")
        print()
        print("  Replace the identifier with a neutral placeholder. If a finding "
              "is a false positive, add a justified entry to the exclusions in "
              ".legal-deny-list.yaml rather than widening a pattern.")
    if status == 0:
        print("check_identifiers: no client identifier found")
    return status


if __name__ == "__main__":
    raise SystemExit(main())
