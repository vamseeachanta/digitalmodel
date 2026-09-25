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

Files the gate cannot read (a PDF, a solver binary) fail unless the committed
manifest ``.legal-uninspectable-baseline.txt`` lists their path with the same
sha256. After reviewing new or changed content, accept it with::

    python scripts/legal/check_identifiers.py --all --update-baseline
"""

from __future__ import annotations

import argparse
import functools
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
#: Uninspectable files accepted by path AND content digest. A count ceiling
#: accepted a replaced binary, or one deleted and another added.
DEFAULT_BASELINE = os.path.join(ROOT, ".legal-uninspectable-baseline.txt")
BASELINE_HEADER = """\
# Uninspectable files accepted by the identifier gate, by content digest.
# Format: sha256, two spaces, repository-relative path (as sha256sum prints).
#
# scripts/legal/check_identifiers.py cannot read these files (binary content,
# an office archive it cannot parse). A file passes only while its path is
# listed here with the same digest: a new file, a changed one or an unlisted
# one fails. Regenerate after reviewing the content, never to silence a failure:
#
#   python scripts/legal/check_identifiers.py --all --update-baseline
#
# Every line is content nobody has read; review the diff before committing it.
"""
_BASELINE_ROW = re.compile(r"([0-9a-f]{64})  (\S.*)")
_SHA256 = re.compile(r"[0-9a-f]{64}")
#: Text is read up to this size. Above it the file is uninspectable, which
#: fails: GitHub refuses files over 100 MB, so this bounds nothing real.
MAX_BYTES = 64 * 1024 * 1024
#: A candidate name: four or more letters, digits or hyphens holding at least
#: one letter. It may start with a digit -- a numbered vessel or hull -- which
#: a letter-first pattern tokenised without the digit, so it never matched.
WORD = re.compile(r"(?=[0-9-]*[A-Za-z])[A-Za-z0-9][A-Za-z0-9-]{3,}")

#: Read when present and DIGITALMODEL_DENY_LIST is unset. One entry per line:
#: a name, or ``re:<pattern>`` for a pattern that must itself stay private.
DEFAULT_PRIVATE = os.path.join(
    os.path.expanduser("~"), ".config", "digitalmodel", "identifier-deny-list.txt"
)
#: A line documenting a pattern has to show one. Marked with this, it is exempt
#: from the structural rules -- never from the name lists -- and only itself.
EXAMPLE_MARK = "identifier-gate: " + "example"

#: A vendor or interpreter install path discloses nothing about an engagement.
VENDOR_PATH = re.compile(
    r"(?i)[A-Za-z]:\\+(?:program files|programdata|windows|python\d*"
    r"|miniconda|anaconda)"
)


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
    # Hashes kept under the salt they were made with, when the salt was
    # rotated and their plaintext was not available to re-hash (C13/C16).
    # Malformed, the block would silently match nothing: refuse instead.
    legacy = rules.get("legacy_hashed_names")
    if legacy is not None and not (
        isinstance(legacy, dict)
        and isinstance(legacy.get("salt"), str)
        and legacy["salt"].strip()
        and isinstance(legacy.get("hashes"), list)
        and legacy["hashes"]
        and all(
            isinstance(h, str) and _SHA256.fullmatch(h.lower())
            for h in legacy["hashes"]
        )
    ):
        sys.exit(
            "check_identifiers: 'legacy_hashed_names' must be a mapping with a "
            "non-empty 'salt' and a non-empty list of sha256 'hashes'"
        )

    private = os.environ.get("DIGITALMODEL_DENY_LIST")
    if private:
        if not os.path.isfile(private):
            sys.exit(
                f"check_identifiers: DIGITALMODEL_DENY_LIST is set to "
                f"{private!r}, which does not exist. Refusing to continue "
                f"without the rules it names."
            )
    elif os.path.isfile(DEFAULT_PRIVATE):
        # Named by nobody, so its absence is not an error -- CI has none and
        # relies on the public pattern classes. Present, it is always read.
        private = DEFAULT_PRIVATE
    names: list[str] = []
    patterns: list[re.Pattern] = []
    if private:
        with open(private, encoding="utf-8-sig") as fh:
            for n, raw in enumerate(fh, start=1):
                ln = raw.strip()
                if not ln or ln.startswith("#"):
                    continue
                if ln.startswith("re:"):
                    try:
                        patterns.append(re.compile(ln[3:]))
                    except re.error:
                        # The pattern is private: name the line, not the text.
                        print(
                            f"check_identifiers: private list line {n} is "
                            f"not a valid regular expression. Refusing to "
                            f"continue without the rule it names.",
                            file=sys.stderr,
                        )
                        sys.exit(3)
                else:
                    names.append(ln.lower())
    rules["_private_names"] = names
    rules["_private_patterns"] = patterns
    return rules


def read_baseline(path: str) -> dict[str, str] | None:
    """Path -> sha256 from a baseline manifest, or None if it is malformed."""
    out: dict[str, str] = {}
    with open(path, encoding="utf-8") as fh:
        for n, raw in enumerate(fh, start=1):
            ln = raw.rstrip("\r\n")
            if not ln.strip() or ln.startswith("#"):
                continue
            m = _BASELINE_ROW.fullmatch(ln)
            if not m:
                print(
                    f"check_identifiers: baseline line {n} is not "
                    f"'<sha256>  <path>'. Refusing to continue.",
                    file=sys.stderr,
                )
                return None
            out[m.group(2)] = m.group(1)
    return out


def write_baseline(path: str, entries: dict[str, str]) -> None:
    rows = "".join(f"{sha}  {p}\n" for p, sha in sorted(entries.items()))
    with open(path, "w", encoding="utf-8", newline="\n") as fh:
        fh.write(BASELINE_HEADER + rows)


@functools.lru_cache(maxsize=1 << 18)
def token_hash(token: str, salt: str) -> str:
    return hashlib.sha256(f"{salt}:{token.strip().lower()}".encode()).hexdigest()


class Uninspectable(Exception):
    """Content the gate could not read. Never a pass."""


#: Office formats are zip archives of XML; their text is read, not skipped.
OFFICE = frozenset(
    {
        ".docx",
        ".docm",
        ".dotx",
        ".xlsx",
        ".xlsm",
        ".pptx",
        ".pptm",
        ".odt",
        ".ods",
        ".odp",
    }
)


def _git(args: list[str], stdin: bytes | None = None) -> bytes:
    """Run git at the repository root. A failure is an error, never "nothing".

    An empty result from a failed enumeration used to read as "nothing to
    scan", which is a pass the gate did not earn.
    """
    out = subprocess.run(["git", *args], cwd=ROOT, capture_output=True, input=stdin)
    if out.returncode != 0:
        sys.exit(
            f"check_identifiers: `git {' '.join(args)}` failed "
            f"(exit {out.returncode}): "
            f"{out.stderr.decode(errors='replace').strip()}"
        )
    return out.stdout


def staged_files() -> list[str]:
    out = _git(["diff", "--cached", "--name-only", "--diff-filter=ACMR", "-z"])
    return [p for p in out.decode("utf-8", "surrogateescape").split("\0") if p]


def tracked_files() -> list[str]:
    out = _git(["ls-files", "-z"])
    return [p for p in out.decode("utf-8", "surrogateescape").split("\0") if p]


#: Characters of path arguments per git invocation; Windows caps a command
#: line at 32,767.
ARG_BUDGET = 20000


def _batches(paths: list[str]) -> list[list[str]]:
    out: list[list[str]] = [[]]
    size = 0
    for p in paths:
        if out[-1] and size + len(p) + 1 > ARG_BUDGET:
            out.append([])
            size = 0
        out[-1].append(p)
        size += len(p) + 1
    return out


def index_blobs(paths: list[str]) -> dict[str, bytes | None]:
    """The staged content of each path -- what the commit will contain.

    Reading the working tree instead checks bytes the commit does not carry.
    Paths are resolved to object IDs through NUL-delimited ``ls-files -s``, and
    only object IDs go into the batch request: a path written into a
    newline-framed request could, with a newline in its name, make git answer
    for a different path. The response framing is validated completely.
    """
    if not paths:
        return {}
    # Batched: one invocation carrying every staged path overflowed the
    # Windows command line (WinError 206) on a commit of ~1,500 files.
    listing = b"".join(
        _git(["ls-files", "-s", "-z", "--", *batch]) for batch in _batches(paths)
    )
    entries: dict[str, tuple[str, str]] = {}
    for rec in listing.decode("utf-8", "surrogateescape").split("\0"):
        if not rec:
            continue
        meta, _, path = rec.partition("\t")
        mode, oid, stage = meta.split()
        if stage == "0":
            entries[path] = (mode, oid)

    wanted = sorted(
        {oid for p, (mode, oid) in entries.items() if p in paths and mode != "160000"}
    )
    contents: dict[str, bytes] = {}
    if wanted:
        raw = _git(
            ["cat-file", "--batch"], stdin="".join(f"{o}\n" for o in wanted).encode()
        )
        pos = 0
        for oid in wanted:
            end = raw.find(b"\n", pos)
            if end < 0:
                sys.exit("check_identifiers: truncated `git cat-file` response")
            header = raw[pos:end].decode("ascii", "replace").split()
            pos = end + 1
            if len(header) != 3 or header[0] != oid or header[1] != "blob":
                sys.exit(
                    f"check_identifiers: unexpected `git cat-file` "
                    f"response for {oid}: {' '.join(header)!r}"
                )
            size = int(header[2])
            body = raw[pos : pos + size]
            if len(body) != size or raw[pos + size : pos + size + 1] != b"\n":
                sys.exit(
                    f"check_identifiers: malformed `git cat-file` body " f"for {oid}"
                )
            contents[oid] = body
            pos += size + 1
        if pos != len(raw):
            sys.exit("check_identifiers: unconsumed `git cat-file` output")

    blobs: dict[str, bytes | None] = {}
    for p in paths:
        if p not in entries:
            blobs[p] = None  # not in the index
        elif entries[p][0] == "160000":
            blobs[p] = b""  # submodule pointer: no content
        else:
            blobs[p] = contents[entries[p][1]]
    return blobs


#: Leading bytes of each declared media type. A file is exempt only if its
#: content IS that type; text renamed to .png is read like any other text.
#: Signatures are specific: `true` (legacy Mac TrueType) and a bare RIFF header
#: were accepted once and let text through under .ttf and .webp.
MEDIA_MAGIC = {
    ".png": (b"\x89PNG\r\n\x1a\n",),
    ".jpg": (b"\xff\xd8\xff",),
    ".jpeg": (b"\xff\xd8\xff",),
    ".gif": (b"GIF87a", b"GIF89a"),
    ".bmp": (b"BM",),
    ".ico": (b"\x00\x00\x01\x00",),
    ".woff": (b"wOFF",),
    ".woff2": (b"wOF2",),
    ".ttf": (b"\x00\x01\x00\x00",),
    ".otf": (b"OTTO",),
}


def is_media(blob: bytes, ext: str) -> bool:
    if ext == ".webp":
        return blob[:4] == b"RIFF" and blob[8:12] == b"WEBP"
    magic = MEDIA_MAGIC.get(ext)
    return bool(magic) and blob.startswith(magic)


#: Bounds on unpacking an office archive: the file-size limit does not bound
#: what it expands to.
OFFICE_MAX_MEMBERS = 10000
OFFICE_MAX_EXPANDED = 256 * 1024 * 1024

#: Spreadsheet attributes that hold cell coordinates, which can look like a
#: job code (column B, row 1234). Skipped only when the value IS a coordinate.
_COORD_ATTRS = frozenset({"r", "ref", "sqref", "topLeftCell", "activeCell"})
_COORD = re.compile(
    r"\$?[A-Z]{1,3}\$?[0-9]+(?::\$?[A-Z]{1,3}\$?[0-9]+)?"
    r"(?:\s+\$?[A-Z]{1,3}\$?[0-9]+(?::\$?[A-Z]{1,3}\$?[0-9]+)?)*"
)

#: Elements whose runs are read together: Word paragraphs and runs, shared and
#: inline spreadsheet strings, drawing shapes and text bodies.
_RUN_CONTAINERS = frozenset({"p", "r", "si", "is", "sp", "txBody"})


def _office_text(blob: bytes) -> str:
    """Every text node and attribute value of every XML part, as whole words.

    Text is emitted in views: each node on its own, and each paragraph or string
    container run together -- Word and Excel split a name across runs freely.
    Character references are decoded by the XML parser. Any member that is
    neither XML nor a recognised image is uninspectable, not skipped.
    """
    import io
    import zipfile
    from xml.etree import ElementTree as ET

    out: list[str] = []
    try:
        with zipfile.ZipFile(io.BytesIO(blob)) as z:
            infos = z.infolist()
            if len(infos) > OFFICE_MAX_MEMBERS:
                raise Uninspectable(
                    f"office archive has {len(infos)} members "
                    f"(limit {OFFICE_MAX_MEMBERS})"
                )
            if sum(i.file_size for i in infos) > OFFICE_MAX_EXPANDED:
                raise Uninspectable(
                    "office archive expands beyond "
                    f"{OFFICE_MAX_EXPANDED // 1024 // 1024} MB"
                )
            for info in infos:
                name = info.filename
                if name.endswith("/"):
                    continue
                data = z.read(info)
                if name.endswith((".xml", ".rels", ".vml")):
                    try:
                        root = ET.fromstring(data)
                    except ET.ParseError as exc:
                        raise Uninspectable(
                            f"office part {name} is not well-formed XML: {exc}"
                        )
                    for el in root.iter():
                        for key, value in el.attrib.items():
                            local = key.rsplit("}", 1)[-1]
                            if local in _COORD_ATTRS and _COORD.fullmatch(value):
                                continue
                            out.append(value)
                        out.extend(t for t in (el.text, el.tail) if t)
                    for el in root.iter():
                        if el.tag.rsplit("}", 1)[-1] in _RUN_CONTAINERS:
                            out.append("".join(el.itertext()))
                    continue
                ext = os.path.splitext(name)[1].lower()
                if is_media(data, ext):
                    continue
                raise Uninspectable(f"office member {name} is not inspectable")
    except Uninspectable:
        raise
    except Exception as exc:  # noqa: BLE001
        raise Uninspectable(f"office archive unreadable: {exc}") from exc
    return "\n".join(out)


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


def _ascii_strings(blob: bytes) -> str:
    """Printable ASCII runs in raw bytes, like ``strings``.

    A UTF-16 decode turns ASCII bytes into unrelated code points, so an ASCII
    tail after a UTF-16 head would vanish from the decoded text. Reading the
    raw runs as well means content cannot hide in either encoding.
    """
    return "\n".join(m.decode("ascii") for m in re.findall(rb"[\x20-\x7e]{4,}", blob))


def text_of(blob: bytes, ext: str) -> str:
    """Decode content for inspection, or raise Uninspectable."""
    if len(blob) > MAX_BYTES:
        raise Uninspectable(f"over {MAX_BYTES // 1024 // 1024} MB")
    if ext in OFFICE:
        return _office_text(blob)
    enc = _looks_utf16(blob)
    if enc:
        # Lenient decode plus the raw ASCII runs: a malformed or mixed payload
        # is still read in both encodings rather than given up on.
        return blob.decode(enc, errors="replace") + "\n" + _ascii_strings(blob)
    if b"\x00" in blob[:4096]:
        raise Uninspectable("binary content")
    try:
        return blob.decode("utf-8-sig")
    except UnicodeDecodeError:
        return blob.decode("latin-1", errors="replace")


@functools.lru_cache(maxsize=1 << 18)
def _candidates(word: str) -> frozenset[str]:
    """The word, its parts either side of hyphens and digits, and each
    hyphen-separated piece with its leading digits removed.

    A denied name with a suffix attached -- ``name-archive``, ``name2`` -- is
    one word to the tokenizer, and its hash matches nothing. A name that
    holds a digit and follows one -- ``9name2`` -- is caught by dropping the
    leading digits, which is the letter-first token the word pattern yielded
    before it admitted a leading digit.
    """
    low = word.lower()
    out = {low}
    out.update(p for p in re.split(r"[-\d]+", low) if len(p) >= 4)
    for piece in [low, *low.split("-")]:
        stripped = re.sub(r"^[\d-]+", "", piece)
        if len(stripped) >= 4:
            out.add(stripped)
            out.update(p for p in re.split(r"[-\d]+", stripped) if len(p) >= 4)
    return frozenset(out)


def check(
    paths: list[str],
    rules: dict,
    staged: bool = False,
    digests: dict[str, tuple[str | None, str]] | None = None,
) -> tuple[list[str], int, list[str], list[str]]:
    """Returns findings, files scanned, media skipped, and uninspectable.

    ``digests``, when given, is filled for every uninspectable file with its
    repository-relative path -> (sha256 of the bytes that could not be read,
    or None when there were none, and the reason).
    """
    salt = str(rules.get("salt", ""))
    hashed = {str(h).lower() for h in rules.get("hashed_names") or []}
    legacy = rules.get("legacy_hashed_names") or {}
    legacy_salt = str(legacy.get("salt", ""))
    legacy_hashed = {str(h).lower() for h in legacy.get("hashes") or []}
    private = set(rules.get("_private_names") or [])
    excluded = {
        str(e["path"]).replace("\\", "/")
        for e in (rules.get("exclusions") or [])
        if "path" in e
    }
    media = {str(x).lower() for x in rules.get("binary_media_extensions") or []}
    compiled = []
    for rule in rules["structural"]:
        try:
            compiled.append(
                (rule["id"], re.compile(rule["pattern"]), rule.get("message", ""))
            )
        except re.error as exc:
            sys.exit(
                f"check_identifiers: rule {rule.get('id')!r} "
                f"has an invalid pattern: {exc}"
            )

    blobs = index_blobs(paths) if staged else {}
    findings: list[str] = []
    media_skipped: list[str] = []
    uninspectable: list[str] = []
    scanned = 0
    private_rx = list(rules.get("_private_patterns") or [])

    def scan(label: str, n: int | str, line: str) -> None:
        for rx in private_rx:
            if rx.search(line):
                findings.append(
                    f"{label}:{n}: [private-pattern] a pattern on "
                    f"the private list matches here" + _excerpt(line)
                )
                break
        structural = [] if EXAMPLE_MARK in line else compiled
        for rid, rx, msg in structural:
            pos = 0
            while (m := rx.search(line, pos)) is not None:
                # A vendor install path exempts itself, not its neighbours.
                # The pattern admits spaces, so one match can run on into the
                # next path; resume just past the exempt match's start rather
                # than after its end.
                if rid == "mapped-drive-path" and VENDOR_PATH.match(line, m.start()):
                    pos = m.start() + 1
                    continue
                findings.append(
                    f"{label}:{n}: [{rid}] {msg.strip()}" + _excerpt(line)
                )
                break
        if hashed or private or legacy_hashed:
            for word in WORD.findall(line):
                if any(
                    c in private
                    or token_hash(c, salt) in hashed
                    or (legacy_hashed and token_hash(c, legacy_salt) in legacy_hashed)
                    for c in _candidates(word)
                ):
                    findings.append(
                        f"{label}:{n}: [denied-name] a name on the "
                        f"deny list appears here" + _excerpt(line)
                    )
                    break

    for rel in paths:
        norm = rel.replace("\\", "/")
        if norm in excluded:
            continue
        # The path is content too: an identifier in a file name is published
        # whatever the file holds, and an empty file used to pass.
        full = os.path.join(ROOT, rel.replace("/", os.sep))
        try:
            shown = os.path.relpath(full, ROOT)
        except ValueError:  # another drive on Windows
            shown = ".."
        if shown.startswith(".."):
            shown = os.path.basename(full)
        before = len(findings)
        scan(norm, "path", shown.replace("\\", "/"))
        label = norm
        if len(findings) > before and not SHOW_LINES:
            # The path itself carries the identifier: name it by digest, for
            # every finding in this file, so the log does not publish it.
            label = "<path " + hashlib.sha256(norm.encode("utf-8")).hexdigest()[:12] + ">"
            findings[before:] = [label + f[len(norm):] for f in findings[before:]]
        ext = os.path.splitext(norm)[1].lower()
        blob: bytes | None = None
        try:
            if staged:
                blob = blobs.get(rel)
                if blob is None:
                    raise Uninspectable("not in the index")
            elif os.path.islink(full):
                # Git stores a symlink as its target text, and that text is
                # what is published. Following the link instead made a link
                # to a directory "not exist" on Linux and pass elsewhere.
                blob = os.readlink(full).encode("utf-8", "surrogateescape")
            else:
                if not os.path.isfile(full):
                    raise Uninspectable("file does not exist")
                if os.path.getsize(full) > MAX_BYTES:
                    raise Uninspectable(f"over {MAX_BYTES // 1024 // 1024} MB")
                with open(full, "rb") as fh:
                    blob = fh.read()
            # Exempt only if the bytes ARE the declared type, not just named so.
            if ext in media and is_media(blob, ext):
                media_skipped.append(norm)
                continue
            text = text_of(blob, ext)
        except Uninspectable as exc:
            uninspectable.append(f"{norm}: {exc}")
            if digests is not None:
                digests[shown.replace("\\", "/")] = (
                    hashlib.sha256(blob).hexdigest() if blob is not None else None,
                    str(exc),
                )
            continue
        scanned += 1
        for n, line in enumerate(text.splitlines(), start=1):
            scan(label, n, line)
    return findings, scanned, media_skipped, uninspectable


#: Findings name the file, line and rule only. Quoting the line would publish
#: the identifier in the CI log of a public repository; --show-lines quotes it
#: for a local run and is refused in CI.
SHOW_LINES = False


def _excerpt(line: str) -> str:
    return f"\n    {line.strip()[:160]}" if SHOW_LINES else ""


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
    ap.add_argument(
        "--all",
        action="store_true",
        help="scan the whole tracked tree, not just staged files",
    )
    ap.add_argument(
        "--hash",
        metavar="TOKEN",
        help="print the salted hash of TOKEN, to extend the list",
    )
    ap.add_argument(
        "--baseline",
        metavar="FILE",
        default=None,
        help="manifest of accepted uninspectable files (sha256, two spaces, "
        f"path); default {os.path.basename(DEFAULT_BASELINE)} at the "
        "repository root when present. A file the gate cannot read passes "
        "only if it is listed with the same digest.",
    )
    ap.add_argument(
        "--update-baseline",
        action="store_true",
        help="write the uninspectable files of this scan into the baseline "
        "and accept them. With --all the manifest becomes exactly this set; "
        "with paths only their entries change. Review the diff before "
        "committing it: every line is content nobody has read.",
    )
    ap.add_argument(
        "--show-lines",
        action="store_true",
        help="quote each offending line (local review only; refused in CI, "
        "where the log of a public repository is public)",
    )
    args = ap.parse_args()
    if args.show_lines:
        if os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"):
            print("check_identifiers: --show-lines is refused in CI", file=sys.stderr)
            return 3
        global SHOW_LINES
        SHOW_LINES = True

    rules = load_rules()

    if args.hash:
        print(token_hash(args.hash, str(rules.get("salt", ""))))
        return 0

    staged = not args.paths and not args.all
    if args.update_baseline and (
        os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS")
    ):
        # An update accepts every uninspectable file it finds, unread. In CI
        # nobody reviews that, so a new binary would pass by its own digest.
        print(
            "check_identifiers: --update-baseline refuses to run in CI (CI or "
            "GITHUB_ACTIONS is set). Regenerate the baseline locally, review "
            "the diff and commit it.",
            file=sys.stderr,
        )
        return 3
    if args.update_baseline and staged:
        print(
            "check_identifiers: --update-baseline needs --all or paths; the "
            "staged mode scans a partial set and would drop entries.",
            file=sys.stderr,
        )
        return 3
    if args.baseline:
        baseline_path = os.path.abspath(args.baseline)
        if not args.update_baseline and not os.path.isfile(baseline_path):
            print(
                f"check_identifiers: baseline {args.baseline!r} does not "
                f"exist. Refusing to continue without the list it names.",
                file=sys.stderr,
            )
            return 3
    else:
        baseline_path = DEFAULT_BASELINE
    baseline = read_baseline(baseline_path) if os.path.isfile(baseline_path) else {}
    if baseline is None:
        return 3

    paths = args.paths or (tracked_files() if args.all else staged_files())
    if not paths:
        # Enumeration succeeded (a failure exits above) and found nothing.
        print("check_identifiers: nothing to scan")
        return 0

    digests: dict[str, tuple[str | None, str]] = {}
    findings, scanned, media, uninspectable = check(
        paths, rules, staged=staged, digests=digests
    )
    print(
        f"check_identifiers: scanned {scanned} file(s); "
        f"{len(media)} declared binary media not inspected; "
        f"{len(uninspectable)} uninspectable"
    )
    status = 0

    if args.update_baseline:
        unreadable = [k for k, (sha, _) in digests.items() if sha is None]
        if unreadable:
            print(
                "check_identifiers: cannot baseline files whose bytes were not "
                "read: " + ", ".join(unreadable),
                file=sys.stderr,
            )
            return 3
        new = {} if args.all else dict(baseline)
        if not args.all:
            for p in paths:
                full = os.path.join(ROOT, p.replace("/", os.sep))
                try:
                    key = os.path.relpath(full, ROOT).replace("\\", "/")
                except ValueError:
                    key = p
                new.pop(key, None)
        new.update({k: sha for k, (sha, _) in digests.items()})
        write_baseline(baseline_path, new)
        print(
            f"check_identifiers: baseline {os.path.basename(baseline_path)} "
            f"now lists {len(new)} uninspectable file(s)"
        )
        baseline = new

    failing: list[str] = []
    accepted = 0
    for key, (sha, reason) in sorted(digests.items()):
        if sha is None:
            failing.append(f"{key}: {reason}")
        elif key not in baseline:
            failing.append(f"{key}: {reason}; not in the baseline")
        elif baseline[key] != sha:
            failing.append(f"{key}: {reason}; changed since the baseline")
        else:
            accepted += 1
    if accepted:
        print(
            f"check_identifiers: {accepted} uninspectable file(s) match the "
            f"baseline by path and sha256"
        )
    if args.all and baseline:
        stale = sorted(set(baseline) - set(digests))
        if stale:
            print(
                f"check_identifiers: {len(stale)} baseline entr(y/ies) no longer "
                f"uninspectable; --update-baseline drops them"
            )
    if failing:
        status = 2
        print()
        print(
            f"check_identifiers: {len(failing)} file(s) could not be "
            f"inspected and are not accepted — content that was not read has "
            f"not passed:"
        )
        for u in failing:
            print(f"  {u}")
        print(
            "  Convert to text, remove the file, declare its type in "
            "binary_media_extensions with a reason, or -- after reviewing the "
            "content -- accept it with --update-baseline."
        )
    if findings:
        status = 1
        print()
        print(
            f"check_identifiers: {len(findings)} finding(s) — this repository "
            f"is PUBLIC"
        )
        for f in findings:
            print(f"  {f}")
        print()
        print(
            "  Replace the identifier with a neutral placeholder. If a finding "
            "is a false positive, add a justified entry to the exclusions in "
            ".legal-deny-list.yaml rather than widening a pattern."
        )
    if status == 0:
        print("check_identifiers: no client identifier found")
    return status


if __name__ == "__main__":
    raise SystemExit(main())
