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

Known limits
------------
The output redactor is a heuristic; the gate's findings fail closed whatever
it misses. A public pattern match is widened to the whitespace-delimited token
around it, so a path containing a space (``/home/First Last/...``) keeps the
part after the space visible unless another rule catches it, and the widening
can also swallow adjacent diagnostic text such as ``source=``. It is not a
complete path redactor. Public CI runs without the private list, so its log
can show only what the committed hashed names and public classes miss, and
the gate fails on any finding regardless.
"""

from __future__ import annotations

import argparse
import contextlib
import functools
import hashlib
import hmac
import os
import re
import secrets
import subprocess
import sys
import traceback

try:
    import yaml
except ImportError:  # pragma: no cover
    yaml = None  # refused by load_rules(), through emit()

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


#: Key for the labels that stand in for a path or an office member name.
#: A bare sha256 of the name could be recomputed offline from candidate
#: names and compared with a published log; keyed by a random salt made
#: once per main() call, a label correlates repeat findings within one run
#: and reveals nothing across runs. Never printed, never persisted.
_LABEL_SALT: bytes | None = None


def new_label_salt() -> None:
    """Start a run: every label after this is keyed by a fresh salt."""
    global _LABEL_SALT
    _LABEL_SALT = secrets.token_hex(16).encode("ascii")


def _label_digest(text: str) -> str:
    if _LABEL_SALT is None:
        # A library caller that never entered main() still gets a keyed
        # label, stable for the life of the process.
        new_label_salt()
    assert _LABEL_SALT is not None
    mac = hmac.new(_LABEL_SALT, text.encode("utf-8"), hashlib.sha256)
    return mac.hexdigest()[:12]


def path_label(path: str) -> str:
    """A path named by a per-run keyed digest, for a path that carries an
    identifier: the same within one run, unlinkable across runs."""
    return "<path " + _label_digest(path) + ">"


# -- the output sink ------------------------------------------------------------
#
# Every line this module writes goes through emit(). Redacting call site by
# call site missed git's stderr, exception text, tracebacks and an
# acknowledgement naming a file; one sink cannot be bypassed that way, and
# tests/legal fail if a bare print() returns.

#: Pattern classes redacted from every printed line whatever the rules file
#: says: a diagnostic can be written before the rules are read, or after they
#: failed to load.
REDACT_PUBLIC = (
    # A user profile path, Windows or POSIX: the account name is personal.
    re.compile(r"(?i)\b[A-Z]:(?:\\|/)+Users(?:\\|/)+[^\\/\s'\"<>]+"),
    re.compile(r"(?<![\w.])/(?:home|Users)/[^/\s'\"<>]+"),
    # A UNC share names a host and a share.
    re.compile(r"\\\\+[A-Za-z0-9][A-Za-z0-9._$-]*\\+[^\\\s'\"<>]*"),
    # A host name in the fleet's naming scheme.
    re.compile(
        r"(?i)\b[a-z]{2,6}-(?:[a-z]{2,4}-)?(?:rds|ansys|ws|host|fs|srv|dc)[0-9]{2,}\b"
    ),
    # An e-mail address names a person or an organisation.
    re.compile(r"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}"),
)
#: Characters that end a redacted run: a public match covers the whole token
#: around it (the rest of a path is as identifying as its start).
_RUN_END = frozenset(" \t\r\n'\"<>()[]{},;|`")


class Redactor:
    """Replaces every identifier the gate knows with ``[redacted]``.

    Matched: a deny-listed name, tokenised and hashed exactly as the gate
    does (hashed, legacy-hashed, private plain names); a private pattern; a
    private or extra name as a phrase -- case-insensitively, with any run of
    whitespace (space, tab, newline, no-break space) between its words; and
    the public pattern classes -- built in, plus the rules' structural
    patterns.

    The marker is a constant. A digest of the matched text, even salted,
    lets anyone holding the public salt test candidate names offline against
    a published log. Phrases are matched on the original text with
    ``re.IGNORECASE``: offsets found in ``text.lower()`` drift when lowering
    changes a character's length (U+0130), which left a phrase visible.
    """

    def __init__(self, rules: dict | None = None, names=()) -> None:
        rules = rules or {}
        self.salt = str(rules.get("salt", ""))
        self.hashed = {str(h).lower() for h in rules.get("hashed_names") or []}
        legacy = rules.get("legacy_hashed_names")
        legacy = legacy if isinstance(legacy, dict) else {}
        self.legacy_salt = str(legacy.get("salt", ""))
        self.legacy = {str(h).lower() for h in legacy.get("hashes") or []}
        self.private = {str(n).lower() for n in rules.get("_private_names") or []}
        phrases = self.private | {str(n).lower() for n in names}
        # Whitespace inside a phrase matches any whitespace run: a name split
        # by a newline or a no-break space is still the name.
        words = {tuple(p.split()) for p in phrases if len(" ".join(p.split())) >= 4}
        self.phrases = [
            re.compile(r"\s+".join(re.escape(w) for w in ws), re.IGNORECASE)
            for ws in sorted(words, key=lambda ws: len(" ".join(ws)), reverse=True)
        ]
        self.patterns = list(REDACT_PUBLIC)
        for rule in rules.get("structural") or []:
            try:
                self.patterns.append(re.compile(str(rule["pattern"])))
            except (re.error, KeyError, TypeError):
                continue
        self.patterns.extend(rules.get("_private_patterns") or [])

    def _denied(self, word: str) -> bool:
        return any(
            c in self.private
            or (self.hashed and token_hash(c, self.salt) in self.hashed)
            or (self.legacy and token_hash(c, self.legacy_salt) in self.legacy)
            for c in _candidates(word)
        )

    #: The replacement for every redacted run. Constant, so a published log
    #: cannot be tested against candidate names.
    MARKER = "[redacted]"

    def redact(self, text: str) -> str:
        spans: list[tuple[int, int]] = []
        for rx in self.patterns:
            for m in rx.finditer(text):
                if m.end() == m.start():
                    continue
                s, e = m.start(), m.end()
                while s > 0 and text[s - 1] not in _RUN_END:
                    s -= 1
                while e < len(text) and text[e] not in _RUN_END:
                    e += 1
                spans.append((s, e))
        for m in WORD.finditer(text):
            if self._denied(m.group(0)):
                spans.append(m.span())
        for phrase in self.phrases:
            spans.extend(m.span() for m in phrase.finditer(text))
        if not spans:
            return text
        spans.sort()
        merged = [list(spans[0])]
        for s, e in spans[1:]:
            if s <= merged[-1][1]:
                merged[-1][1] = max(merged[-1][1], e)
            else:
                merged.append([s, e])
        out, pos = [], 0
        for s, e in merged:
            out.append(text[pos:s])
            out.append(self.MARKER)
            pos = e
        out.append(text[pos:])
        return "".join(out)


class _Sink:
    """The output policy. main() resets it on entry and on exit, so no call
    can leave --show-lines on for the next."""

    def __init__(self) -> None:
        self.reset()

    def reset(self) -> None:
        self.show = False
        self.redactor = Redactor()


_SINK = _Sink()


def set_output_policy(rules: dict | None, show: bool = False) -> None:
    """Redact against *rules* (None: the public classes only); *show*
    disables redaction and is for a local --show-lines run only."""
    _SINK.redactor = Redactor(rules)
    _SINK.show = bool(show)


def emit(message: object = "", *, err: bool = False) -> None:
    """The only writer to stdout and stderr in this module."""
    text = str(message)
    if not _SINK.show:
        text = _SINK.redactor.redact(text)
    stream = sys.stderr if err else sys.stdout
    stream.write(text + "\n")


def die(message: str, code: int = 1) -> None:
    """Emit *message* on stderr and exit with *code*."""
    emit(message, err=True)
    raise SystemExit(code)


#: Exit status for an exception the gate did not anticipate.
EXIT_INTERNAL = 4


class StageError(Exception):
    """An exception raised while the gate was at *stage*. Only the stage and
    the exception type are printed: the message and the traceback can quote
    a file name or file content."""

    def __init__(self, stage: str, cause: BaseException) -> None:
        super().__init__(stage)
        self.stage = stage
        self.cause = cause


@contextlib.contextmanager
def stage(name: str):
    try:
        yield
    except StageError:
        raise
    except Exception as exc:  # noqa: BLE001
        raise StageError(name, exc) from exc


class _Parser(argparse.ArgumentParser):
    """argparse echoes an unknown argument; route it through the sink."""

    def _print_message(self, message, file=None):
        if message:
            emit(message.rstrip("\n"), err=file is sys.stderr)


def validate_legacy(rules: dict) -> None:
    """Refuse a present but malformed ``legacy_hashed_names`` block.

    Hashes kept under the salt they were made with, when the salt was rotated
    and their plaintext was not available to re-hash (C13/C16). An absent
    block is allowed. A present one -- an explicit null included -- must be a
    mapping with a salt and hashes: read as "no legacy names", it would
    silently match nothing.
    """
    if "legacy_hashed_names" not in rules:
        return
    legacy = rules["legacy_hashed_names"]
    if not (
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
        die(
            "check_identifiers: 'legacy_hashed_names' must be a mapping with a "
            "non-empty 'salt' and a non-empty list of sha256 'hashes' (omit the "
            "block entirely when there are none; an explicit null is refused)"
        )


def load_rules() -> dict:
    if yaml is None:  # pragma: no cover
        die("check_identifiers: PyYAML is required")
    if not os.path.isfile(RULES):
        die(
            f"check_identifiers: rules file missing: {os.path.basename(RULES)} "
            f"at the repository root"
        )
    try:
        with open(RULES, encoding="utf-8") as fh:
            rules = yaml.safe_load(fh)
    except Exception as exc:  # noqa: BLE001
        # The parser's message quotes the offending YAML: name its type only.
        die(f"check_identifiers: rules file unreadable ({type(exc).__name__})")
    if not isinstance(rules, dict) or "structural" not in rules:
        die("check_identifiers: rules file has no 'structural' section")
    validate_legacy(rules)

    private = os.environ.get("DIGITALMODEL_DENY_LIST")
    if private:
        if not os.path.isfile(private):
            # The path is not printed: it can carry an account or a client.
            die(
                "check_identifiers: the file DIGITALMODEL_DENY_LIST names does "
                "not exist. Refusing to continue without the rules it names."
            )
    elif os.path.isfile(DEFAULT_PRIVATE):
        # Named by nobody, so its absence is not an error -- CI has none and
        # relies on the public pattern classes. Present, it is always read.
        private = DEFAULT_PRIVATE
    names: list[str] = []
    patterns: list[re.Pattern] = []
    if private:
        with stage("reading private list"), open(private, encoding="utf-8-sig") as fh:
            for n, raw in enumerate(fh, start=1):
                ln = raw.strip()
                if not ln or ln.startswith("#"):
                    continue
                if ln.startswith("re:"):
                    try:
                        patterns.append(re.compile(ln[3:]))
                    except re.error:
                        # The pattern is private: name the line, not the text.
                        die(
                            f"check_identifiers: private list line {n} is "
                            f"not a valid regular expression. Refusing to "
                            f"continue without the rule it names.",
                            3,
                        )
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
                emit(
                    f"check_identifiers: baseline line {n} is not "
                    f"'<sha256>  <path>'. Refusing to continue.",
                    err=True,
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
        # git's message quotes paths in forms no filter parses reliably
        # (unquoted, embedded quotes, multi-line). Only the command and the
        # status are printed; the message only for a local --show-lines run.
        msg = f"check_identifiers: `git {args[0]}` failed (exit {out.returncode})"
        if _SINK.show:
            detail = out.stderr.decode(errors="replace").strip()
            if detail:
                msg += ": " + detail
        die(msg)
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
                die("check_identifiers: truncated `git cat-file` response")
            header = raw[pos:end].decode("ascii", "replace").split()
            pos = end + 1
            if len(header) != 3 or header[0] != oid or header[1] != "blob":
                die(f"check_identifiers: unexpected `git cat-file` response for {oid}")
            size = int(header[2])
            body = raw[pos : pos + size]
            if len(body) != size or raw[pos + size : pos + size + 1] != b"\n":
                die(f"check_identifiers: malformed `git cat-file` body for {oid}")
            contents[oid] = body
            pos += size + 1
        if pos != len(raw):
            die("check_identifiers: unconsumed `git cat-file` output")

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


def _member(name: str) -> str:
    """An office member named by a per-run keyed digest: its name can carry
    an identifier."""
    return "member " + _label_digest(name)


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
                        line, col = getattr(exc, "position", (0, 0))
                        raise Uninspectable(
                            f"office {_member(name)} is not well-formed XML "
                            f"(line {line}, column {col})"
                        ) from None
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
                raise Uninspectable(f"office {_member(name)} is not inspectable")
    except Uninspectable:
        raise
    except Exception as exc:  # noqa: BLE001
        # The message (a bad CRC, a missing member) can quote a member name:
        # report the kind of failure only.
        raise Uninspectable(
            f"office archive unreadable ({type(exc).__name__})"
        ) from None
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
    show_lines: bool = False,
    labels: dict[str, str] | None = None,
) -> tuple[list[str], int, list[str], list[str]]:
    """Returns findings, files scanned, media skipped, and uninspectable.

    ``digests``, when given, is filled for every uninspectable file with its
    repository-relative path -> (sha256 of the bytes that could not be read,
    or None when there were none, and the reason). ``labels``, when given,
    maps the same keys to the name to print: the path itself, or its digest
    when the path carries an identifier.

    ``show_lines`` quotes offending lines and paths. It is passed per call so
    that no earlier call can leave it on; callers refuse it in CI.
    """
    validate_legacy(rules)
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
        except re.error:
            # The error text quotes the pattern: name the rule only.
            die(f"check_identifiers: rule {rule.get('id')!r} has an invalid pattern")

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
                    f"the private list matches here" + _excerpt(line, show_lines)
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
                    f"{label}:{n}: [{rid}] {msg.strip()}" + _excerpt(line, show_lines)
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
                        f"deny list appears here" + _excerpt(line, show_lines)
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
        # Findings name the repository-relative path -- the text scanned
        # here -- never the argument as given, which can be absolute and
        # carry the account that ran the gate.
        disp = shown.replace("\\", "/")
        scan(disp, "path", disp)
        label = disp
        if len(findings) > before and not show_lines:
            # The path itself carries the identifier: name it by digest, for
            # every finding and every diagnostic about this file, so the log
            # does not publish it.
            label = path_label(disp)
            findings[before:] = [label + f[len(disp) :] for f in findings[before:]]
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
            uninspectable.append(f"{label}: {exc}")
            key = shown.replace("\\", "/")
            if labels is not None:
                labels[key] = label
            if digests is not None:
                digests[key] = (
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
#: for a local run and is refused in CI. The policy is passed per call, never
#: held in module state, so one call cannot leave it on for the next.


def _excerpt(line: str, show_lines: bool) -> str:
    return f"\n    {line.strip()[:160]}" if show_lines else ""


def main() -> int:
    """Run the gate. Every exception is reported by its type and the stage
    the gate was at, with exit status 4: an exception message or traceback
    can quote a denied file name or file content. ``--show-lines`` on a local
    run adds the traceback."""
    # A finding line can carry any character from the file it quotes. On a
    # console whose encoding cannot represent it (cp1252 on Windows) printing
    # raised mid-report and the report was lost.
    for stream in (sys.stdout, sys.stderr):
        try:
            stream.reconfigure(encoding="utf-8", errors="replace")
        except (AttributeError, ValueError):
            pass
    _SINK.reset()
    new_label_salt()
    try:
        return _run()
    except StageError as exc:
        failed, cause = exc.stage, exc.cause
    except Exception as exc:  # noqa: BLE001
        failed, cause = "running", exc
    finally:
        show = _SINK.show
        _SINK.reset()
    _SINK.show = show
    try:
        emit(
            f"check_identifiers: failed while {failed} "
            f"({type(cause).__name__}); the detail is not printed because it "
            f"can quote a file name or content (--show-lines shows it locally)",
            err=True,
        )
        if show:
            emit("".join(traceback.format_exception(cause)).rstrip(), err=True)
    finally:
        _SINK.reset()
    return EXIT_INTERNAL


def _run() -> int:
    # The rules come first so that every line after them -- argparse's own
    # usage errors, which echo the argument, included -- is redacted against
    # the full deny list.
    with stage("loading rules"):
        rules = load_rules()
    set_output_policy(rules)
    ap = _Parser(description="Client identifier gate")
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
    show_lines = bool(args.show_lines)
    if show_lines and (os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS")):
        emit("check_identifiers: --show-lines is refused in CI", err=True)
        return 3
    # Local only: quotes lines, shows paths, and turns redaction off.
    set_output_policy(rules, show=show_lines)

    if args.hash:
        emit(token_hash(args.hash, str(rules.get("salt", ""))))
        return 0

    staged = not args.paths and not args.all
    if args.update_baseline and (
        os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS")
    ):
        # An update accepts every uninspectable file it finds, unread. In CI
        # nobody reviews that, so a new binary would pass by its own digest.
        emit(
            "check_identifiers: --update-baseline refuses to run in CI (CI or "
            "GITHUB_ACTIONS is set). Regenerate the baseline locally, review "
            "the diff and commit it.",
            err=True,
        )
        return 3
    if args.update_baseline and staged:
        emit(
            "check_identifiers: --update-baseline needs --all or paths; the "
            "staged mode scans a partial set and would drop entries.",
            err=True,
        )
        return 3
    if args.baseline:
        baseline_path = os.path.abspath(args.baseline)
        if not args.update_baseline and not os.path.isfile(baseline_path):
            emit(
                "check_identifiers: the baseline named by --baseline does not "
                "exist. Refusing to continue without the list it names.",
                err=True,
            )
            return 3
    else:
        baseline_path = DEFAULT_BASELINE
    with stage("reading baseline"):
        baseline = read_baseline(baseline_path) if os.path.isfile(baseline_path) else {}
    if baseline is None:
        return 3

    with stage("listing files"):
        paths = args.paths or (tracked_files() if args.all else staged_files())
    if not paths:
        # Enumeration succeeded (a failure exits above) and found nothing.
        emit("check_identifiers: nothing to scan")
        return 0

    digests: dict[str, tuple[str | None, str]] = {}
    labels: dict[str, str] = {}
    with stage("scanning a file"):
        findings, scanned, media, uninspectable = check(
            paths,
            rules,
            staged=staged,
            digests=digests,
            show_lines=show_lines,
            labels=labels,
        )

    def shown(key: str) -> str:
        # Every line that names a file goes through here: a path carrying an
        # identifier is printed by digest unless --show-lines (local only).
        return key if show_lines else labels.get(key, path_label(key))

    emit(
        f"check_identifiers: scanned {scanned} file(s); "
        f"{len(media)} declared binary media not inspected; "
        f"{len(uninspectable)} uninspectable"
    )
    status = 0

    if args.update_baseline:
        unreadable = [k for k, (sha, _) in digests.items() if sha is None]
        if unreadable:
            emit(
                "check_identifiers: cannot baseline files whose bytes were not "
                "read: " + ", ".join(shown(k) for k in unreadable),
                err=True,
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
        with stage("writing baseline"):
            write_baseline(baseline_path, new)
        # A --baseline name is the caller's and unchecked: it is not echoed.
        named = (
            os.path.basename(baseline_path)
            if show_lines or baseline_path == DEFAULT_BASELINE
            else "named by --baseline"
        )
        emit(
            f"check_identifiers: baseline {named} now lists {len(new)} "
            f"uninspectable file(s)"
        )
        baseline = new

    failing: list[str] = []
    accepted = 0
    for key, (sha, reason) in sorted(digests.items()):
        if sha is None:
            failing.append(f"{shown(key)}: {reason}")
        elif key not in baseline:
            failing.append(f"{shown(key)}: {reason}; not in the baseline")
        elif baseline[key] != sha:
            failing.append(f"{shown(key)}: {reason}; changed since the baseline")
        else:
            accepted += 1
    if accepted:
        emit(
            f"check_identifiers: {accepted} uninspectable file(s) match the "
            f"baseline by path and sha256"
        )
    if args.all and baseline:
        stale = sorted(set(baseline) - set(digests))
        if stale:
            emit(
                f"check_identifiers: {len(stale)} baseline entr(y/ies) no longer "
                f"uninspectable; --update-baseline drops them"
            )
    if failing:
        status = 2
        emit("")
        emit(
            f"check_identifiers: {len(failing)} file(s) could not be "
            f"inspected and are not accepted — content that was not read has "
            f"not passed:"
        )
        for u in failing:
            emit(f"  {u}")
        emit(
            "  Convert to text, remove the file, declare its type in "
            "binary_media_extensions with a reason, or -- after reviewing the "
            "content -- accept it with --update-baseline."
        )
    if findings:
        status = 1
        emit("")
        emit(
            f"check_identifiers: {len(findings)} finding(s) — this repository "
            f"is PUBLIC"
        )
        for f in findings:
            emit(f"  {f}")
        emit("")
        emit(
            "  Replace the identifier with a neutral placeholder. If a finding "
            "is a false positive, add a justified entry to the exclusions in "
            ".legal-deny-list.yaml rather than widening a pattern."
        )
    if status == 0:
        emit("check_identifiers: no client identifier found")
    return status


if __name__ == "__main__":
    raise SystemExit(main())
