#!/usr/bin/env python
"""Sanitize and organize OrcaFlex models from a private s7 model archive into
the digitalmodel library.

Walks the s7 directory, deduplicates by content hash, applies client-name
sanitization, strips metadata headers, and writes sanitized .yml files into
an organized category-based folder structure under docs/domains/orcaflex/.

The name map is private
-----------------------
The real-to-neutral name map is the de-identification key, so it is not in
this public file (owner decisions C13/C16). It is read at run time from a
private JSON file:

    1. ``--map PATH``, else
    2. the path in ``DIGITALMODEL_S7_SANITIZE_MAP``, else
    3. ``~/.config/digitalmodel/s7-sanitize-map.json``.

When none is found, or the configured file is missing or malformed, the
script stops before reading or writing any model: running with an empty map
would write unsanitized models. The file holds::

    {
      "default_s7_root": "<path to the s7 archive, optional>",
      "sanitization_map": {"<real name>": "<neutral name>", ...},
      "category_map": {"<source folder prefix>": "<target category>", ...},
      "exclusions": ["<source folder prefix to skip>", ...]
    }

The audit is private too
------------------------
The audit records every replacement as original -> replacement and every
source path, which is the name map again. It is written to private storage:

    1. ``--audit PATH``, else
    2. the path in ``DIGITALMODEL_S7_SANITIZE_AUDIT``, else
    3. ``s7-sanitize-audit.json`` next to the private name map.

A location inside this repository or inside the output tree is refused before
any model is read. Public output carries counts only.

The console is public output
----------------------------
The redacting log filter is installed before the arguments are parsed.
argparse's usage and error text is written through the same redactor, with
every argument the caller typed redacted as a phrase, so an unknown option or
a stray path is never echoed. Any exception is reported by its type and the
stage the run was at, with exit status 4 and no traceback; ``--debug-traceback``
adds the traceback on a local run and is refused in CI.

Known limits
------------
This is a local-only tool, and its redaction is a heuristic.

* Log filtering depends on the logger and handler topology. The filter sits
  on this module's ``sanitize_s7`` logger and on the root handlers present
  when the CLI starts. A library logger with its own handler, a handler added
  later, a logger that does not propagate, or a child logger (a parent's
  filter does not run for a child's records) can emit a record the filter
  never sees.
* The identifier gate's redactor widens a public pattern match to the
  whitespace-delimited token around it, so a path containing a space keeps
  the part after the space visible unless the private map names it.

Public CI does not run this script and has no private map; the identifier
gate, which fails closed on any finding, is the control for committed output.

Usage:
    uv run python scripts/sanitize_s7_models.py
    uv run python scripts/sanitize_s7_models.py --dry-run
    uv run python scripts/sanitize_s7_models.py --skip-dat
    uv run python scripts/sanitize_s7_models.py --map /private/s7-map.json \
        --s7-root /path/to/s7 --output-root /path/to/out
"""
from __future__ import annotations

import argparse
import hashlib
import json
import logging
import os
import re
import secrets
import sys
import time
from dataclasses import asdict, dataclass, field
from pathlib import Path

try:
    import OrcFxAPI

    HAS_ORCFX = True
except ImportError:
    HAS_ORCFX = False

# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

DEFAULT_OUTPUT_ROOT = Path("docs/domains/orcaflex")

MAP_ENV_VAR = "DIGITALMODEL_S7_SANITIZE_MAP"
MAP_FILE_NAME = "s7-sanitize-map.json"
AUDIT_ENV_VAR = "DIGITALMODEL_S7_SANITIZE_AUDIT"
AUDIT_FILE_NAME = "s7-sanitize-audit.json"
#: The repository this script belongs to: nothing identifying is written here.
REPO_ROOT = Path(__file__).resolve().parent.parent


def default_map_path() -> Path:
    """``~/.config/digitalmodel/s7-sanitize-map.json`` for the current user."""
    return Path.home() / ".config" / "digitalmodel" / MAP_FILE_NAME


class SanitizeMapError(RuntimeError):
    """The private name map is missing or malformed; the run must stop."""


@dataclass(frozen=True)
class SanitizeConfig:
    """The private name map, validated, with lookup orders precomputed."""

    sanitization_map: dict[str, str]
    category_map: dict[str, str]
    exclusions: tuple[str, ...]
    default_s7_root: str = ""
    source: str = ""

    @property
    def sorted_sanitization_pairs(self) -> list[tuple[str, str]]:
        # Longest key first, to avoid partial-match corruption.
        return sorted(
            self.sanitization_map.items(), key=lambda kv: len(kv[0]), reverse=True
        )

    @property
    def sorted_category_pairs(self) -> list[tuple[str, str]]:
        # Longest prefix first, so the most specific category wins.
        return sorted(
            self.category_map.items(), key=lambda kv: len(kv[0]), reverse=True
        )


def _str_map(data: dict, key: str, required: bool) -> dict[str, str]:
    # Messages name the key, never the file: its path can carry an account
    # or a client, and the log is public output.
    value = data.get(key, None)
    if value is None:
        if required:
            raise SanitizeMapError(f"the name map: '{key}' is missing")
        return {}
    if not isinstance(value, dict) or not all(
        isinstance(k, str) and isinstance(v, str) for k, v in value.items()
    ):
        raise SanitizeMapError(f"the name map: '{key}' must map strings to strings")
    if required and not value:
        raise SanitizeMapError(f"the name map: '{key}' is empty")
    return dict(value)


def load_sanitize_config(path: str | Path | None = None) -> SanitizeConfig:
    """Load the private name map; raise :class:`SanitizeMapError` if absent.

    Resolution: *path*, else ``$DIGITALMODEL_S7_SANITIZE_MAP``, else
    ``~/.config/digitalmodel/s7-sanitize-map.json``. There is no built-in
    fallback map.
    """
    if path is not None:
        resolved, origin = Path(path), "--map"
    elif os.environ.get(MAP_ENV_VAR):
        resolved, origin = Path(os.environ[MAP_ENV_VAR]), MAP_ENV_VAR
    else:
        resolved, origin = default_map_path(), "the default location"

    # No message names the file or quotes its content: the path can carry an
    # account or a client, and a JSON error quotes the text it failed on.
    if not resolved.is_file():
        raise SanitizeMapError(
            f"private s7 name map not found (from {origin}); set {MAP_ENV_VAR} "
            f"or install ~/.config/digitalmodel/{MAP_FILE_NAME}. Refusing to "
            "run without it."
        )
    try:
        data = json.loads(resolved.read_text(encoding="utf-8"))
    except (OSError, ValueError) as exc:
        raise SanitizeMapError(
            f"cannot read the name map (from {origin}): {type(exc).__name__}"
        ) from None
    if not isinstance(data, dict):
        raise SanitizeMapError("the name map must be a JSON object")

    sanitization = _str_map(data, "sanitization_map", required=True)
    categories = _str_map(data, "category_map", required=False)
    exclusions = data.get("exclusions", [])
    if not isinstance(exclusions, list) or not all(
        isinstance(e, str) for e in exclusions
    ):
        raise SanitizeMapError("the name map: 'exclusions' must be a list of strings")
    root = data.get("default_s7_root", "") or ""
    if not isinstance(root, str):
        raise SanitizeMapError("the name map: 'default_s7_root' must be a string")

    return SanitizeConfig(
        sanitization_map=sanitization,
        category_map=categories,
        exclusions=tuple(exclusions),
        default_s7_root=root,
        source=str(resolved),
    )

# YAML metadata lines that OrcFxAPI.SaveData() embeds at file top.
_HEADER_LINE_RE = re.compile(r"^(User|Machine|File)\s*:.*$", re.MULTILINE)

# Heuristic: OrcaFlex YAML files contain a `---` separator or known
# section names.
_ORCAFLEX_SECTION_NAMES = {
    "General",
    "Environment",
    "LineTypes",
    "Lines",
    "Vessels",
    "VesselTypes",
    "3DBuoys",
    "6DBuoys",
    "Links",
    "Shapes",
    "Constraints",
    "WaveTrains",
    "DiffractionData",
    "ClumpTypes",
    "Groups",
    "Winches",
}

logger = logging.getLogger("sanitize_s7")


# ---------------------------------------------------------------------------
# Log redaction
# ---------------------------------------------------------------------------
#
# The log is public output (a terminal, a CI log, a pasted transcript). Every
# record passes RedactingFilter, which runs the formatted message through the
# identifier gate's redactor (scripts/legal/check_identifiers.py): the gate's
# deny list and public pattern classes, plus every name and source folder of
# the private map. Identifying diagnostics are written to the private audit;
# the log carries counts and opaque ids.

_GATE = REPO_ROOT / "scripts" / "legal" / "check_identifiers.py"
#: Per-run salt for opaque ids, so an id cannot be dictionary-matched to a path.
_RUN_SALT = secrets.token_hex(8)


def _gate_module():
    """The identifier gate, loaded from this repository, or None."""
    import importlib.util

    name = "_sanitize_s7_identifier_gate"
    if name in sys.modules:
        return sys.modules[name]
    try:
        spec = importlib.util.spec_from_file_location(name, _GATE)
        mod = importlib.util.module_from_spec(spec)
        sys.modules[name] = mod
        spec.loader.exec_module(mod)
    except Exception:  # noqa: BLE001
        sys.modules.pop(name, None)
        return None
    return mod


def _literal_redactor(names):
    """Fallback when the gate cannot be loaded: replace every whole log line
    that holds a name, since the public pattern classes are unavailable."""
    low_names = [n.lower() for n in names if len(n) >= 4]

    def redact(text: str) -> str:
        if any(n in text.lower() for n in low_names):
            return "<redacted log line>"
        return text

    return redact


def build_redactor(config: SanitizeConfig | None = None, extra=()):
    """A text -> text redactor for the private map *config* and *extra* text."""
    names: list[str] = [str(x) for x in extra if x]
    if config is not None:
        names.extend(config.sanitization_map)
        for key in list(config.category_map) + list(config.exclusions):
            names.append(key.replace("\\", "/").split("/")[0])
        if config.default_s7_root:
            names.append(config.default_s7_root)
    gate = _gate_module()
    if gate is None:
        return _literal_redactor(names)
    try:
        rules = gate.load_rules()
    except BaseException:  # noqa: BLE001 -- the gate exits on a bad rules file
        rules = None
    return gate.Redactor(rules, names=names).redact


class RedactingFilter(logging.Filter):
    """Redacts every record's formatted message; drops tracebacks."""

    def __init__(self) -> None:
        super().__init__()
        self.redact = build_redactor()

    def filter(self, record: logging.LogRecord) -> bool:
        try:
            message = record.getMessage()
        except Exception:  # noqa: BLE001
            message = str(record.msg)
        record.msg = self.redact(message)
        record.args = None
        # A traceback quotes paths and exception text verbatim.
        record.exc_info = None
        record.exc_text = None
        record.stack_info = None
        return True


_FILTER = RedactingFilter()
logger.addFilter(_FILTER)


def install_log_redaction(
    config: SanitizeConfig | None, handlers=None, extra=()
) -> RedactingFilter:
    """Redact against *config* and *extra* on this module's logger and on
    *handlers* (for the root handler the CLI configures)."""
    _FILTER.redact = build_redactor(config, extra)
    if _FILTER not in logger.filters:
        logger.addFilter(_FILTER)
    for handler in handlers or ():
        if _FILTER not in handler.filters:
            handler.addFilter(_FILTER)
    return _FILTER


def opaque_id(path: Path | str) -> str:
    """An id for a source path, matched to it only in the private audit."""
    digest = hashlib.sha256(f"{_RUN_SALT}:{path}".encode("utf-8")).hexdigest()
    return "src-" + digest[:10]


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------

@dataclass
class AuditEntry:
    source_path: str
    target_path: str
    sha256: str
    file_size_bytes: int
    transformations: list[str] = field(default_factory=list)
    category: str = ""
    status: str = "ok"
    error: str = ""


@dataclass
class RunStats:
    total_files_found: int = 0
    yml_processed: int = 0
    dat_processed: int = 0
    dupes_skipped: int = 0
    excluded_skipped: int = 0
    non_orcaflex_skipped: int = 0
    errors: int = 0
    categories_used: set[str] = field(default_factory=set)
    start_time: float = 0.0
    end_time: float = 0.0


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def sha256_of_file(path: Path) -> str:
    """Return hex digest of file contents."""
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 16), b""):
            h.update(chunk)
    return h.hexdigest()


def is_excluded(rel_path: str, config: SanitizeConfig) -> bool:
    """Check whether *rel_path* (forward-slash, relative to s7-root) falls
    under any excluded directory prefix."""
    rel_lower = rel_path.lower()
    for exc in config.exclusions:
        exc_lower = exc.lower().replace("\\", "/")
        if rel_lower.startswith(exc_lower + "/") or rel_lower == exc_lower:
            return True
    return False


def resolve_category(rel_path: str, config: SanitizeConfig) -> str | None:
    """Return the target category for a file's relative directory, or None
    if no mapping matches."""
    rel_norm = rel_path.replace("\\", "/")
    for prefix, category in config.sorted_category_pairs:
        prefix_norm = prefix.replace("\\", "/")
        if rel_norm.startswith(prefix_norm + "/") or rel_norm == prefix_norm:
            return category
    return None


def looks_like_orcaflex_yml(path: Path) -> bool:
    """Heuristic check: does this .yml file appear to be an OrcaFlex model?"""
    try:
        with open(path, "r", encoding="latin-1", errors="replace") as f:
            head = f.read(8192)
    except OSError:
        return False

    if "---" in head:
        return True

    for section_name in _ORCAFLEX_SECTION_NAMES:
        if re.search(rf"^{section_name}\s*:", head, re.MULTILINE):
            return True

    return False


def sanitize_text(text: str, config: SanitizeConfig) -> tuple[str, list[str]]:
    """Apply all sanitization replacements and strip metadata headers.

    Returns (sanitized_text, list_of_transformation_descriptions).
    """
    transformations: list[str] = []

    # Apply replacements longest-first.
    for old, new in config.sorted_sanitization_pairs:
        if old in text:
            count = text.count(old)
            if new:
                text = text.replace(old, new)
                transformations.append(f"replaced '{old}' -> '{new}' ({count}x)")
            else:
                text = text.replace(old, "")
                transformations.append(f"removed '{old}' ({count}x)")

    # Strip header metadata lines.
    stripped, n = _HEADER_LINE_RE.subn("", text)
    if n:
        transformations.append(f"removed {n} metadata header line(s)")
        text = stripped

    # Collapse any resulting blank-line runs at the very start of file.
    text = text.lstrip("\n")

    return text, transformations


def sanitize_object_names(
    model: "OrcFxAPI.Model", config: SanitizeConfig
) -> list[str]:
    """Rename objects inside a loaded OrcFxAPI model using the sanitization
    map.  Returns list of transformation descriptions."""
    transformations: list[str] = []

    for obj in model.objects:
        original_name = obj.Name
        new_name = original_name
        for old, new in config.sorted_sanitization_pairs:
            if old in new_name:
                new_name = new_name.replace(old, new)
        if new_name != original_name:
            obj.Name = new_name
            transformations.append(
                f"object renamed: '{original_name}' -> '{new_name}'"
            )

    return transformations


def target_yml_path(
    output_root: Path, category: str, source_name: str, config: SanitizeConfig
) -> Path:
    """Build the target path:
    <output_root>/<category>/monolithic/<sanitized_filename>.yml
    """
    # Sanitize the filename itself.
    sanitized_name = source_name
    for old, new in config.sorted_sanitization_pairs:
        if old in sanitized_name:
            sanitized_name = sanitized_name.replace(old, new)

    # Ensure .yml extension.
    stem = Path(sanitized_name).stem
    return output_root / category / "monolithic" / f"{stem}.yml"


# ---------------------------------------------------------------------------
# Processing
# ---------------------------------------------------------------------------

def process_yml_file(
    source: Path,
    output_root: Path,
    category: str,
    dry_run: bool,
    config: SanitizeConfig,
) -> AuditEntry:
    """Text-based sanitization of a .yml file."""
    file_hash = sha256_of_file(source)
    file_size = source.stat().st_size
    target = target_yml_path(output_root, category, source.name, config)

    entry = AuditEntry(
        source_path=str(source),
        target_path=str(target),
        sha256=file_hash,
        file_size_bytes=file_size,
        category=category,
    )

    try:
        with open(source, "r", encoding="latin-1", errors="replace") as f:
            raw_text = f.read()

        sanitized, transforms = sanitize_text(raw_text, config)
        entry.transformations = transforms

        if not dry_run:
            target.parent.mkdir(parents=True, exist_ok=True)
            with open(target, "w", encoding="latin-1", newline="") as f:
                f.write(sanitized)

        entry.status = "ok"
    except Exception as exc:
        entry.status = "error"
        entry.error = str(exc)  # private: the audit keeps the detail
        logger.error(
            "Error processing source %s (%s); the detail is in the private audit",
            opaque_id(source),
            type(exc).__name__,
        )

    return entry


def process_dat_file(
    source: Path,
    output_root: Path,
    category: str,
    dry_run: bool,
    config: SanitizeConfig,
) -> AuditEntry:
    """Load .dat via OrcFxAPI, sanitize object names, save as .yml, then
    apply text-based sanitization on the result."""
    file_hash = sha256_of_file(source)
    file_size = source.stat().st_size
    target = target_yml_path(output_root, category, source.name, config)

    entry = AuditEntry(
        source_path=str(source),
        target_path=str(target),
        sha256=file_hash,
        file_size_bytes=file_size,
        category=category,
    )

    if not HAS_ORCFX:
        entry.status = "skipped"
        entry.error = "OrcFxAPI not installed"
        return entry

    try:
        model = OrcFxAPI.Model(str(source))
        obj_transforms = sanitize_object_names(model, config)
        entry.transformations.extend(obj_transforms)

        if not dry_run:
            target.parent.mkdir(parents=True, exist_ok=True)
            model.SaveData(str(target))

            # Second pass: text-based sanitization on the saved .yml to
            # catch embedded strings that object-rename did not cover.
            with open(target, "r", encoding="latin-1", errors="replace") as f:
                raw_text = f.read()

            sanitized, text_transforms = sanitize_text(raw_text, config)
            entry.transformations.extend(text_transforms)

            with open(target, "w", encoding="latin-1", newline="") as f:
                f.write(sanitized)

        entry.status = "ok"
    except Exception as exc:
        entry.status = "error"
        entry.error = str(exc)  # private: the audit keeps the detail
        logger.error(
            "Error processing source %s (%s); the detail is in the private audit",
            opaque_id(source),
            type(exc).__name__,
        )

    return entry


# ---------------------------------------------------------------------------
# Discovery
# ---------------------------------------------------------------------------

def discover_model_files(
    s7_root: Path, config: SanitizeConfig
) -> tuple[list[Path], list[Path], int]:
    """Walk s7_root and return (yml_files, dat_files, excluded_count).

    Applies exclusion filtering and the OrcaFlex-heuristic check for .yml
    files.
    """
    yml_files: list[Path] = []
    dat_files: list[Path] = []
    excluded_count = 0

    for path in sorted(s7_root.rglob("*")):
        if not path.is_file():
            continue
        suffix = path.suffix.lower()
        if suffix not in (".dat", ".yml"):
            continue

        rel = path.relative_to(s7_root).as_posix()

        if is_excluded(rel, config):
            excluded_count += 1
            continue

        if suffix == ".yml":
            if not looks_like_orcaflex_yml(path):
                excluded_count += 1
                continue
            yml_files.append(path)
        else:
            dat_files.append(path)

    return yml_files, dat_files, excluded_count


# ---------------------------------------------------------------------------
# Audit location
# ---------------------------------------------------------------------------


class AuditPathError(RuntimeError):
    """The audit location is public; the run must stop."""


def _inside(path: Path, root: Path) -> bool:
    try:
        path.relative_to(root)
    except ValueError:
        return False
    return True


def resolve_audit_path(
    explicit: str | Path | None, config: SanitizeConfig, output_root: Path
) -> Path:
    """The private audit location, or :class:`AuditPathError`.

    Resolution: *explicit*, else ``$DIGITALMODEL_S7_SANITIZE_AUDIT``, else
    ``s7-sanitize-audit.json`` next to the private name map. The audit holds
    original names and source paths, so a location inside this repository or
    inside the (public) output tree is refused.
    """
    if explicit:
        audit, origin = Path(explicit), "--audit"
    elif os.environ.get(AUDIT_ENV_VAR):
        audit, origin = Path(os.environ[AUDIT_ENV_VAR]), AUDIT_ENV_VAR
    else:
        audit, origin = Path(config.source).parent / AUDIT_FILE_NAME, "the map folder"
    audit = audit.resolve()
    for root, what in ((REPO_ROOT, "the repository"), (output_root, "the output tree")):
        if _inside(audit, root.resolve()):
            raise AuditPathError(
                f"the audit location (from {origin}) is inside {what}; the audit "
                f"holds original names and source paths and must stay private. "
                f"Set --audit or {AUDIT_ENV_VAR} to a private location."
            )
    return audit


# ---------------------------------------------------------------------------
# Main orchestration
# ---------------------------------------------------------------------------

#: Exit status for an exception the run did not anticipate.
EXIT_INTERNAL = 4
#: The stage the run is at, named in the report of an unexpected exception.
_STAGE = ["starting"]


def _at(stage: str) -> None:
    _STAGE[0] = stage


def run(args: argparse.Namespace) -> int:
    """Main entry point.  Returns exit code (0 = success).

    Stops with exit code 2, before reading or writing any model, when the
    private name map cannot be loaded.
    """
    _at("loading the name map")
    try:
        config = load_sanitize_config(getattr(args, "map", None))
    except SanitizeMapError as exc:
        # The message names the key or the origin, never the path.
        logger.error("%s", exc)
        return 2
    install_log_redaction(
        config, extra=[x for x in (args.s7_root, config.default_s7_root) if x]
    )
    logger.info("Name map loaded (%d entries)", len(config.sanitization_map))

    s7_arg = args.s7_root or config.default_s7_root
    if not s7_arg:
        logger.error("No --s7-root given and the name map sets no default_s7_root")
        return 2
    s7_root = Path(s7_arg).resolve()
    install_log_redaction(config, extra=[s7_arg, str(s7_root)])
    _at("resolving the output and audit locations")
    output_root = Path(args.output_root).resolve()
    dry_run: bool = args.dry_run
    skip_dat: bool = args.skip_dat
    try:
        audit_path = resolve_audit_path(getattr(args, "audit", None), config, output_root)
    except AuditPathError as exc:
        logger.error("%s", exc)
        return 2

    if not s7_root.is_dir():
        logger.error("The s7 root directory does not exist (path not shown)")
        return 1

    stats = RunStats(start_time=time.time())
    audit_log: list[AuditEntry] = []
    seen_hashes: set[str] = set()
    # Identifying diagnostics: written to the private audit, never logged.
    diagnostics: list[dict[str, str]] = [{"kind": "source_root", "path": str(s7_root)}]

    def note(kind: str, path: Path | str) -> str:
        ident = opaque_id(path)
        diagnostics.append({"kind": kind, "id": ident, "path": str(path)})
        return ident

    _at("discovering model files")
    logger.info("Discovering model files (source root in the private audit) ...")
    yml_files, dat_files, excluded_count = discover_model_files(s7_root, config)
    stats.excluded_skipped = excluded_count
    stats.total_files_found = len(yml_files) + len(dat_files)

    logger.info(
        "Found %d .yml and %d .dat files (%d excluded)",
        len(yml_files),
        len(dat_files),
        excluded_count,
    )

    # --- Phase 1: .yml files (text-based sanitization) ---
    logger.info("--- Phase 1: Sanitizing .yml files ---")
    for source in yml_files:
        _at("hashing a source")
        file_hash = sha256_of_file(source)
        if file_hash in seen_hashes:
            stats.dupes_skipped += 1
            logger.debug("Duplicate skipped: source %s", note("duplicate", source))
            continue
        seen_hashes.add(file_hash)

        rel_dir = source.parent.relative_to(s7_root).as_posix()
        category = resolve_category(rel_dir, config)
        if category is None:
            category = "uncategorized"
            logger.warning(
                "No category mapping for source folder %s", note("unmapped", rel_dir)
            )

        stats.categories_used.add(category)
        _at("processing a source")
        entry = process_yml_file(source, output_root, category, dry_run, config)
        audit_log.append(entry)

        if entry.status == "error":
            stats.errors += 1
        else:
            stats.yml_processed += 1

    # --- Phase 2: .dat files (OrcFxAPI conversion + text sanitization) ---
    if skip_dat:
        logger.info("--- Phase 2: Skipped (.dat conversion disabled) ---")
    elif not HAS_ORCFX:
        logger.warning(
            "--- Phase 2: Skipped (OrcFxAPI not installed) ---"
        )
    else:
        logger.info("--- Phase 2: Converting .dat files via OrcFxAPI ---")
        for source in dat_files:
            _at("hashing a source")
            file_hash = sha256_of_file(source)
            if file_hash in seen_hashes:
                stats.dupes_skipped += 1
                logger.debug("Duplicate skipped: source %s", note("duplicate", source))
                continue
            seen_hashes.add(file_hash)

            rel_dir = source.parent.relative_to(s7_root).as_posix()
            category = resolve_category(rel_dir, config)
            if category is None:
                category = "uncategorized"
                logger.warning(
                    "No category mapping for source folder %s",
                    note("unmapped", rel_dir),
                )

            stats.categories_used.add(category)
            _at("processing a source")
            entry = process_dat_file(source, output_root, category, dry_run, config)
            audit_log.append(entry)

            if entry.status == "error":
                stats.errors += 1
            elif entry.status == "skipped":
                pass
            else:
                stats.dat_processed += 1

    stats.end_time = time.time()

    # --- Write audit log (private: it names originals and sources) ---
    audit_data = {
        "summary": {
            "total_files_found": stats.total_files_found,
            "yml_processed": stats.yml_processed,
            "dat_processed": stats.dat_processed,
            "dupes_skipped": stats.dupes_skipped,
            "excluded_skipped": stats.excluded_skipped,
            "non_orcaflex_skipped": stats.non_orcaflex_skipped,
            "errors": stats.errors,
            "categories_used": sorted(stats.categories_used),
            "elapsed_seconds": round(stats.end_time - stats.start_time, 2),
            "dry_run": dry_run,
            "orcfx_available": HAS_ORCFX,
        },
        "entries": [asdict(e) for e in audit_log],
        "diagnostics": diagnostics,
    }

    _at("writing the private audit")
    if not dry_run:
        audit_path.parent.mkdir(parents=True, exist_ok=True)
        with open(audit_path, "w", encoding="utf-8") as f:
            json.dump(audit_data, f, indent=2)
        logger.info("Private audit log written (%d entries)", len(audit_log))
    else:
        logger.info("DRY RUN: private audit log not written")

    # --- Print summary ---
    _at("printing the summary")
    _print_summary(stats, dry_run)

    return 0 if stats.errors == 0 else 1


def _print_summary(stats: RunStats, dry_run: bool) -> None:
    elapsed = stats.end_time - stats.start_time
    prefix = "[DRY RUN] " if dry_run else ""
    print(f"\n{'=' * 60}")
    print(f"{prefix}Sanitization Summary")
    print(f"{'=' * 60}")
    print(f"  Total model files found : {stats.total_files_found}")
    print(f"  .yml files processed    : {stats.yml_processed}")
    print(f"  .dat files processed    : {stats.dat_processed}")
    print(f"  Duplicates skipped      : {stats.dupes_skipped}")
    print(f"  Excluded dirs skipped   : {stats.excluded_skipped}")
    print(f"  Non-OrcaFlex .yml skip  : {stats.non_orcaflex_skipped}")
    print(f"  Errors                  : {stats.errors}")
    print(f"  Categories used         : {len(stats.categories_used)}")
    print(f"  Elapsed time            : {elapsed:.1f}s")
    print(f"  OrcFxAPI available      : {HAS_ORCFX}")
    print(f"{'=' * 60}\n")


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def _in_ci() -> bool:
    return bool(os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"))


def _cli_write(message: str, *, err: bool = True, redact=None) -> None:
    """Write one console message through the redactor (the log filter's
    unless *redact* is given): the console is public output."""
    text = (redact or _FILTER.redact)(str(message))
    stream = sys.stderr if err else sys.stdout
    stream.write(text + "\n")
    stream.flush()


class _Parser(argparse.ArgumentParser):
    """argparse echoes an unknown option or a stray argument in its error;
    every message it writes goes through the redactor instead, with each
    argument the caller typed (other than a known option name) redacted."""

    redact = None

    def _print_message(self, message, file=None):
        if message:
            _cli_write(
                message.rstrip("\n"), err=file is not sys.stdout, redact=self.redact
            )


def _typed_arguments(parser: argparse.ArgumentParser, argv: list[str]) -> list[str]:
    """Every argument in *argv* that is not a known option name, and the value
    of a known ``--option=value``: the text argparse may echo."""
    known = set(parser._option_string_actions)
    typed: list[str] = []
    for token in argv:
        if token in known:
            continue
        head, eq, value = token.partition("=")
        if eq and head in known:
            typed.append(value)
        else:
            typed.append(token)
    return [t for t in typed if t.strip()]


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    argv = sys.argv[1:] if argv is None else list(argv)
    parser = _Parser(
        description="Sanitize and organize OrcaFlex models from s7/ into "
        "the digitalmodel library.",
    )
    parser.add_argument(
        "--s7-root",
        type=str,
        default=None,
        help="Path to the s7 model archive (default: default_s7_root in the "
        "private name map)",
    )
    parser.add_argument(
        "--map",
        type=str,
        default=None,
        help=f"Private name map JSON (default: ${MAP_ENV_VAR}, else "
        f"~/.config/digitalmodel/{MAP_FILE_NAME})",
    )
    parser.add_argument(
        "--audit",
        type=str,
        default=None,
        help=f"Private audit JSON (default: ${AUDIT_ENV_VAR}, else "
        f"{AUDIT_FILE_NAME} next to the name map). Refused inside the "
        "repository or the output tree.",
    )
    parser.add_argument(
        "--output-root",
        type=str,
        default=str(DEFAULT_OUTPUT_ROOT),
        help="Output directory for sanitized models "
        f"(default: {DEFAULT_OUTPUT_ROOT})",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Discover and log transformations without writing files",
    )
    parser.add_argument(
        "--skip-dat",
        action="store_true",
        help="Skip .dat file conversion (useful without OrcFxAPI)",
    )
    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Enable debug logging",
    )
    parser.add_argument(
        "--debug-traceback",
        action="store_true",
        help="On an unexpected error, print the full traceback (local runs "
        "only: it can quote names and paths; refused in CI)",
    )
    parser.redact = build_redactor(None, extra=_typed_arguments(parser, argv))
    return parser.parse_args(argv)


def main(argv: list[str] | None = None) -> int:
    """The CLI. Every exception is reported by its type and the stage the run
    was at, with exit status 4: an exception message or a traceback can quote
    a source path or a name. ``--debug-traceback`` adds the traceback on a
    local run and is refused in CI."""
    for stream in (sys.stdout, sys.stderr):
        try:
            stream.reconfigure(encoding="utf-8", errors="replace")
        except (AttributeError, ValueError):
            pass
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)-8s %(message)s",
        datefmt="%H:%M:%S",
    )
    # Before the arguments are parsed: the root handler redacts too, since a
    # record from any other logger (OrcFxAPI, a library) reaches the console
    # only through it.
    install_log_redaction(None, handlers=logging.getLogger().handlers)
    _at("parsing arguments")
    args = parse_args(argv)
    debug = bool(args.debug_traceback)
    if debug and _in_ci():
        _cli_write(
            "sanitize_s7_models: --debug-traceback is refused in CI (CI or "
            "GITHUB_ACTIONS is set): a traceback can quote names and paths"
        )
        return 2
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    _at("starting")
    try:
        return run(args)
    except Exception as exc:  # noqa: BLE001 -- the boundary of the CLI
        _cli_write(
            f"sanitize_s7_models: failed while {_STAGE[0]} "
            f"({type(exc).__name__}); the detail is not printed because it can "
            "quote a source path or a name (--debug-traceback shows it locally)"
        )
        if debug:
            import traceback

            sys.stderr.write("".join(traceback.format_exception(exc)))
        return EXIT_INTERNAL


if __name__ == "__main__":
    sys.exit(main())
