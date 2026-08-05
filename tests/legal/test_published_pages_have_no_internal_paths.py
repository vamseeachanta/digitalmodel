"""Published capability pages must not disclose absolute filesystem paths.

Context: digitalmodel#1965. Two published capability pages rendered a source
provenance table whose cells were absolute paths into an internal file share,
together with an organization identifier embedded in those paths. The producer
that emits those pages hardcoded the same paths, so correcting the pages alone
would let the next regeneration reprint them.

Why this detector is repo-local rather than a reuse of an existing gate:

* ``scripts/check_generated_html.py`` censuses ``docs/api/**`` only, and it does
  not regenerate pages registered in ``PAGE_EXCLUSIONS`` -- which is where the
  first page is registered. It therefore covers neither page for this defect.
* The second page lives under ``docs/domains/`` and is outside that census
  entirely.
* ``scripts/legal/legal-sanity-scan.sh`` does not exist in this repository, and
  the workspace-hub per-repo form is fail-open under workspace-hub#3804 -- it
  was observed reporting PASS over a tree that contained the leak.

Matching is on *path shape*, not on identifier strings. Two reasons: an
identifier assertion would require committing the identifier to the repository,
which is the thing being removed; and shape matching has no token boundaries to
get wrong. The root segment is constrained to the Filesystem Hierarchy Standard
top-level directory names, an externally-defined enumeration -- without that
constraint the pattern matches embedded JavaScript regex literals, minified CSS
and base64 image payloads, which was measured at 151 of the tree's HTML files.
"""
from __future__ import annotations

import functools
import html
import importlib.util
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
DOCS_ROOT = REPO_ROOT / "docs"

PRODUCER = (
    REPO_ROOT
    / "scripts/python/digitalmodel/ocimf/build_coefficient_explorer.py"
)

# The two pages remediated by #1965. Named explicitly so that the "zero
# violations" assertion below cannot be satisfied by deleting a page.
REMEDIATED_PAGES = (
    "docs/api/hydro/ocimf-coefficient-explorer.html",
    "docs/domains/charts/phase2/ocimf/ocimf_coefficient_explorer.html",
)

# Pages carrying absolute paths that are out of #1965's scope, each with the
# exact occurrence count and a stated reason. Following the convention of
# scripts/generated_html_ownership.py, no page is recorded without a reason.
#
# These are recorded rather than ignored. The counts are exact, so any listed
# page gaining an occurrence fails the ratchet test below, and any page not
# listed at all must be clean. Widening this change to remediate them would
# repeat the unreviewed scope-widening that produced the issue.
#
# Known limit, stated rather than papered over: the ratchet compares counts, so
# swapping one path for a different path inside a listed file would not trip it.
# Recording the matched strings instead would place those locations in the
# repository, which is the disclosure being removed. Counts are the weaker but
# non-self-defeating choice; closing this properly belongs with the
# authenticated scanner tracked by digitalmodel#1961.
_ORCAWAVE = "Windows developer-workspace path emitted by the OrcaWave benchmark reporter"
_ORCAFLEX = "Windows developer-workspace path emitted by the OrcaFlex model-library reporter"
_OPENFOAM = (
    "distribution-standard OpenFOAM install prefix, not a deployment-specific "
    "location; discloses only that a public package is installed"
)

RECORDED_ABSOLUTE_PATHS = {
    "docs/domains/orcaflex/examples/model_library_report.html": (2, _ORCAFLEX),
    "docs/domains/orcawave/L00_validation_wamit/2.1/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.2/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.3/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.5c/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.5f/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.6/benchmark/body_0/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.6/benchmark/body_1/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.7/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.8/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/2.9/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/3.1/benchmark/benchmark_report.html": (20, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/3.2/benchmark/benchmark_report.html": (10, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/3.3/benchmark/body_0/benchmark_report.html": (24, _ORCAWAVE),
    "docs/domains/orcawave/L00_validation_wamit/3.3/benchmark/body_1/benchmark_report.html": (24, _ORCAWAVE),
    "docs/reports/2026-08-04-issue-1959-solver-start-evidence.html": (1, _OPENFOAM),
}

RECORDED_COUNTS = {page: count for page, (count, _) in RECORDED_ABSOLUTE_PATHS.items()}

# Filesystem Hierarchy Standard top-level directories, plus the conventional
# macOS and Windows user roots. This is an enumeration of an external standard,
# not a value fitted to the leak being removed.
_FHS_ROOTS = (
    "bin", "boot", "dev", "etc", "home", "lib", "lib64", "media", "mnt",
    "opt", "proc", "root", "run", "sbin", "srv", "sys", "tmp", "usr", "var",
    "Users", "Volumes", "data",
)

# A leading slash, an FHS root, and at least one further segment. The lookbehind
# rejects a slash that is attached to a preceding word character, a colon or
# another slash, which is what keeps URL path segments (``https://host/usr/x``)
# and protocol-relative URLs out.
_POSIX_ABSOLUTE_PATH = re.compile(
    r"(?<![\w:/.-])/(?:" + "|".join(_FHS_ROOTS) + r")/[A-Za-z0-9_.-]+"
    r"(?:/[A-Za-z0-9_.-]+)*"
)

# A drive letter followed by at least two separators. Requiring two separators
# rather than one is what distinguishes ``D:\workspace-hub\repo`` from the
# two-character escape sequences (``a:\n``) that appear inside embedded scripts;
# with a single separator the pattern fired 575 times on escape sequences alone.
_WINDOWS_ABSOLUTE_PATH = re.compile(
    r"(?<![\w])[A-Za-z]:[\\/][A-Za-z0-9_.-]+[\\/][A-Za-z0-9_.-]+"
    r"(?:[\\/][A-Za-z0-9_.-]+)*"
)


# A UNC path names a file server and a share directly. It carries the same
# disclosure as a mounted absolute path and shares none of its syntax, so it
# needs its own pattern rather than a widened one.
_UNC_SHARE_PATH = re.compile(
    r"\\\\[A-Za-z0-9_.-]+\\[A-Za-z0-9_.-]+(?:\\[A-Za-z0-9_.-]+)*"
)

_PATTERNS = (_POSIX_ABSOLUTE_PATH, _WINDOWS_ABSOLUTE_PATH, _UNC_SHARE_PATH)


def find_absolute_paths(text: str) -> list[str]:
    """Return every absolute-filesystem-path occurrence in ``text``, in order."""
    found = [
        (m.start(), m.group(0))
        for pattern in _PATTERNS
        for m in pattern.finditer(text)
    ]
    return [value for _, value in sorted(found)]


@functools.lru_cache(maxsize=1)
def published_pages() -> tuple[str, ...]:
    """Every committed HTML page under ``docs/``, as repo-relative POSIX paths."""
    return tuple(
        sorted(p.relative_to(REPO_ROOT).as_posix() for p in DOCS_ROOT.rglob("*.html"))
    )


@functools.lru_cache(maxsize=1)
def _violations_by_page() -> dict[str, int]:
    counts = {}
    for relpath in published_pages():
        text = (REPO_ROOT / relpath).read_text(encoding="utf-8", errors="replace")
        hits = find_absolute_paths(text)
        if hits:
            counts[relpath] = len(hits)
    return counts


@functools.lru_cache(maxsize=1)
def _load_producer():
    spec = importlib.util.spec_from_file_location("_ocimf_producer", PRODUCER)
    module = importlib.util.module_from_spec(spec)
    # The producer defines a dataclass; dataclasses resolves annotations via
    # sys.modules[cls.__module__], so the module must be registered before it
    # is executed.
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


# --------------------------------------------------------------------------
# The detector must be capable of failing, and must not fire on non-paths.
# --------------------------------------------------------------------------

def test_detector_catches_a_planted_absolute_path():
    planted = "<td>report</td><td>/srv/example-share/project/data.xlsx</td>"
    assert find_absolute_paths(planted) == ["/srv/example-share/project/data.xlsx"]


def test_detector_catches_a_planted_windows_absolute_path():
    planted = r"<td>E:\build-area\project\run\input.yml</td>"
    assert find_absolute_paths(planted) == [r"E:\build-area\project\run\input.yml"]


def test_detector_catches_a_planted_unc_share_path():
    # A UNC path names a file server and a share directly, which is the same
    # disclosure this issue is about, so the detector must cover it.
    planted = r"<td>\\file-server\group-share\project\data.xlsx</td>"
    assert find_absolute_paths(planted) == [
        r"\\file-server\group-share\project\data.xlsx"
    ]


def test_detector_ignores_urls_with_path_segments():
    markup = (
        '<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>'
        '<a href="https://github.com/vamseeachanta/digitalmodel/issues/1965">x</a>'
        '<a href="https://example.invalid/usr/share/doc/readme.txt">y</a>'
    )
    assert find_absolute_paths(markup) == []


def test_detector_ignores_relative_paths():
    markup = "<code>wiki/standards/ocimf-meg3.md</code><code>src/digitalmodel/x.py</code>"
    assert find_absolute_paths(markup) == []


# --------------------------------------------------------------------------
# Anti-vacuity: a census that matches nothing would make the scan below pass
# forever.
# --------------------------------------------------------------------------

def test_page_census_is_not_empty():
    assert published_pages() != ()


def test_page_census_includes_both_remediated_pages():
    census = set(published_pages())
    assert census & set(REMEDIATED_PAGES) == set(REMEDIATED_PAGES)


# --------------------------------------------------------------------------
# The published surface.
# --------------------------------------------------------------------------

def test_published_html_outside_the_recorded_baseline_has_no_absolute_paths():
    offenders = {
        page: count
        for page, count in _violations_by_page().items()
        if page not in RECORDED_ABSOLUTE_PATHS
    }
    assert offenders == {}


def test_recorded_pre_existing_pages_do_not_gain_absolute_paths():
    recorded = {
        page: count
        for page, count in _violations_by_page().items()
        if page in RECORDED_ABSOLUTE_PATHS
    }
    assert recorded == RECORDED_COUNTS


# --------------------------------------------------------------------------
# The producer. Correcting only the pages leaves these paths reprintable, and
# the drift gate does not regenerate either page.
# --------------------------------------------------------------------------

def test_producer_source_has_no_absolute_filesystem_paths():
    source = PRODUCER.read_text(encoding="utf-8")
    assert find_absolute_paths(source) == []


def test_rendered_provenance_table_has_no_absolute_filesystem_paths():
    module = _load_producer()
    digests = {source.key: "0" * 64 for source in module.PROVENANCE_SOURCES}
    assert find_absolute_paths(module.render_provenance_html(digests)) == []


def test_provenance_rows_retain_title_and_digest():
    module = _load_producer()
    digests = {
        source.key: f"{index:064x}"
        for index, source in enumerate(module.PROVENANCE_SOURCES, start=1)
    }
    rendered = module.render_provenance_html(digests)
    rows = re.findall(
        r'<tr><td>(.*?)</td><td class="digest"><code>([0-9a-f]{64})</code></td>',
        rendered,
    )
    assert rows == [
        (html.escape(source.title), digests[source.key])
        for source in module.PROVENANCE_SOURCES
    ]


def test_producer_exposes_no_absolute_path_defaults():
    module = _load_producer()
    defaults = [
        str(value)
        for name, value in vars(module).items()
        if name.isupper() and isinstance(value, (str, Path))
    ]
    assert find_absolute_paths("\n".join(defaults)) == []
