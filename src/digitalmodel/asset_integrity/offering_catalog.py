# ABOUTME: Loader, validator, query helpers and Markdown renderer for the FFS offering catalog
# ABOUTME: (ffs_offering_catalog.yml, ffs_design_screen_catalog.yml, ffs_damage_mechanism_crosswalk.yml). #2197
"""FFS offering catalog: one loader for the three YAML data files under ``asset_integrity/data``.

* ``ffs_offering_catalog.yml``  -- industries x assets x damage mechanisms x codes x engine status
* ``ffs_design_screen_catalog.yml`` -- registered design-screen workflows that are not FFS verdicts (owner decision D5)
* ``ffs_damage_mechanism_crosswalk.yml`` -- API RP 571 mechanism names -> API 579 part numbers

The validator here is the single home of the schema rules; ``tests/asset_integrity/test_offering_catalog.py``
calls it one rule at a time. ``render_markdown`` regenerates
``docs/domains/asset-integrity/ffs-offering-catalog.md`` and ``render_capability_map`` regenerates the
``engines.ffs`` block of ``docs/capability-map/capabilities-added.yml``; the render tests fail on drift.

CLI::

    python -m digitalmodel.asset_integrity.offering_catalog render    # rewrite the page + capability map
    python -m digitalmodel.asset_integrity.offering_catalog check     # exit 1 on drift or schema problems
    python -m digitalmodel.asset_integrity.offering_catalog validate  # schema rules only
    python -m digitalmodel.asset_integrity.offering_catalog gaps      # list the `none` rows
"""

from __future__ import annotations

import argparse
import importlib
import re
import sys
from collections import Counter
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Sequence

import yaml

DATA_DIR = Path(__file__).resolve().parent / "data"
FFS_CATALOG = DATA_DIR / "ffs_offering_catalog.yml"
DESIGN_CATALOG = DATA_DIR / "ffs_design_screen_catalog.yml"
CROSSWALK = DATA_DIR / "ffs_damage_mechanism_crosswalk.yml"

# Repo-relative targets; only valid in a source checkout (the CLI takes --page / --capability-map overrides).
REPO_ROOT = Path(__file__).resolve().parents[3]
PAGE = REPO_ROOT / "docs" / "domains" / "asset-integrity" / "ffs-offering-catalog.md"
CAPABILITY_MAP = REPO_ROOT / "docs" / "capability-map" / "capabilities-added.yml"
WORKFLOW_REGISTRY = REPO_ROOT / "docs" / "registry" / "workflows.yaml"

STATUSES = frozenset({"live", "workflow", "validated", "engine", "planned", "none"})
TIERS = frozenset({"T1", "T2", "T3"})
ENGINE_STATUSES_NEEDING_MODULE = frozenset({"live", "workflow", "validated", "engine"})
ORDER = {"none": 0, "planned": 1, "engine": 2, "workflow": 3, "validated": 3, "live": 4}
STATUS_DOC = {
    "live": "engine, tests, validation record, registered durable workflow with example",
    "workflow": "registered durable workflow with example, no validation record",
    "validated": "engine + tests + validation record, no registered workflow",
    "engine": "engine + tests only",
    "planned": "issue filed",
    "none": "roadmap candidate, no issue yet",
}
THRESHOLD_RE = r"(?:<|>|≥|≤)\s?\d+\s?(?:%|mm)"
ROW_KEYS = frozenset(
    {
        "mechanism",
        "codes",
        "parts",
        "engines",
        "tiers",
        "status",
        "caveat",
        "note",
        "issue",
    }
)
DESIGN_TOKENS_BANNED_FROM_FFS = ("free-span", "on-bottom-stability")

RENDER_CMD = "python -m digitalmodel.asset_integrity.offering_catalog render"
GENERATED_HEADER = (
    "<!-- GENERATED FILE, do not edit. Source: src/digitalmodel/asset_integrity/data/ffs_offering_catalog.yml "
    f"and ffs_design_screen_catalog.yml. Regenerate: {RENDER_CMD} -->"
)
CAPMAP_MARKER = "# --- GENERATED below this line"
EMPTY = "—"


class CatalogError(ValueError):
    """Raised when a catalog file fails its schema rules."""


# ---------------------------------------------------------------------------
# data model
# ---------------------------------------------------------------------------


@dataclass(frozen=True)
class Row:
    """One defect / mechanism row, flattened with its industry and asset."""

    industry: str
    asset: str
    mechanism: str
    codes: tuple[str, ...]
    parts: tuple[int, ...]
    engines: tuple[str, ...]
    tiers: tuple[str, ...]
    status: str
    caveat: str | None = None
    note: str | None = None
    issue: int | None = None
    design: bool = False

    @property
    def where(self) -> str:
        return f"{self.industry} / {self.asset} / {self.mechanism}"


def _row(d: dict, industry: str, asset: str, design: bool = False) -> Row:
    return Row(
        industry=industry,
        asset=asset,
        mechanism=str(d["mechanism"]),
        codes=tuple(d.get("codes", [])),
        parts=tuple(d.get("parts", [])),
        engines=tuple(d.get("engines", [])),
        tiers=tuple(d.get("tiers", [])),
        status=str(d["status"]),
        caveat=d.get("caveat"),
        note=d.get("note"),
        issue=d.get("issue"),
        design=design,
    )


@dataclass
class Catalog:
    """The three catalogs plus the raw FFS text (some rules are text rules)."""

    ffs: dict
    design: dict
    crosswalk: dict
    ffs_text: str = ""

    # -- flattening ---------------------------------------------------------
    @property
    def rows(self) -> list[Row]:
        return [
            _row(d, ind["id"], asset["asset"])
            for ind in self.ffs["industries"]
            for asset in ind["assets"]
            for d in asset["defects"]
        ]

    @property
    def design_rows(self) -> list[Row]:
        return [
            _row(r, s["id"], s["title"], design=True)
            for s in self.design["screens"]
            for r in s["rows"]
        ]

    def _pool(self, include_design: bool) -> list[Row]:
        return self.rows + self.design_rows if include_design else self.rows

    @property
    def engines(self) -> dict[str, dict]:
        """FFS engines then design engines (keys are disjoint by rule)."""
        return {**self.ffs["engines"], **self.design["engines"]}

    def section_number(self, industry_or_screen_id: str) -> int:
        ids = [i["id"] for i in self.ffs["industries"]] + [
            s["id"] for s in self.design["screens"]
        ]
        return ids.index(industry_or_screen_id) + 1

    # -- queries ------------------------------------------------------------
    def by_industry(self, industry_id: str) -> list[Row]:
        ids = {i["id"] for i in self.ffs["industries"]} | {
            s["id"] for s in self.design["screens"]
        }
        if industry_id not in ids:
            raise KeyError(industry_id)
        return [r for r in self._pool(True) if r.industry == industry_id]

    def by_code(self, code: str, include_design: bool = False) -> list[Row]:
        if code not in self.ffs["codes"]:
            raise KeyError(code)
        return [r for r in self._pool(include_design) if code in r.codes]

    def by_status(self, status: str, include_design: bool = False) -> list[Row]:
        if status not in STATUSES:
            raise ValueError(
                f"unknown status {status!r}; expected one of {sorted(STATUSES)}"
            )
        return [r for r in self._pool(include_design) if r.status == status]

    def gaps(self, include_design: bool = False) -> list[Row]:
        """Rows with status ``none``: no engine and no issue, the roadmap beyond the filed issues."""
        return self.by_status("none", include_design)

    def summary(self) -> Counter:
        """Coverage summary over FFS rows only (design screens are not FFS verdicts)."""
        counts = Counter({s: 0 for s in STATUSES})
        counts.update(r.status for r in self.rows)
        counts["total"] = len(self.rows)
        return counts

    def mechanisms_for_part(self, part: int) -> list[dict]:
        """API RP 571 mechanisms whose crosswalk lists the given API 579 part."""
        return [
            {**m, "category": c["name"]}
            for c in self.crosswalk["categories"]
            for m in c["mechanisms"]
            if part in m["parts"]
        ]


# ---------------------------------------------------------------------------
# loading
# ---------------------------------------------------------------------------


def _read_yaml(path: Path) -> dict:
    data = yaml.safe_load(path.read_text(encoding="utf-8"))
    if not isinstance(data, dict):
        raise CatalogError(f"{path}: expected a mapping at top level")
    return data


def load(
    ffs_path: Path = FFS_CATALOG,
    design_path: Path = DESIGN_CATALOG,
    crosswalk_path: Path = CROSSWALK,
) -> Catalog:
    """Load the three catalogs. Structural shape is checked here; cross-reference rules are in ``validate``."""
    ffs_text = Path(ffs_path).read_text(encoding="utf-8")
    ffs = yaml.safe_load(ffs_text)
    design = _read_yaml(Path(design_path))
    crosswalk = _read_yaml(Path(crosswalk_path))
    for key in ("codes", "engines", "industries", "tiers", "sources"):
        if key not in ffs:
            raise CatalogError(f"{ffs_path}: missing top-level key {key!r}")
    for key in ("engines", "screens", "source_catalog"):
        if key not in design:
            raise CatalogError(f"{design_path}: missing top-level key {key!r}")
    for key in ("taxonomy", "target", "categories"):
        if key not in crosswalk:
            raise CatalogError(f"{crosswalk_path}: missing top-level key {key!r}")
    return Catalog(ffs=ffs, design=design, crosswalk=crosswalk, ffs_text=ffs_text)


def load_registry_ids(path: Path = WORKFLOW_REGISTRY) -> set[str]:
    reg = yaml.safe_load(Path(path).read_text(encoding="utf-8"))
    return {w["id"] for w in reg["workflows"]}


# ---------------------------------------------------------------------------
# validation rules (each returns a list of problems; empty means pass)
# ---------------------------------------------------------------------------


def _check_engine(
    key: str,
    eng: dict,
    *,
    registry_ids: set[str] | None,
    import_modules: bool,
    repo: Path,
) -> list[str]:
    out: list[str] = []
    if eng.get("status") not in STATUSES:
        return [f"{key}: unknown engine status {eng.get('status')!r}"]
    if eng["status"] in ENGINE_STATUSES_NEEDING_MODULE:
        if "module" not in eng:
            out.append(f"{key}: engine status needs a module")
        elif import_modules:
            try:
                importlib.import_module(f"digitalmodel.{eng['module']}")
            except Exception as exc:  # noqa: BLE001 - report, do not raise
                out.append(
                    f"{key}: module digitalmodel.{eng['module']} failed to import ({exc})"
                )
    if eng["status"] == "planned" and not isinstance(eng.get("issue"), int):
        out.append(f"{key}: planned needs an issue")
    if eng["status"] in {"live", "workflow"}:
        wf = eng.get("workflow")
        if not wf:
            out.append(f"{key}: live/workflow requires a registered workflow")
        else:
            if registry_ids is not None and wf not in registry_ids:
                out.append(f"{key}: workflow id not in registry")
            if not (repo / "examples" / "workflows" / wf / "input.yml").exists():
                out.append(
                    f"{key}: live/workflow needs a committed example examples/workflows/{wf}/input.yml"
                )
    if "route" in eng:
        out.append(
            f"{key}: 'route' is not a catalog field (delivery is the registered workflow)"
        )
    if eng["status"] in {"live", "validated"}:
        rec = eng.get("validation")
        if not rec or not (repo / rec).exists():
            out.append(f"{key}: {eng['status']} needs an existing validation record")
    if (
        "workflow" in eng
        and registry_ids is not None
        and eng["workflow"] not in registry_ids
    ):
        out.append(f"{key}: unknown workflow {eng['workflow']}")
    return out


def check_engines(
    cat: Catalog,
    *,
    registry_ids: set[str] | None = None,
    import_modules: bool = False,
    repo: Path = REPO_ROOT,
) -> list[str]:
    """Engine registry rules: module present/importable, planned has an issue, live/workflow have a registered
    workflow with a committed example, live/validated have an existing record, every workflow id is registered."""
    out: list[str] = []
    for key, eng in cat.ffs["engines"].items():
        out += _check_engine(
            key,
            eng,
            registry_ids=registry_ids,
            import_modules=import_modules,
            repo=repo,
        )
    return out


def _check_row_keys(cat: Catalog, design: bool) -> list[str]:
    """An unquoted comma in a flow-style mechanism splits it into stray keys; catch that at the source."""
    if design:
        raw = [(s["id"], r) for s in cat.design["screens"] for r in s["rows"]]
    else:
        raw = [
            (i["id"], d)
            for i in cat.ffs["industries"]
            for a in i["assets"]
            for d in a["defects"]
        ]
    return [
        f"{where} / {d.get('mechanism')}: stray row keys {sorted(set(d) - ROW_KEYS)} (quote the mechanism?)"
        for where, d in raw
        if set(d) - ROW_KEYS
    ]


def _check_row_refs(r: Row, codes: dict, engines: dict, parts: dict) -> list[str]:
    out: list[str] = []
    if r.status not in STATUSES:
        out.append(f"{r.where}: unknown status {r.status!r}")
    if not r.tiers or not set(r.tiers) <= TIERS:
        out.append(f"{r.where}: bad tiers {list(r.tiers)}")
    out += [f"{r.where}: unknown code {c}" for c in r.codes if c not in codes]
    out += [f"{r.where}: unknown engine {e}" for e in r.engines if e not in engines]
    out += [f"{r.where}: unknown API 579 part {p}" for p in r.parts if p not in parts]
    if r.parts and "api-579-1" not in r.codes:
        out.append(f"{r.where}: parts given without api-579-1")
    return out


def check_rows(cat: Catalog) -> list[str]:
    """Every FFS row resolves: status, tiers, codes, engines and API 579 parts all exist."""
    codes, engines, parts = (
        cat.ffs["codes"],
        cat.ffs["engines"],
        cat.ffs["codes"]["api-579-1"]["parts"],
    )
    out: list[str] = _check_row_keys(cat, design=False)
    for key, code in codes.items():
        if not code.get("short"):
            out.append(f"code {key}: missing short label")
    for r in cat.rows:
        out += _check_row_refs(r, codes, engines, parts)
    return out


def _check_row_status(r: Row, engines: dict) -> list[str]:
    if r.status == "none":
        return (
            [f"{r.where}: 'none' row lists engines {list(r.engines)}"]
            if r.engines
            else []
        )
    if not r.engines:
        return [f"{r.where}: status {r.status} without engines"]
    out: list[str] = []
    strongest = max(ORDER[engines[e]["status"]] for e in r.engines)
    if ORDER[r.status] > strongest:
        out.append(f"{r.where}: row status stronger than its engines")
    if r.status == "planned" and not (
        any(engines[e]["status"] == "planned" for e in r.engines)
        or isinstance(r.issue, int)
    ):
        out.append(f"{r.where}: planned row without a planned engine or issue")
    if r.status == "live" and not all(
        engines[e]["status"] in {"live", "validated"} for e in r.engines
    ):
        out.append(f"{r.where}: live row leans on an unvalidated engine")
    return out


def check_status_semantics(cat: Catalog) -> list[str]:
    """A row is never stronger than its engines; ``none`` rows list no engines; ``live`` rows lean only on
    live/validated engines; ``planned`` rows point at a planned engine or carry an issue."""
    engines = cat.ffs["engines"]
    out: list[str] = []
    for r in cat.rows:
        if all(e in engines for e in r.engines):
            out += _check_row_status(r, engines)
    return out


def check_no_licensed_thresholds(cat: Catalog) -> list[str]:
    """Percent or mm thresholds attributed to a standard must not appear in the catalog text."""
    found = re.findall(THRESHOLD_RE, cat.ffs_text)
    return [f"threshold-looking values in catalog: {found}"] if found else []


def check_design(
    cat: Catalog,
    *,
    registry_ids: set[str] | None = None,
    import_modules: bool = False,
    repo: Path = REPO_ROOT,
) -> list[str]:
    """Design screens obey the FFS rules, share no engine keys with the FFS catalog, and the FFS catalog
    no longer carries the design screens as rows."""
    out: list[str] = []
    if cat.design.get("source_catalog") != FFS_CATALOG.name:
        out.append(f"design catalog source_catalog must be {FFS_CATALOG.name}")
    d_engines = cat.design["engines"]
    for key, eng in d_engines.items():
        if key in cat.ffs["engines"]:
            out.append(f"{key}: engine listed in both catalogs")
        out += _check_engine(
            key,
            eng,
            registry_ids=registry_ids,
            import_modules=import_modules,
            repo=repo,
        )
    parts = cat.ffs["codes"]["api-579-1"]["parts"]
    out += _check_row_keys(cat, design=True)
    for r in cat.design_rows:
        out += _check_row_refs(r, cat.ffs["codes"], d_engines, parts)
        if all(e in d_engines for e in r.engines):
            out += _check_row_status(r, d_engines)
    for token in DESIGN_TOKENS_BANNED_FROM_FFS:
        if token in cat.ffs_text:
            out.append(f"{token} still in the FFS catalog")
    return out


def check_crosswalk(cat: Catalog) -> list[str]:
    """Every API RP 571 mechanism maps to at least one existing API 579 part; names are unique."""
    parts = set(cat.ffs["codes"]["api-579-1"]["parts"])
    xw = cat.crosswalk
    out: list[str] = []
    if xw.get("taxonomy") not in cat.ffs["codes"] or xw.get("target") != "api-579-1":
        out.append(
            "crosswalk taxonomy/target must be catalog codes (api-rp-571 -> api-579-1)"
        )
    seen: Counter = Counter()
    for group in xw["categories"]:
        for m in group.get("mechanisms", []):
            seen[m["name"]] += 1
            bad = [p for p in m.get("parts", []) if p not in parts]
            if not m.get("parts") or bad:
                out.append(
                    f"crosswalk {m['name']}: parts {m.get('parts')} not all in API 579 parts"
                )
    out += [f"crosswalk duplicate mechanism name {n}" for n, k in seen.items() if k > 1]
    return out


def validate(
    cat: Catalog,
    *,
    registry_ids: set[str] | None = None,
    import_modules: bool = False,
    repo: Path = REPO_ROOT,
) -> list[str]:
    """Run every rule; return the list of problems (empty means the catalogs are consistent)."""
    problems = check_engines(
        cat, registry_ids=registry_ids, import_modules=import_modules, repo=repo
    )
    problems += check_rows(cat)
    problems += check_status_semantics(cat)
    problems += check_no_licensed_thresholds(cat)
    problems += check_design(
        cat, registry_ids=registry_ids, import_modules=import_modules, repo=repo
    )
    problems += check_crosswalk(cat)
    return problems


# ---------------------------------------------------------------------------
# Markdown rendering
# ---------------------------------------------------------------------------


def asset_column(industry: dict) -> bool:
    """Whether an industry's table carries an Asset column (explicit flag, else more than one asset)."""
    flag = industry.get("asset_column")
    return bool(flag) if flag is not None else len(industry["assets"]) > 1


def _cap(s: str) -> str:
    return s[:1].upper() + s[1:]


def _codes_cell(r: Row, codes: dict) -> str:
    cells = []
    for c in r.codes:
        short = codes[c]["short"]
        if c == "api-579-1" and r.parts:
            short = f"{short} Pt {'/'.join(str(p) for p in r.parts)}"
        cells.append(short)
    return ", ".join(cells) or EMPTY


def _engines_cell(r: Row, engines: dict) -> str:
    cells = []
    for e in r.engines:
        eng = engines[e]
        cells.append(
            f"{e} (#{eng['issue']})"
            if eng["status"] == "planned" and eng.get("issue")
            else e
        )
    return ", ".join(cells) or EMPTY


def _status_cell(r: Row) -> str:
    extras = []
    if r.issue:
        extras.append(f"#{r.issue}")
    if r.caveat:
        extras.append(str(r.caveat))
    if r.note:
        extras.append(str(r.note))
    return f"{r.status} ({'; '.join(extras)})" if extras else r.status


def _table(
    rows: Iterable[Row], *, codes: dict, engines: dict, with_asset: bool, first_col: str
) -> list[str]:
    head = (["Asset"] if with_asset else []) + [
        first_col,
        "Governing codes",
        "Engine(s)",
        "Tier",
        "Status",
    ]
    out = ["| " + " | ".join(head) + " |", "|" + "---|" * len(head)]
    for r in rows:
        cells = ([_cap(r.asset)] if with_asset else []) + [
            _cap(r.mechanism),
            _codes_cell(r, codes),
            _engines_cell(r, engines),
            ", ".join(r.tiers),
            _status_cell(r),
        ]
        out.append("| " + " | ".join(cells) + " |")
    return out


def _how_to_read(cat: Catalog) -> list[str]:
    tiers = "; ".join(f"{k} {v}" for k, v in cat.ffs["tiers"].items())
    statuses = " · ".join(
        f"`{s}` ({STATUS_DOC[s]})"
        for s in ("live", "workflow", "validated", "engine", "planned", "none")
    )
    parts = " · ".join(
        f"{n} {name}" for n, name in cat.ffs["codes"]["api-579-1"]["parts"].items()
    )
    return [
        "## How to read these tables",
        "",
        f"- **Tier**: {tiers}.",
        f"- **Status**: {statuses}. A row is never stronger than the strongest engine it depends on; "
        "`none` rows list no engines.",
        "- Codes are cited as publisher identifiers only. Clause text, tables, figures and licensed numeric "
        "thresholds stay out of this repo; thresholds are user inputs whose public defaults are recorded on the "
        "implementing issue.",
        f"- API 579-1 parts: {parts}. API RP 571 supplies the damage-mechanism taxonomy that routes a finding to a "
        "part (`ffs_damage_mechanism_crosswalk.yml`).",
        "",
    ]


def _summary_block(cat: Catalog) -> list[str]:
    s = cat.summary()
    out = ["## Coverage summary", "", "| Status | Rows |", "|---|---|"]
    out += [
        f"| {st} | {s[st]} |"
        for st in ("live", "workflow", "validated", "engine", "planned", "none")
    ]
    out += [f"| total | {s['total']} |", ""]
    if s["live"] == 0:
        lead = (
            f"No row is `live` today: the {s['workflow']} `workflow` rows are not qualified as `live` and the "
            f"{s['validated']} `validated` rows lack a registered workflow."
        )
    else:
        lead = f"{s['live']} of {s['total']} rows are `live` today."
    gaps_by_industry: dict[str, list[str]] = {}
    for r in cat.gaps():
        gaps_by_industry.setdefault(r.industry, []).append(r.mechanism)
    gap_text = "; ".join(
        f"**{ind}**: {', '.join(ms)}" for ind, ms in gaps_by_industry.items()
    )
    out += [
        f"{lead} The {s['none']} `none` rows are the roadmap beyond the filed issues: {gap_text}.",
        "",
    ]
    return out


def _sources_block(cat: Catalog) -> list[str]:
    src = cat.ffs["sources"]
    out = [f"## Sources (public overviews consulted {src['consulted']})", ""]
    for item in src["items"]:
        links = ", ".join(f"[{link['label']}]({link['url']})" for link in item["links"])
        out.append(f"- {item['topic']}: {links}")
    return out


def render_markdown(cat: Catalog) -> str:
    """Render the offering-catalog page from the two catalogs. Deterministic; LF line endings."""
    codes, engines = cat.ffs["codes"], cat.engines
    epic = str(cat.ffs.get("epic", "")).rsplit("#", 1)[-1]
    lines = [
        GENERATED_HEADER,
        f"# FFS Offering Catalog — Industry Lookup Tables (data {cat.ffs['generated']})",
        "",
        f"**Epic:** #{epic} Phase 4 · **Data:** `src/digitalmodel/asset_integrity/data/{FFS_CATALOG.name}` "
        f"(FFS verdicts) and `{DESIGN_CATALOG.name}` (design screens, owner decision D5); this page is rendered "
        f"from both by `{RENDER_CMD}` and `tests/asset_integrity/test_offering_catalog_render.py` fails on drift",
        "**Companion notes:** `ffs-readiness-review-2026-09-25.md` and `level3-and-part9-program-2026-09-25.md` "
        "(PR #2186), plan `docs/plans/2026-09-25-issue-1057-ffs-offering-program.md`",
        "",
    ]
    lines += _how_to_read(cat)
    n = 0
    for ind in cat.ffs["industries"]:
        n += 1
        lines += [f"## {n}. {ind['title']}", ""]
        if ind.get("intro"):
            lines += [ind["intro"], ""]
        with_asset = asset_column(ind)
        rows = cat.by_industry(ind["id"])
        if not with_asset and len(ind["assets"]) > 1:
            # keep the asset visible when the layout has no Asset column
            rows = [
                Row(**{**r.__dict__, "mechanism": f"{_cap(r.asset)}: {r.mechanism}"})
                if r.asset != ind["assets"][0]["asset"]
                else r
                for r in rows
            ]
        lines += _table(
            rows,
            codes=codes,
            engines=engines,
            with_asset=with_asset,
            first_col="Defect / mechanism",
        )
        lines.append("")
    for screen in cat.design["screens"]:
        n += 1
        lines += [f"## {n}. {screen['title']}", ""]
        if screen.get("intro"):
            lines += [screen["intro"], ""]
        lines += _table(
            cat.by_industry(screen["id"]),
            codes=codes,
            engines=engines,
            with_asset=False,
            first_col="Screen",
        )
        lines.append("")
    lines += _summary_block(cat)
    lines += _sources_block(cat)
    return "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# capability map (docs/capability-map/capabilities-added.yml, engines.ffs block)
# ---------------------------------------------------------------------------

_CAPMAP_FIELDS = ("status", "module", "workflow", "validation", "issue")


def capability_map_entries(cat: Catalog) -> dict[str, dict]:
    """Per-engine entries for the capabilities page's ``ffs`` section, straight from the FFS engine registry."""
    return {
        key: {f: eng[f] for f in _CAPMAP_FIELDS if f in eng}
        for key, eng in cat.ffs["engines"].items()
    }


def render_capability_map(cat: Catalog, current_text: str) -> str:
    """Replace everything from the generated marker onward with the regenerated ``engines.ffs`` block."""
    if CAPMAP_MARKER not in current_text:
        raise CatalogError(
            f"capability map lacks the marker line starting {CAPMAP_MARKER!r}"
        )
    head = current_text.split(CAPMAP_MARKER, 1)[0]
    lines = [
        f"{CAPMAP_MARKER} by `{RENDER_CMD}`",
        f"# --- from src/digitalmodel/asset_integrity/data/{FFS_CATALOG.name} (issue #2197). Do not edit by hand.",
        "# Format: engines.<section id>.<engine key>: {status, module?, workflow?, validation?, issue?}",
        "engines:",
        "  ffs:",
    ]
    for key, entry in capability_map_entries(cat).items():
        flow = yaml.safe_dump(
            entry, default_flow_style=True, width=10_000, sort_keys=False
        ).strip()
        lines.append(f"    {key}: {flow}")
    return head + "\n".join(lines) + "\n"


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------


def _drift(label: str, expected: str, actual: str) -> list[str]:
    if expected == actual:
        return []
    exp, act = expected.splitlines(), actual.splitlines()
    first = next(
        (i for i, (a, b) in enumerate(zip(exp, act)) if a != b), min(len(exp), len(act))
    )
    return [
        f"drift in {label}: first difference at line {first + 1} (run: {RENDER_CMD})"
    ]


def check(cat: Catalog, page: Path, capability_map: Path) -> list[str]:
    problems = validate(cat)
    page_text = page.read_text(encoding="utf-8") if page.exists() else ""
    problems += _drift(str(page), render_markdown(cat), page_text)
    cap_text = capability_map.read_text(encoding="utf-8")
    problems += _drift(
        str(capability_map), render_capability_map(cat, cap_text), cap_text
    )
    return problems


def main(argv: Sequence[str] | None = None) -> int:
    ap = argparse.ArgumentParser(
        prog="python -m digitalmodel.asset_integrity.offering_catalog",
        description=__doc__,
    )
    sub = ap.add_subparsers(dest="cmd", required=True)
    for name in ("render", "check"):
        p = sub.add_parser(name)
        p.add_argument("--page", type=Path, default=PAGE)
        p.add_argument("--capability-map", type=Path, default=CAPABILITY_MAP)
    sub.add_parser("validate")
    sub.add_parser("gaps")
    args = ap.parse_args(argv)

    cat = load()
    if args.cmd == "validate":
        problems = validate(cat)
    elif args.cmd == "gaps":
        for r in cat.gaps(include_design=True):
            print(f"{r.industry} | {r.asset} | {r.mechanism} | {', '.join(r.codes)}")
        return 0
    elif args.cmd == "check":
        problems = check(cat, args.page, args.capability_map)
    else:  # render
        problems = validate(cat)
        if not problems:
            args.page.write_bytes(render_markdown(cat).encode("utf-8"))
            cap_text = args.capability_map.read_text(encoding="utf-8")
            args.capability_map.write_bytes(
                render_capability_map(cat, cap_text).encode("utf-8")
            )
            print(f"rendered {args.page} and {args.capability_map}")
    for p in problems:
        print(p)
    if not problems:
        print(
            f"{args.cmd}: OK ({len(cat.rows)} FFS rows, {len(cat.design_rows)} design rows)"
        )
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main())
