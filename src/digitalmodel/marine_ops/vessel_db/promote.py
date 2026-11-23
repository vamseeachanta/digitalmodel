"""
ABOUTME: Promote raw vessel datasets (data/vessels/raw/*.json) to analysis-ready
processed/ CSVs, filling estimated radii of gyration where they are absent.

Run:
    uv run python -m digitalmodel.marine_ops.vessel_db.promote

Produces under data/vessels/processed/:
- particulars.csv        principal dims + mass props (estimated gyradii where absent)
- crane_summary.csv      installation crane SWL anchors + deck data
- rao_datasets.csv       index of RAO proxy/benchmark datasets
- metocean_criteria.csv  regional metocean design criteria
- PROVENANCE_REPORT.md   provenance integrity + gyradii estimation log

Every estimated gyradius carries the relation it came from (see gyradii.py);
no bare invented numbers are written.
"""

from __future__ import annotations

import csv
from pathlib import Path
from typing import Optional

from digitalmodel.marine_ops.vessel_db.gyradii import estimate_gyradii
from digitalmodel.marine_ops.vessel_db.loader import (
    Record,
    datasets,
    iter_records,
    parse_value,
    validate_provenance,
    vessels_dir,
)

PARTICULARS_COLS = [
    "vessel_id", "name", "scope", "vessel_type", "owner_operator", "year_built",
    "loa", "lbp", "beam", "depth", "draft", "draft_transit", "displacement", "cb",
    "lcg", "vcg", "tcg",
    "kxx", "kxx_basis", "kyy", "kyy_basis", "kzz", "kzz_basis",
    "n_citations", "n_gaps",
]


def _gyradius(rec: Record, canon: str, estimates: dict) -> tuple[Optional[float], str]:
    """Return ``(value, basis)`` for a gyradius: cited if present, else estimated,
    else gap."""
    num, marker, _ = rec.dimension(canon)
    if marker == "number":
        return num, "cited"
    est = estimates.get({"kxx": "kxx_roll", "kyy": "kyy_pitch", "kzz": "kzz_yaw"}[canon])
    if est is not None:
        return est.value, est.basis
    return None, "gap"


def build_particulars(base: Path) -> tuple[list[dict], list[str]]:
    """Build particulars rows across all *_particulars datasets. Returns
    ``(rows, estimation_log)``."""
    rows: list[dict] = []
    log: list[str] = []
    for scope, layer in datasets(base):
        if layer != "particulars":
            continue
        for rec in iter_records(scope, layer, base):
            dims = rec.canonical_dimensions()
            beam = dims.get("beam")
            lbp = dims.get("lbp") or dims.get("loa")  # documented LOA fallback
            estimates = estimate_gyradii(beam, lbp, rec.vessel_type)
            kxx, kxx_b = _gyradius(rec, "kxx", estimates)
            kyy, kyy_b = _gyradius(rec, "kyy", estimates)
            kzz, kzz_b = _gyradius(rec, "kzz", estimates)
            for axis, basis in (("kxx", kxx_b), ("kyy", kyy_b), ("kzz", kzz_b)):
                if basis.startswith("estimated:"):
                    log.append(f"{rec.name}: {axis} <- {basis}")
            rows.append({
                "vessel_id": rec.vessel_id(),
                "name": rec.name,
                "scope": scope,
                "vessel_type": rec.vessel_type,
                "owner_operator": rec.owner_operator,
                "year_built": rec.year_built,
                "loa": dims.get("loa"),
                "lbp": dims.get("lbp"),
                "beam": beam,
                "depth": dims.get("depth"),
                "draft": dims.get("draft"),
                "draft_transit": dims.get("draft_transit"),
                "displacement": dims.get("displacement"),
                "cb": dims.get("cb"),
                "lcg": dims.get("lcg"),
                "vcg": dims.get("vcg"),
                "tcg": dims.get("tcg"),
                "kxx": kxx, "kxx_basis": kxx_b,
                "kyy": kyy, "kyy_basis": kyy_b,
                "kzz": kzz, "kzz_basis": kzz_b,
                "n_citations": len(rec.citations),
                "n_gaps": len(rec.gaps),
            })
    return rows, log


def build_crane_summary(base: Path) -> list[dict]:
    rows: list[dict] = []
    from digitalmodel.marine_ops.vessel_db.loader import _extract_anchor_points
    for rec in iter_records("install", "crane_deck", base):
        radii, caps, max_swl = _extract_anchor_points(rec.raw_fields)
        f = rec.raw_fields
        def g(*names):
            for n in names:
                if n in f:
                    num, m = parse_value(f[n])
                    if m == "number":
                        return num
            return None
        rows.append({
            "vessel_id": rec.vessel_id(),
            "name": rec.name,
            "vessel_type": rec.vessel_type,
            "max_single_crane_swl_te": max_swl or None,
            "tandem_swl_te": g("tandem_swl_t", "tandem_swl_te", "combined_swl_t"),
            "n_swl_radius_anchors": len(radii),
            "deck_area_m2": g("deck_area_m2", "deck_area_m2_main", "free_deck_area_m2"),
            "deck_strength_t_per_m2": g("deck_strength_t_per_m2", "deck_load_t_per_m2"),
            "dp_class": f.get("dp_class", ""),
            "n_citations": len(rec.citations),
        })
    return rows


def build_rao_index(base: Path) -> list[dict]:
    rows: list[dict] = []
    for scope, layer in datasets(base):
        if layer != "rao":
            continue
        for rec in iter_records(scope, layer, base):
            f = rec.raw_fields
            url = ""
            for c in rec.citations:
                if c.get("url"):
                    url = c["url"]; break
            rows.append({
                "scope": scope,
                "name": rec.name,
                "vessel_type": rec.vessel_type,
                "kind": str(f.get("kind", "")),
                "dof_covered": ";".join(f.get("dof_covered", [])) if isinstance(f.get("dof_covered"), list) else str(f.get("dof_covered", "")),
                "headings_deg": str(f.get("headings_deg", "")),
                "water_depth_m": str(f.get("water_depth_m", f.get("water_depth", ""))),
                "primary_url": url,
                "n_gaps": len(rec.gaps),
            })
    return rows


def build_metocean(base: Path) -> list[dict]:
    rows: list[dict] = []
    for rec in iter_records("support", "metocean", base):
        f = rec.raw_fields
        row = {"region": rec.name}
        for k, v in f.items():
            row[k] = v
        row["n_citations"] = len(rec.citations)
        rows.append(row)
    return rows


def _write_csv(path: Path, rows: list[dict], cols: Optional[list[str]] = None) -> None:
    if not rows:
        return
    if cols is None:
        cols = list({k for r in rows for k in r})
    with open(path, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=cols, extrasaction="ignore")
        w.writeheader()
        for r in rows:
            w.writerow(r)


def promote(base: Optional[Path] = None) -> dict:
    base = base or vessels_dir()
    proc = base / "processed"
    proc.mkdir(exist_ok=True)

    particulars, est_log = build_particulars(base)
    crane = build_crane_summary(base)
    rao = build_rao_index(base)
    metocean = build_metocean(base)

    _write_csv(proc / "particulars.csv", particulars, PARTICULARS_COLS)
    _write_csv(proc / "crane_summary.csv", crane)
    _write_csv(proc / "rao_datasets.csv", rao)
    _write_csv(proc / "metocean_criteria.csv", metocean)

    violations = validate_provenance(base)
    _write_provenance_report(proc / "PROVENANCE_REPORT.md", est_log, violations,
                             len(particulars), len(crane), len(rao), len(metocean))

    return {
        "particulars": len(particulars),
        "crane": len(crane),
        "rao": len(rao),
        "metocean": len(metocean),
        "estimated_gyradii": len(est_log),
        "provenance_violations": len(violations),
    }


def _write_provenance_report(path, est_log, violations, n_part, n_crane, n_rao, n_met):
    lines = ["# Vessel DB — Provenance & Estimation Report", ""]
    lines.append(f"- particulars rows: {n_part}")
    lines.append(f"- crane summary rows: {n_crane}")
    lines.append(f"- RAO datasets: {n_rao}")
    lines.append(f"- metocean regions: {n_met}")
    lines.append(f"- gyradii estimated: {len(est_log)}")
    lines.append(f"- provenance violations: {len(violations)}")
    lines.append("")
    lines.append("## Provenance integrity")
    lines.append("")
    if not violations:
        lines.append("✅ No un-cited hard numbers. Every numeric field is cited, "
                     "`estimated:<basis>`, or `gap`.")
    else:
        lines.append("⚠️ Numeric fields lacking a citation (and not marked estimated/gap):")
        lines.append("")
        lines.append("| scope | layer | record | field | value |")
        lines.append("|---|---|---|---|---|")
        for v in violations:
            lines.append(f"| {v.scope} | {v.layer} | {v.record} | {v.field_name} | {v.value} |")
    lines.append("")
    lines.append("## Estimated radii of gyration")
    lines.append("")
    lines.append("Gyradii are rarely public; these were filled from documented "
                 "relations (see `gyradii.py`). Cited values were kept as-is.")
    lines.append("")
    for entry in est_log:
        lines.append(f"- {entry}")
    path.write_text("\n".join(lines) + "\n")


if __name__ == "__main__":
    result = promote()
    print("Vessel DB promotion complete:")
    for k, v in result.items():
        print(f"  {k}: {v}")
    if result["provenance_violations"]:
        print(f"\n⚠️  {result['provenance_violations']} provenance violations — "
              "see processed/PROVENANCE_REPORT.md")
