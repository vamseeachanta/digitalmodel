"""
Scan extracted API RP 2MET tables for likely metocean metrics and
copy matching CSVs to a shortlist folder for manual review.

Usage:
  python scripts/metocean/select_metocean_tables.py
"""

from __future__ import annotations

import os
import json
import shutil
from typing import List, Dict

import pandas as pd


ROOT = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
EXTRACT_DIR = os.path.join(
    ROOT, "data", "metocean", "raw", "extracted", "api_rp_2met_jan2021"
)
SHORTLIST_DIR = os.path.join(
    ROOT, "data", "metocean", "raw", "extracted", "api_rp_2met_jan2021_shortlist"
)

# Keywords to identify metocean tables
KEYWORDS = [
    "hs", "significant wave", "tp", "tz", "wave height", "sea state",
    "wind", "u10", "10 m", "wind speed", "gust",
    "current", "speed", "depth", "profile",
    "return period", "probability", "exceedance", "rp",
]


def contains_keywords(df: pd.DataFrame) -> bool:
    # search in headers and first few rows for tokens
    text = " ".join(map(str, df.columns)).lower()
    sample = df.head(6).astype(str).agg(" ".join, axis=1).str.lower().str.cat(sep=" ")
    hay = f"{text} {sample}"
    return any(k in hay for k in KEYWORDS)


def main() -> None:
    os.makedirs(SHORTLIST_DIR, exist_ok=True)
    meta_path = os.path.join(EXTRACT_DIR, "metadata.json")
    with open(meta_path, "r", encoding="utf-8") as f:
        meta = json.load(f)

    selected: List[Dict] = []
    for t in meta.get("tables", []):
        csv_name = t.get("csv")
        if not csv_name:
            continue
        p = os.path.join(EXTRACT_DIR, csv_name)
        if not os.path.exists(p):
            continue
        try:
            try:
                df = pd.read_csv(p, encoding="utf-8", on_bad_lines="skip")
            except UnicodeDecodeError:
                df = pd.read_csv(p, encoding="latin-1", on_bad_lines="skip")
        except Exception:
            continue
        if df.empty:
            continue
        if contains_keywords(df):
            dst = os.path.join(SHORTLIST_DIR, csv_name)
            shutil.copy2(p, dst)
            selected.append({"index": t.get("index"), "csv": csv_name, "rows": int(t.get("rows", 0)), "cols": int(t.get("cols", 0))})

    report = {
        "matched_count": len(selected),
        "files": selected,
        "keywords": KEYWORDS,
    }
    with open(os.path.join(SHORTLIST_DIR, "shortlist_report.json"), "w", encoding="utf-8") as f:
        json.dump(report, f, indent=2)

    print(f"Shortlisted {len(selected)} tables -> {SHORTLIST_DIR}")


if __name__ == "__main__":
    main()

