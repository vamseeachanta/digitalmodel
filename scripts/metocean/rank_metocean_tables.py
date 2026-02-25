"""
Rank shortlisted API RP 2MET tables by presence of metocean keywords
to identify likely candidates for standardized processing.

Usage:
  python scripts/metocean/rank_metocean_tables.py
"""

from __future__ import annotations

import os
import json
from collections import Counter
from typing import List, Dict

import pandas as pd


ROOT = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
EXTRACT_DIR = os.path.join(
    ROOT, "data", "metocean", "raw", "extracted", "api_rp_2met_jan2021"
)
SHORTLIST_DIR = os.path.join(
    ROOT, "data", "metocean", "raw", "extracted", "api_rp_2met_jan2021_shortlist"
)
OUT_PATH = os.path.join(SHORTLIST_DIR, "shortlist_ranking.csv")

TOKENS = [
    # waves
    "hs", "h_s", "significant wave", "hs (m)", "wave height", "tp", "t_p", "tz", "t_z",
    # wind
    "wind", "u10", "10 m", "m/s", "gust",
    # current
    "current", "profile", "depth", "speed",
    # stats
    "return period", "probability", "exceedance", "percent",
]


def text_blob(df: pd.DataFrame) -> str:
    head = " ".join(map(str, df.columns))
    sample = df.head(8).astype(str).agg(" ".join, axis=1).str.cat(sep=" ")
    return f"{head} {sample}".lower()


def main() -> None:
    if not os.path.isdir(SHORTLIST_DIR):
        print("Shortlist directory not found; run select_metocean_tables.py first.")
        return
    meta = {}
    meta_path = os.path.join(EXTRACT_DIR, "metadata.json")
    if os.path.exists(meta_path):
        with open(meta_path, "r", encoding="utf-8") as f:
            m = json.load(f)
            meta = {str(t.get("csv")): t for t in m.get("tables", [])}

    rows: List[Dict] = []
    for name in sorted(os.listdir(SHORTLIST_DIR)):
        if not name.lower().endswith(".csv"):
            continue
        path = os.path.join(SHORTLIST_DIR, name)
        try:
            try:
                df = pd.read_csv(path, encoding="utf-8", on_bad_lines="skip")
            except UnicodeDecodeError:
                df = pd.read_csv(path, encoding="latin-1", on_bad_lines="skip")
        except Exception:
            continue
        blob = text_blob(df)
        matches = [tok for tok in TOKENS if tok in blob]
        counts = Counter(matches)
        score = sum(counts.values())
        rows.append({
            "csv": name,
            "score": score,
            "unique_tokens": len(set(matches)),
            "tokens": ", ".join(sorted(set(matches))),
            "rows": int(meta.get(name, {}).get("rows", len(df))),
            "cols": int(meta.get(name, {}).get("cols", len(df.columns))),
        })

    out = pd.DataFrame(rows).sort_values(["score", "unique_tokens", "rows"], ascending=[False, False, False])
    out.to_csv(OUT_PATH, index=False)
    print(f"Wrote ranking -> {OUT_PATH} ({len(out)} files)")


if __name__ == "__main__":
    main()

