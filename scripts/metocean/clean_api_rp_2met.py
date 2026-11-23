"""
Clean and standardize extracted metocean tables from API RP 2MET into
three processed datasets: waves, wind, and current.

Requirements:
  pip install pandas

Usage:
  python scripts/metocean/clean_api_rp_2met.py
"""

from __future__ import annotations

import os
import json
from typing import Dict, List, Tuple

import pandas as pd


ROOT = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
EXTRACT_DIR = os.path.join(
    ROOT, "data", "metocean", "raw", "extracted", "api_rp_2met_jan2021"
)
PROCESSED_DIR = os.path.join(ROOT, "data", "metocean", "processed")
SHORTLIST_DIR = os.path.join(
    ROOT, "data", "metocean", "raw", "extracted", "api_rp_2met_jan2021_shortlist"
)


STANDARD_COLUMNS = {
    "waves": [
        "region",
        "location",
        "return_period_years",
        "probability_exceedance",
        "hs_m",
        "tp_s",
        "tz_s",
        "direction_deg",
        "source_table",
    ],
    "wind": [
        "region",
        "location",
        "return_period_years",
        "probability_exceedance",
        "u10_ms",
        "direction_deg",
        "height_m",
        "source_table",
    ],
    "current": [
        "region",
        "location",
        "return_period_years",
        "probability_exceedance",
        "speed_ms",
        "direction_deg",
        "depth_m",
        "source_table",
    ],
}


SYNONYMS: Dict[str, List[str]] = {
    # Waves
    "hs_m": ["hs", "significant_wave_height", "hs (m)", "hs(m)", "h_s", "h_s (m)", "sig_wave_ht"],
    "tp_s": ["tp", "peak_period", "t_p", "tpeak", "t_p (s)", "tp(s)"],
    "tz_s": ["tz", "mean_zero_crossing_period", "t_z", "tzero", "t_z (s)", "tz(s)"],
    # Wind
    "u10_ms": ["u10", "u_10", "wind_speed", "wind speed", "v10", "u (10 m)", "u10 (m/s)", "windspeed"],
    "height_m": ["z", "height", "ref_height", "zref", "height (m)", "10m", "ref height"],
    # Current
    "speed_ms": ["speed", "current_speed", "u", "v", "speed (m/s)", "current speed"],
    "depth_m": ["depth", "z", "d", "depth (m)", "z (m)"],
    # Shared
    "direction_deg": ["dir", "direction", "direction (deg)", "dir (deg)", "bearing"],
    "return_period_years": ["rp", "return_period", "return period", "return period (years)", "years"],
    "probability_exceedance": ["p", "%", "probability", "exceedance", "% exceedance", "annual probability"],
    "region": ["region", "area", "basin"],
    "location": ["location", "site", "station", "metocean location"],
}


def normalize_header(text: str) -> str:
    t = str(text or "").strip().lower()
    t = t.replace("\n", " ")
    t = t.replace("%", " percent ")
    t = t.replace("/", " per ")
    t = "".join(ch if ch.isalnum() or ch.isspace() else " " for ch in t)
    t = " ".join(t.split())
    t = t.replace(" ", "_")
    return t


def map_to_standard(cols: List[str]) -> Dict[str, str]:
    norm = [normalize_header(c) for c in cols]
    mapping: Dict[str, str] = {}
    for std, syns in SYNONYMS.items():
        for i, c in enumerate(norm):
            if c == std or c in syns:
                mapping[cols[i]] = std
    # carry over region/location if present in raw
    for key in ("region", "location"):
        if key in norm and key not in mapping.values():
            i = norm.index(key)
            mapping[cols[i]] = key
    return mapping


SEARCH_TOKENS = [
    "hs", "significant", "wave", "tp", "tz", "u10", "wind",
    "current", "speed", "depth", "return", "period", "direction",
]


def infer_header(df: pd.DataFrame) -> pd.DataFrame:
    # If columns are unnamed or empty-like, try to use a suitable row as header
    unnamed = all(str(c).startswith("Unnamed") or str(c).strip() == "" for c in df.columns)
    if unnamed:
        # Look for a row with header-like tokens
        max_scan = min(10, len(df))
        header_idx = None
        for i in range(max_scan):
            values = [str(x).lower() for x in df.iloc[i].fillna("")]
            row_concat = " ".join(values)
            if any(tok in row_concat for tok in SEARCH_TOKENS) and sum(v != "" for v in values) >= 2:
                header_idx = i
                break
        if header_idx is None:
            # fallback to first non-empty row
            for i in range(max_scan):
                row = df.iloc[i]
                non_null = row.dropna()
                if non_null.size >= 2:
                    header_idx = i
                    break
        if header_idx is not None:
            header_vals = [normalize_header(x) or f"col_{j+1}" for j, x in enumerate(df.iloc[header_idx])]
            df2 = df.iloc[header_idx + 1 :].copy()
            df2.columns = header_vals
            return df2
    return df


def classify_table(mapped_cols: List[str]) -> str | None:
    s = set(mapped_cols)
    wave_hits = {"hs_m", "tp_s", "tz_s"} & s
    wind_hits = {"u10_ms"} & s
    current_hits = {"speed_ms"} & s
    if wave_hits:
        return "waves"
    if wind_hits:
        return "wind"
    if current_hits:
        return "current"
    # secondary heuristics
    if {"return_period_years", "probability_exceedance"} <= s and ("direction_deg" in s):
        return "waves"
    return None


def coerce_numeric(df: pd.DataFrame, cols: List[str]) -> None:
    for c in cols:
        if c in df.columns:
            df[c] = (
                df[c]
                .astype(str)
                .str.replace(",", "", regex=False)
                .str.replace("—", "-", regex=False)
                .str.replace("–", "-", regex=False)
            )
            df[c] = pd.to_numeric(df[c], errors="coerce")


def process() -> Tuple[int, Dict[str, int]]:
    # Prefer shortlist directory if it exists to reduce noise
    use_dir = SHORTLIST_DIR if os.path.isdir(SHORTLIST_DIR) else EXTRACT_DIR
    meta_path = os.path.join(EXTRACT_DIR, "metadata.json")
    if not os.path.exists(meta_path):
        raise FileNotFoundError(f"metadata.json not found in {EXTRACT_DIR}")
    with open(meta_path, "r", encoding="utf-8") as f:
        meta = json.load(f)

    os.makedirs(PROCESSED_DIR, exist_ok=True)

    agg: Dict[str, List[pd.DataFrame]] = {"waves": [], "wind": [], "current": []}

    for t in meta.get("tables", []):
        csv_name = t.get("csv")
        if not csv_name:
            continue
        path = os.path.join(use_dir, csv_name)
        if not os.path.exists(path):
            continue
        try:
            try:
                df = pd.read_csv(path, encoding="utf-8", on_bad_lines="skip")
            except UnicodeDecodeError:
                df = pd.read_csv(path, encoding="latin-1", on_bad_lines="skip")
        except Exception:
            continue

        if df.empty:
            continue

        df = infer_header(df)
        original_cols = list(df.columns)

        # Drop fully empty rows/cols
        df = df.dropna(how="all").dropna(axis=1, how="all")
        if df.empty:
            continue

        mapping = map_to_standard(list(df.columns))
        # Retry with alternate header inference if nothing maps
        if not mapping:
            df_alt = infer_header(df.copy())
            if list(df_alt.columns) != list(df.columns):
                mapping = map_to_standard(list(df_alt.columns))
                df = df_alt
        if not mapping:
            continue
        df = df.rename(columns=mapping)

        category = classify_table(list(df.columns))
        if not category:
            continue

        # Keep only the standard columns for the category, filling missing
        std_cols = STANDARD_COLUMNS[category]
        for c in std_cols:
            if c not in df.columns:
                df[c] = None

        # Coerce numeric fields
        if category == "waves":
            coerce_numeric(df, ["return_period_years", "probability_exceedance", "hs_m", "tp_s", "tz_s", "direction_deg"])
        elif category == "wind":
            coerce_numeric(df, ["return_period_years", "probability_exceedance", "u10_ms", "direction_deg", "height_m"])
        elif category == "current":
            coerce_numeric(df, ["return_period_years", "probability_exceedance", "speed_ms", "direction_deg", "depth_m"])

        # Add source table index for traceability
        df["source_table"] = t.get("index")

        # Keep only standardized columns and drop duplicates
        df = df[std_cols].copy()
        df = df.dropna(how="all")
        if df.empty:
            continue
        agg[category].append(df)

    counts: Dict[str, int] = {}
    for category, frames in agg.items():
        if not frames:
            counts[category] = 0
            continue
        out = pd.concat(frames, ignore_index=True)
        out = out.drop_duplicates()
        # Light cleanup: drop rows without any key metric
        key_cols = {
            "waves": ["hs_m", "tp_s", "tz_s"],
            "wind": ["u10_ms"],
            "current": ["speed_ms"],
        }[category]
        if key_cols:
            out = out.dropna(subset=key_cols, how="all")
        out_path = os.path.join(PROCESSED_DIR, f"api_rp_2met_{category}.csv")
        out.to_csv(out_path, index=False)
        counts[category] = len(out)

    # Write a simple data dictionary
    dictionary = {
        "source": os.path.basename(meta_path),
        "datasets": {
            k: {"columns": STANDARD_COLUMNS[k]} for k in STANDARD_COLUMNS
        },
        "synonyms": SYNONYMS,
    }
    with open(os.path.join(PROCESSED_DIR, "api_rp_2met_dictionary.json"), "w", encoding="utf-8") as f:
        json.dump(dictionary, f, indent=2)

    total = sum(counts.values())
    print(f"Processed rows -> waves: {counts['waves']}, wind: {counts['wind']}, current: {counts['current']} (total {total})")
    return total, counts


if __name__ == "__main__":
    process()
