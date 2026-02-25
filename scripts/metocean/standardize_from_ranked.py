"""
Semi-automatic standardization from ranked shortlist.
Handles clear cases for wind and waves where columns are obvious.

Rules (conservative):
- Wind: must contain a column with m/s values and tokens {wind or 10 m};
         must have a return period column (header contains 'return' or 'years').
- Waves: must contain a column matching hs (m) and optionally tp (s)/tz (s);
         must have a return period column.

Outputs appended rows to processed CSVs.
"""

from __future__ import annotations

import os
import re
import json
from typing import Dict, List, Tuple

import pandas as pd


ROOT = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
EXTRACT_DIR = os.path.join(
    ROOT, "data", "metocean", "raw", "extracted", "api_rp_2met_jan2021_shortlist"
)
RANK_PATH = os.path.join(EXTRACT_DIR, "shortlist_ranking.csv")
PROCESSED_DIR = os.path.join(ROOT, "data", "metocean", "processed")


def normalize(s: str) -> str:
    s = str(s or "").lower().strip()
    s = s.replace("\n", " ")
    s = re.sub(r"[^a-z0-9%/\. ]+", " ", s)
    s = re.sub(r"\s+", " ", s)
    return s


def infer_header(df: pd.DataFrame) -> pd.DataFrame:
    if all(str(c).startswith("Unnamed") or str(c).strip() == "" for c in df.columns):
        for i in range(min(6, len(df))):
            row = [normalize(x) for x in df.iloc[i].tolist()]
            if sum(bool(x) for x in row) >= 2:
                new_cols = [x if x else f"col_{j+1}" for j, x in enumerate(row)]
                out = df.iloc[i + 1 :].copy()
                out.columns = new_cols
                return out
    return df


def find_return_period_col(cols: List[str]) -> str | None:
    for c in cols:
        nc = normalize(c)
        if "return" in nc or "years" in nc or nc in {"rp", "return period"}:
            return c
    return None


def find_numeric_cols(df: pd.DataFrame) -> List[str]:
    cand = []
    for c in df.columns:
        try:
            ser = pd.to_numeric(df[c], errors="coerce")
            # Require at least 3 numeric entries
            if ser.notna().sum() >= 3:
                cand.append(c)
        except Exception:
            continue
    return cand


def guess_units_token(col: str) -> str:
    nc = normalize(col)
    if "m/s" in nc or " m s" in nc:
        return "ms"
    if "(m)" in nc or nc.endswith(" m"):
        return "m"
    if "(s)" in nc or nc.endswith(" s"):
        return "s"
    return ""


def process_wind(df: pd.DataFrame, src: str) -> pd.DataFrame | None:
    rp_col = find_return_period_col(list(df.columns))
    blob = " ".join(normalize(x) for x in list(df.columns))
    if not rp_col or not ("wind" in blob or "10 m" in blob or "10m" in blob):
        return None
    num_cols = find_numeric_cols(df.drop(columns=[rp_col], errors="ignore"))
    # Prefer a single clear m/s column
    ms_cols = [c for c in num_cols if guess_units_token(c) == "ms"]
    cand = ms_cols or num_cols
    if not cand:
        return None
    speed_col = cand[0]
    out = pd.DataFrame({
        "region": None,
        "location": None,
        "return_period_years": pd.to_numeric(df[rp_col], errors="coerce"),
        "probability_exceedance": None,
        "u10_ms": pd.to_numeric(df[speed_col], errors="coerce"),
        "direction_deg": None,
        "height_m": 10 if ("10 m" in blob or "10m" in blob) else None,
        "source_table": src,
    })
    out = out.dropna(subset=["return_period_years", "u10_ms"], how="any")
    return out if not out.empty else None


def process_waves(df: pd.DataFrame, src: str) -> pd.DataFrame | None:
    rp_col = find_return_period_col(list(df.columns))
    blob = " ".join(normalize(x) for x in list(df.columns))
    if not rp_col or not ("hs" in blob or "significant wave" in blob or "wave height" in blob):
        return None
    num_cols = find_numeric_cols(df.drop(columns=[rp_col], errors="ignore"))
    # Choose hs as a column with meters
    hs_cols = [c for c in num_cols if guess_units_token(c) == "m" or "hs" in normalize(c)]
    if not hs_cols:
        return None
    hs_col = hs_cols[0]
    tp_cols = [c for c in num_cols if c != hs_col and ("tp" in normalize(c) or guess_units_token(c) == "s")]
    out = pd.DataFrame({
        "region": None,
        "location": None,
        "return_period_years": pd.to_numeric(df[rp_col], errors="coerce"),
        "probability_exceedance": None,
        "hs_m": pd.to_numeric(df[hs_col], errors="coerce"),
        "tp_s": pd.to_numeric(df[tp_cols[0]], errors="coerce") if tp_cols else None,
        "tz_s": None,
        "direction_deg": None,
        "source_table": src,
    })
    out = out.dropna(subset=["return_period_years", "hs_m"], how="any")
    return out if not out.empty else None


def parse_range_to_number(s: str) -> float | None:
    t = normalize(s)
    # patterns like "10 to 11", "10to11", "10-11"
    m = re.findall(r"(\d+(?:\.\d+)?)", t)
    if not m:
        return None
    try:
        nums = [float(x) for x in m]
        return max(nums) if nums else None
    except Exception:
        return None


def process_matrix(df: pd.DataFrame, src: str) -> Tuple[pd.DataFrame | None, pd.DataFrame | None, pd.DataFrame | None]:
    # Look for a row containing Return Period; next row should be RP headings
    rp_row = None
    for i in range(min(12, len(df))):
        row_norm = [normalize(x) for x in df.iloc[i].tolist()]
        if any("return" in x and "year" in x for x in row_norm):
            rp_row = i
            break
    if rp_row is None or rp_row + 1 >= len(df):
        return None, None, None
    rp_vals_raw = [str(x) for x in df.iloc[rp_row + 1].tolist()]
    # Gather return period numeric values and their column indices
    # Support headers that pack multiple RPs in one cell (e.g., "100 50 10")
    rp_cols: List[Tuple[int, float]] = []
    rp_multi: Dict[int, List[float]] = {}
    for j, v in enumerate(rp_vals_raw):
        nums = []
        for tok in re.findall(r"\d+(?:\.\d+)?", str(v)):
            try:
                nums.append(float(tok))
            except Exception:
                pass
        nums = [x for x in nums if x in (1.0, 1, 2, 5, 10, 25, 50, 100)]
        if not nums:
            continue
        if len(nums) == 1:
            rp_cols.append((j, float(nums[0])))
        else:
            rp_multi[j] = [float(x) for x in nums]
    if not rp_cols:
        return None, None, None

    # Identify parameter name column
    param_col = 0
    for j, v in enumerate(df.columns):
        if "metocean" in normalize(v) and "parameter" in normalize(v):
            param_col = j
            break

    waves_rows = []
    wind_rows = []
    current_rows = []

    # Try to capture region by scanning a window above header across multiple columns
    region = None
    candidates = []
    for k in range(max(0, rp_row - 3), rp_row + 1):
        for j in range(min(6, len(df.columns))):
            cell = str(df.iloc[k, j])
            c = normalize(cell)
            if not c:
                continue
            if any(tok in c for tok in ["metocean", "parameter", "return", "year", "table", "annex"]):
                continue
            candidates.append(cell.strip())
    if candidates:
        region = max(candidates, key=lambda s: len(s))

    # Iterate data rows after rp_row+2
    for i in range(rp_row + 2, len(df)):
        label = str(df.iloc[i, param_col]) if param_col < len(df.columns) else ""
        labn = normalize(label)
        if not labn:
            continue
        is_wind_10m = "wind" in labn or "10 min" in labn or "10 m" in labn
        is_gust = "gust" in labn
        is_hs = "significant" in labn or "wave height" in labn or labn.startswith("hs")
        is_tp = "peak" in labn or labn.startswith("tp")
        is_current = "current" in labn or "surface" in labn or "mid-depth" in labn or "near-bottom" in labn

        for j, rp in rp_cols:
            if j >= len(df.columns):
                continue
            val_raw = df.iloc[i, j]
            if pd.isna(val_raw):
                continue
            val = parse_range_to_number(str(val_raw))
            if val is None:
                # try numeric coercion
                try:
                    val = float(str(val_raw).strip())
                except Exception:
                    continue
            if is_hs:
                waves_rows.append({
                    "region": region,
                    "location": None,
                    "return_period_years": rp,
                    "probability_exceedance": None,
                    "hs_m": val,
                    "tp_s": None,
                    "tz_s": None,
                    "direction_deg": None,
                    "source_table": src,
                })
        # Handle multi-RP packed header cells by splitting values within the cell
        for j, rp_list in rp_multi.items():
            if j >= len(df.columns):
                continue
            val_raw = str(df.iloc[i, j])
            parts = re.findall(r"\d+(?:\.\d+)?(?:\s*to\s*\d+(?:\.\d+)?)?", normalize(val_raw))
            # Fallback: extract plain numbers
            if not parts:
                nums = [parse_range_to_number(val_raw)] * len(rp_list)
            else:
                nums = [parse_range_to_number(p) for p in parts]
            # Align lengths
            nums = [x for x in nums if x is not None]
            for k, rp in enumerate(rp_list[: len(nums)]):
                val = nums[k]
                if val is None:
                    continue
                if is_hs:
                    waves_rows.append({
                        "region": region,
                        "location": None,
                        "return_period_years": rp,
                        "probability_exceedance": None,
                        "hs_m": val,
                        "tp_s": None,
                        "tz_s": None,
                        "direction_deg": None,
                        "source_table": src,
                    })
                elif is_tp:
                    waves_rows.append({
                        "region": region,
                        "location": None,
                        "return_period_years": rp,
                        "probability_exceedance": None,
                        "hs_m": None,
                        "tp_s": val,
                        "tz_s": None,
                        "direction_deg": None,
                        "source_table": src,
                    })
                elif is_wind_10m and not is_gust:
                    wind_rows.append({
                        "region": region,
                        "location": None,
                        "return_period_years": rp,
                        "probability_exceedance": None,
                        "u10_ms": val,
                        "direction_deg": None,
                        "height_m": 10,
                        "source_table": src,
                    })
                elif is_current:
                    current_rows.append({
                        "region": region,
                        "location": None,
                        "return_period_years": rp,
                        "probability_exceedance": None,
                        "speed_ms": val,
                        "direction_deg": None,
                        "depth_m": 0.0 if "surface" in labn else None,
                        "source_table": src,
                    })
            

    wr = pd.DataFrame(wind_rows) if wind_rows else None
    wa = pd.DataFrame(waves_rows) if waves_rows else None
    cr = pd.DataFrame(current_rows) if current_rows else None
    return wr, wa, cr


def main() -> None:
    os.makedirs(PROCESSED_DIR, exist_ok=True)
    if not os.path.exists(RANK_PATH):
        print("Ranking file not found; run rank_metocean_tables.py first.")
        return
    ranking = pd.read_csv(RANK_PATH)

    wind_rows = []
    wave_rows = []
    current_rows = []

    for _, row in ranking.iterrows():
        csv_name = row["csv"]
        path = os.path.join(EXTRACT_DIR, csv_name)
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
        df = df.dropna(how="all").dropna(axis=1, how="all")
        if df.empty:
            continue
        # Matrix-style tables
        mw, mwa, mcr = process_matrix(df, src=csv_name)
        if mw is not None:
            wind_rows.append(mw)
        if mwa is not None:
            wave_rows.append(mwa)
        if mcr is not None:
            current_rows.append(mcr)
        # Also attempt simple header-based extraction
        w = process_wind(df, src=csv_name)
        if w is not None:
            wind_rows.append(w)
        wa = process_waves(df, src=csv_name)
        if wa is not None:
            wave_rows.append(wa)

    if wind_rows:
        wind = pd.concat(wind_rows, ignore_index=True).drop_duplicates()
        wind.to_csv(os.path.join(PROCESSED_DIR, "api_rp_2met_wind.csv"), index=False)
        print(f"WIND rows: {len(wind)}")
    else:
        print("WIND rows: 0")

    if wave_rows:
        waves = pd.concat(wave_rows, ignore_index=True).drop_duplicates()
        waves.to_csv(os.path.join(PROCESSED_DIR, "api_rp_2met_waves.csv"), index=False)
        print(f"WAVES rows: {len(waves)}")
    else:
        print("WAVES rows: 0")

    if current_rows:
        current = pd.concat(current_rows, ignore_index=True).drop_duplicates()
        current.to_csv(os.path.join(PROCESSED_DIR, "api_rp_2met_current.csv"), index=False)
        print(f"CURRENT rows: {len(current)}")
    else:
        print("CURRENT rows: 0")


if __name__ == "__main__":
    main()
