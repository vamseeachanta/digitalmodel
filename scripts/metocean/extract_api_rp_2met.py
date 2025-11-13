"""
Extract tables from API RP 2MET PDF into CSV/Excel and metadata.

Requirements:
- Java 8+
- Python 3.10+
- pip install tabula-py pandas openpyxl

Usage:
  python scripts/metocean/extract_api_rp_2met.py
"""

import os
import json
from datetime import datetime

import pandas as pd

try:
    import tabula  # type: ignore
except Exception as e:  # pragma: no cover
    raise SystemExit(
        "tabula-py is required. Install with: pip install tabula-py pandas openpyxl"
    ) from e


ROOT = os.path.dirname(os.path.dirname(os.path.dirname(__file__)))
PDF_PATH = os.path.join(
    ROOT,
    "data",
    "metocean",
    "raw",
    "sources",
    "API_RP_2MET_Derivation_of_Metocean_Design_Operating_Conditions_Jan2021.pdf",
)
OUT_DIR = os.path.join(
    ROOT,
    "data",
    "metocean",
    "raw",
    "extracted",
    "api_rp_2met_jan2021",
)


def ensure_dirs() -> None:
    os.makedirs(OUT_DIR, exist_ok=True)


def extract_tables() -> dict:
    if not os.path.exists(PDF_PATH):
        raise FileNotFoundError(f"Source PDF not found: {PDF_PATH}")

    print(f"Reading tables from {PDF_PATH} ...")
    # Lattice and stream modes capture different table styles; try both
    java_opts = ["-Dfile.encoding=UTF-8"]
    # Some Windows setups emit cp1252; be lenient in decode
    tables_lattice = tabula.read_pdf(
        PDF_PATH,
        pages="all",
        multiple_tables=True,
        lattice=True,
        encoding="latin-1",
        java_options=java_opts,
    )
    tables_stream = tabula.read_pdf(
        PDF_PATH,
        pages="all",
        multiple_tables=True,
        stream=True,
        encoding="latin-1",
        java_options=java_opts,
    )

    # Deduplicate by shape + head signature
    def sig(df: pd.DataFrame) -> str:
        head = tuple(map(str, list(df.columns)))
        shape = df.shape
        return f"{shape}-{hash(head)}"

    all_tables = []
    seen = set()
    for mode, coll in (("lattice", tables_lattice), ("stream", tables_stream)):
        for i, df in enumerate(coll):
            if df is None or df.empty:
                continue
            # Clean up generic Unnamed columns and drop fully empty columns
            df = df.rename(columns=lambda c: str(c).strip()).copy()
            df = df.dropna(axis=1, how="all")
            key = sig(df)
            if key in seen:
                continue
            seen.add(key)
            all_tables.append((mode, i, df))

    meta = {
        "source": os.path.basename(PDF_PATH),
        "extracted_at": datetime.utcnow().isoformat() + "Z",
        "tables": [],
    }

    xlsx_path = os.path.join(OUT_DIR, "api_rp_2met_tables.xlsx")
    with pd.ExcelWriter(xlsx_path, engine="openpyxl") as xw:
        for idx, (mode, i, df) in enumerate(all_tables, start=1):
            csv_name = f"table_{idx:03}.csv"
            csv_path = os.path.join(OUT_DIR, csv_name)
            df.to_csv(csv_path, index=False)
            # Limit sheet name length and remove invalid chars
            sheet_name = f"{mode}_{idx:03}"[:31]
            df.to_excel(xw, sheet_name=sheet_name, index=False)
            meta["tables"].append(
                {
                    "index": idx,
                    "mode": mode,
                    "rows": int(df.shape[0]),
                    "cols": int(df.shape[1]),
                    "csv": csv_name,
                    "sheet": sheet_name,
                    "columns": [str(c) for c in df.columns],
                }
            )

    with open(os.path.join(OUT_DIR, "metadata.json"), "w", encoding="utf-8") as f:
        json.dump(meta, f, indent=2)

    print(f"Extracted {len(all_tables)} unique tables -> {OUT_DIR}")
    return meta


def main() -> None:
    ensure_dirs()
    extract_tables()


if __name__ == "__main__":
    main()
