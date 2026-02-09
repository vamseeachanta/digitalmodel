"""Capture OrcaFlex model views and extract along-length statics data for SME review."""

import json
import os
import sys
from pathlib import Path

import OrcFxAPI as ofx

BASE = Path(__file__).resolve().parent.parent
LIBRARY = BASE / "docs" / "modules" / "orcaflex" / "library" / "tier2_fast"
OUT = BASE / "benchmark_output" / "sme_review_assets"
OUT.mkdir(parents=True, exist_ok=True)

MODELS = [
    "a01_catenary_riser",
    "a01_lazy_wave_riser",
    "a01_pliant_wave_riser",
    "a01_steep_wave_riser",
]

RESULT_NAMES = [
    "Effective tension",
    "Bend moment",
    "x",
    "z",
]


def load_and_solve(path: Path) -> ofx.Model:
    """Load a model and run statics."""
    m = ofx.Model()
    m.LoadData(str(path))
    m.CalculateStatics()
    return m


def get_lines(model: ofx.Model) -> list:
    """Get all Line objects from a model."""
    return [obj for obj in model.objects if obj.type == ofx.ObjectType.Line]


def extract_range_data(line, result_name: str) -> dict:
    """Extract along-length static result using StaticResult per node."""
    try:
        arcs = line.NodeArclengths
        values = []
        for a in arcs:
            oe = ofx.oeArcLength(float(a))
            val = line.StaticResult(result_name, oe)
            values.append(float(val))
        return {
            "arc_length": [float(a) for a in arcs],
            "values": values,
        }
    except Exception as e:
        print(f"    Warning: Could not extract '{result_name}' for {line.Name}: {e}")
        return None


def save_model_view(model: ofx.Model, filepath: str):
    """Save a 3D model view as BMP."""
    try:
        vp = model.defaultViewParameters
        vp.ViewSize = 300.0
        model.SaveModelView(str(filepath), vp)
        print(f"  Saved view: {filepath}")
    except Exception as e:
        print(f"  Warning: SaveModelView failed: {e}")
        # Try without custom params
        try:
            model.SaveModelView(str(filepath))
            print(f"  Saved view (default params): {filepath}")
        except Exception as e2:
            print(f"  ERROR: Could not save view: {e2}")


def process_model(model_name: str) -> dict:
    """Process a single riser model - capture views and extract data."""
    print(f"\n{'='*60}")
    print(f"Processing: {model_name}")
    print(f"{'='*60}")

    model_dir = LIBRARY / model_name
    mono_dir = model_dir / "monolithic"
    mod_dir = model_dir / "modular"

    result = {"model_name": model_name, "monolithic": {}, "modular": {}}

    # Find monolithic .dat file
    dat_files = list(mono_dir.glob("*.dat"))
    if not dat_files:
        print(f"  ERROR: No .dat file in {mono_dir}")
        return result

    # --- Monolithic ---
    print(f"  Loading monolithic: {dat_files[0].name}")
    try:
        m_mono = load_and_solve(dat_files[0])
        print(f"  Statics converged (monolithic)")

        # Save view
        view_path = OUT / f"{model_name}_monolithic.bmp"
        save_model_view(m_mono, str(view_path))

        # Extract along-length data
        lines = get_lines(m_mono)
        print(f"  Lines found: {[l.Name for l in lines]}")
        for line in lines:
            line_data = {}
            for rn in RESULT_NAMES:
                rd = extract_range_data(line, rn)
                if rd:
                    line_data[rn] = rd
            if line_data:
                result["monolithic"][line.Name] = line_data

    except Exception as e:
        print(f"  ERROR (monolithic): {e}")

    # --- Modular ---
    master = mod_dir / "master.yml"
    if master.exists():
        print(f"  Loading modular: master.yml")
        try:
            m_mod = load_and_solve(master)
            print(f"  Statics converged (modular)")

            # Save view
            view_path = OUT / f"{model_name}_modular.bmp"
            save_model_view(m_mod, str(view_path))

            # Extract along-length data
            lines = get_lines(m_mod)
            print(f"  Lines found: {[l.Name for l in lines]}")
            for line in lines:
                line_data = {}
                for rn in RESULT_NAMES:
                    rd = extract_range_data(line, rn)
                    if rd:
                        line_data[rn] = rd
                if line_data:
                    result["modular"][line.Name] = line_data

        except Exception as e:
            print(f"  ERROR (modular): {e}")
    else:
        print(f"  WARNING: No modular master.yml found")

    return result


def main():
    all_results = {}
    for model_name in MODELS:
        data = process_model(model_name)
        all_results[model_name] = data

    # Save combined JSON
    json_path = OUT / "range_data.json"
    with open(json_path, "w") as f:
        json.dump(all_results, f, indent=2)
    print(f"\nSaved range data: {json_path}")

    # List generated files
    print(f"\nGenerated files in {OUT}:")
    for f in sorted(OUT.iterdir()):
        size = f.stat().st_size
        print(f"  {f.name} ({size:,} bytes)")


if __name__ == "__main__":
    main()
