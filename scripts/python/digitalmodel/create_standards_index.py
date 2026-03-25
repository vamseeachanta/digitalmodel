import json
from pathlib import Path

INPUT_FILE = "digitalmodel/data/legacy_assets_index.json"
OUTPUT_FILE = "digitalmodel/data/standards/index.json"

def main():
    print(f"Loading full index from {INPUT_FILE}...")
    try:
        with open(INPUT_FILE, "r") as f:
            all_assets = json.load(f)
    except FileNotFoundError:
        print(f"Error: {INPUT_FILE} not found. Run index_assets.py first.")
        return

    print(f"Total assets: {len(all_assets)}")
    
    standards = [a for a in all_assets if a.get("type") == "Standard"]
    
    print(f"Found {len(standards)} standards.")
    
    # Ensure output directory exists
    Path(OUTPUT_FILE).parent.mkdir(parents=True, exist_ok=True)
    
    with open(OUTPUT_FILE, "w") as f:
        json.dump(standards, f, indent=2)
        
    print(f"Standards index saved to {OUTPUT_FILE}")

if __name__ == "__main__":
    main()
