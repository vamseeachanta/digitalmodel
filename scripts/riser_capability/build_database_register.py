"""Generate the Job 1 input register for #3954; --check verifies committed bytes."""
from __future__ import annotations

import csv
import hashlib
import importlib.util
import json
from pathlib import Path
import sys

import pyarrow.parquet as parquet
import yaml

ROOT = Path(__file__).resolve().parents[2]
_spec = importlib.util.spec_from_file_location("register_inventory", Path(__file__).with_name("build_riser_inventory.py"))
inventory = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(inventory)
# Category, repository path, declared selector, count unit. Never store counts.
INPUTS = (
    ("Line pipe and tubulars", "data/line_pipe/data.json", "records", "records"),
    ("Line pipe and tubulars", "data/riser_database/config_catalog.csv", "", "rows"),
    ("Stack-ups and rig interface", "data/riser_database/riser_stackup.csv", "", "rows"),
    ("Stack-ups and rig interface", "data/riser_database/rig_riser_interface.csv", "", "rows"),
    ("Stack-ups and rig interface", "data/drilling_rigs/udw_rig_fleet_external.yml", "rigs", "rigs"),
    ("Stack-ups and rig interface", "data/drilling_rigs/udw_rig_fleet_external.yml", "contracts", "contracts"),
    ("Flexible pipe", "data/flexible_pipe/data.json", "records", "records"),
    ("Structural sections", "data/riser_database/structure_properties/data.json", "records", "records"),
    ("Buoyancy modules", "data/riser_buoyancy/data.json", "records", "records"),
    ("Soil", "data/soil_stiffness/data.json", "records", "records"),
    ("Soil", "data/geotechnical/geotech_database.yaml", "parameters", "parameter rows"),
    ("S-N, SCF, DFF", "data/fatigue/sn_curves.yaml", "curves", "curves"),
    ("S-N, SCF, DFF", "data/riser_database/material_sn_scf_dff.csv", "", "rows"),
    ("Vessel RAOs", "data/rao_database.parquet", "", "rows"),
    ("Metocean", "data/vessels/processed/metocean_criteria.csv", "", "rows"),
    ("Standards crosswalk", "data/riser_database/standards_crosswalk.csv", "", "rows"),
    ("Vessels (drillship basis)", "data/vessels/processed/drillship_basis.json", "dimensions_m", "drillship dimension rows"),
)
DEFERRED = (
    "config/pipe/*.yml", "data/vessels/ (remaining members)",
    "docs/domains/orcaflex/library/line_types/", "docs/domains/orcaflex/library/buoy_types/",
    "src/digitalmodel/fatigue/ (including __init__.py)", "src/digitalmodel/materials/line_pipe.py",
)
OUTPUT = "docs/domains/risers/riser-input-database-register"


def input_file(root: Path, relative: str) -> Path:
    path = root / relative
    if not path.resolve().is_relative_to(root.resolve()):
        raise ValueError(f"{relative}: path resolves outside the repository")
    if not path.is_file():
        raise ValueError(f"{relative}: required input is missing")
    return path


def read_json(root: Path, relative: str) -> dict:
    return json.loads(input_file(root, relative).read_text(encoding="utf-8"))


def array(value, name: str) -> list:
    if not isinstance(value, list):
        raise ValueError(f"{name} must be an array")
    return value


def count_input(root: Path, relative: str, selector: str) -> tuple[int, str, str]:
    """Return computed count, detected provenance mechanism and population."""
    path = input_file(root, relative)
    provenance, records = "none", None
    if path.suffix == ".csv":
        count = inventory.record_count(path)
        with path.open(encoding="utf-8", newline="") as stream:
            fields = next(csv.reader(stream))
        provenance = ", ".join(k for k in ("source_path", "wiki_path", "code_id", "n_citations", "registry_revision") if k in fields) or "none"
    elif selector == "records":
        records = array(read_json(root, relative)["records"], selector)
        count = inventory.record_count(path)
        schema = read_json(root, str(Path(relative).with_name("schema.json")))
        sources = yaml.safe_load(input_file(root, str(Path(relative).with_name("sources.yml"))).read_text(encoding="utf-8"))
        fields = schema["properties"]["records"]["items"]["properties"]
        if "citation_id" in fields and "citations" in sources:
            provenance = "citation_id per row resolved in sources.yml"
    elif selector == "dimensions_m":
        data = read_json(root, relative)
        count = len(array(data[selector], selector))
        width = len(array(data["columns"], "columns"))
        for row in data[selector]:
            array(row, "dimension row")
            if len(row) != width:
                raise ValueError(f"{relative}: row width {len(row)} differs from columns width {width}")
        if count != data["row_count"]:
            raise ValueError(f"{relative}: computed count {count} differs from declared row_count {data['row_count']}")
        if all(k in data.get("source", {}) for k in ("repository", "file", "revision", "sha256")):
            provenance = "source block (repository, file, revision, SHA-256)"
    elif path.suffix == ".parquet":
        manifest = read_json(root, "data/rao_database.manifest.json")
        for field, actual in (("database_bytes", path.stat().st_size),
                              ("database_sha256", hashlib.sha256(path.read_bytes()).hexdigest())):
            if manifest[field] != actual:
                raise ValueError(f"{relative}: manifest {field} mismatch")
        count = parquet.read_metadata(path).num_rows
        if any(entry.get("sources") for entry in manifest["entries"]):
            provenance = "sources in rao_database.manifest.json entries"
    else:
        data = yaml.safe_load(path.read_text(encoding="utf-8"))
        if selector == "curves":
            count = sum(len(s.get("curves", {})) for s in data["standards"].values())
            count += len(data["standards"]["DNV"]["multislope"])
            if data.get("metadata", {}).get("source"):
                provenance = "file-level metadata.source only"
        else:
            selected = array(data[selector], selector)
            count = len(selected)
            provenance = ", ".join(k for k in ("source_id", "locator", "source_keys") if any(k in r for r in selected)) or "none"
            if selector == "rigs" and data.get("sources"):
                provenance = "file-level sources list"
    status = inventory.population(count).split(" ", 1)[0]
    if records and all(row.get("status") == "synthetic" for row in records):
        status = "seeded"
    return count, provenance, status


def build_register(root: Path = ROOT) -> dict:
    rows = []
    for category, relative, selector, unit in INPUTS:
        try:
            count, provenance, status = count_input(root, relative, selector)
        except (KeyError, TypeError, ValueError) as error:
            raise ValueError(f"{relative}: {error}") from error
        rows.append(dict(category=category, path=relative, format=Path(relative).suffix[1:],
                         selector=selector, count=count, unit=unit, provenance=provenance, status=status, issue=None))
    source = input_file(root, "data/riser_capability/riser_types.yml")
    for absent in yaml.safe_load(source.read_text(encoding="utf-8"))["absent_database_categories"]:
        rows.append(dict(category=absent["category"], path=None, format=None, selector=None,
                         count=None, unit=None, provenance="none", status="absent", issue=absent["issue"]))
    return {"not_yet_counted": sorted(DEFERRED), "rows": rows}


def render(register: dict) -> dict[str, bytes]:
    lines = ["# Riser input-database register", "", "Part of #3954: Job 1 only.", "",
             "Generated by `scripts/riser_capability/build_database_register.py`; counts are recomputed.",
             "Population only: engineering qualification, standards verification and source redistribution rights",
             "are not established. Empty means zero records; seeded means all catalogue rows are synthetic;",
             "populated means nonzero records otherwise. Absent categories come from the capability inventory.", "",
             "| Category | Path | Format | Count | Unit | Status | Provenance mechanism | Open issue |",
             "| --- | --- | --- | --- | --- | --- | --- | --- |"]
    for row in register["rows"]:
        issue = f"[#{row['issue']}](https://github.com/vamseeachanta/digitalmodel/issues/{row['issue']})" if row["issue"] else "none"
        cells = [row["category"], f"`{row['path']}`" if row["path"] else "none", row["format"] or "-",
                 str(row["count"]) if row["count"] is not None else "-", row["unit"] or "-",
                 row["status"], row["provenance"], issue]
        lines.append("| " + " | ".join(cells) + " |")
    lines += ["", "Table 1. Computed counts and detected provenance mechanisms for Job 1 inputs.", "",
              "`data/drilling_rigs/README.md` is excluded as metadata.", "",
              "## Not yet counted (second slice of #3954)", ""]
    lines += [f"- `{path}`" for path in register["not_yet_counted"]]
    lines += ["", "## Reproduce", "", "```bash", "export MPLCONFIGDIR=$(mktemp -d)",
              "PYTHONPATH=src uv run python scripts/riser_capability/build_database_register.py",
              "PYTHONPATH=src uv run python scripts/riser_capability/build_database_register.py --check",
              'PYTHONPATH=src uv run python -m pytest tests/riser_capability/test_database_register.py -q -m "not solver"',
              "```", "", "## Run-it-yourself prompt", "",
              "Regenerate the Job 1 register, independently recount its inputs, and run the commands above.",
              "Keep the deferred roots explicit; report population without an engineering qualification claim.", ""]
    encoded = ['{', '"not_yet_counted": ' + json.dumps(register["not_yet_counted"]) + ',', '"rows": [']
    encoded += [json.dumps(row, sort_keys=True, separators=(",", ":")) + ("," if i < len(register["rows"])-1 else "")
                for i, row in enumerate(register["rows"])]
    encoded += ["]", "}", ""]
    return {OUTPUT + ".md": "\n".join(lines).encode(), OUTPUT + ".json": "\n".join(encoded).encode()}


def main(argv: list[str], root: Path = ROOT) -> int:
    outputs = render(build_register(root))  # Validate every input before replacing outputs.
    for name in outputs:
        if not (root / name).resolve().is_relative_to(root.resolve()):
            raise ValueError(f"{name}: path resolves outside the repository")
    if "--check" in argv:
        stale = [name for name, content in outputs.items() if not (root / name).is_file() or (root / name).read_bytes() != content]
        for name in stale:
            sys.stderr.write(f"{name}: out of date\n")
        return int(bool(stale))
    for name, content in outputs.items():
        path = root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(content)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
