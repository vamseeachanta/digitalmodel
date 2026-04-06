"""Jumper Installation Analysis Pipeline.

End-to-end workflow:
  1. spec.yml (declarative input) 
  2. jumper_lift.py (calculate jumper properties from Excel conversion)
  3. orcaflex model.yml (generate line-section config for model generator)
  4. OrcaFlex analysis (run solver)
  5. Post-processing (extract results, generate reports)
  6. Output delivery (PDF/HTML reports, data tables, Go/No-Go decisions)

Supports multiple jumper configurations from a single spec:
  • SZ (static) – Saipem crane, no active heave compensation
  • DZ + AHCon – DZ configuration with active heave compensation ON
  • DZ + AHCoff – DZ configuration with active heave compensation OFF

References:
    DNV-RP-H103 (2011) – Modelling and Analysis of Marine Operations
    DNV-ST-N001 (2021) – Marine Operations and Marine Warranty
"""
from __future__ import annotations

import json
import datetime
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional

try:
    import yaml
    HAS_YAML = True
except ImportError:
    HAS_YAML = False

from digitalmodel.marine_ops.installation.jumper_lift import (
    JumperConfig,
    KNOWN_JUMPER_CONFIGS,
    run_jumper_analysis,
    generate_orcaflex_line_sections_yaml,
)


@dataclass
class PipelineConfig:
    """Configuration for the full jumper analysis pipeline."""
    spec_path: str
    output_dir: str = "./output"
    run_orcaflex: bool = False
    generate_report: bool = True
    go_no_go_criteria: Optional[Dict[str, float]] = None


@dataclass
class PipelineOutput:
    """Results from the complete pipeline."""
    config_name: str
    timestamp: str = field(default_factory=lambda: datetime.datetime.now().isoformat())

    # Stage 1: spec.yml
    spec_data: Dict[str, Any] = field(default_factory=dict)

    # Stage 2: calculations
    pipe_properties: Dict[str, float] = field(default_factory=dict)
    pipe_geometry: Dict[str, float] = field(default_factory=dict)
    orcaflex_sections: List[Dict] = field(default_factory=list)
    weight_tally: Dict[str, Any] = field(default_factory=dict)
    crane_utilisation: Dict[str, float] = field(default_factory=dict)

    # Stage 3: YAML generation
    line_sections_yaml: str = ""

    # Stage 6: outputs
    report_path: str = ""
    go_no_go: str = "pending"


def load_spec(spec_path: str) -> Dict:
    """Load spec.yml and return as dict.
    
    Stage 1: Reads the declarative jumper installation spec.
    
    Args:
        spec_path: Path to spec.yml file.
    
    Returns:
        Parsed yaml spec data.
    """
    path = Path(spec_path)
    if not path.exists():
        raise FileNotFoundError(f"Spec file not found: {spec_path}")
    
    if HAS_YAML:
        with open(path) as f:
            return yaml.safe_load(f)
    else:
        raise ImportError("PyYAML required to load spec.yml: pip install pyyaml")


def parse_jumper_config(spec: Dict) -> JumperConfig:
    """Convert spec.yml jumper section to JumperConfig.
    
    Args:
        spec: Parsed spec.yml data.
    
    Returns:
        JumperConfig with values from spec (falling back to known configs).
    """
    jumper_data = spec.get("jumper", {})
    config_name = jumper_data.get("config_name", "ballymore_mf_plet")
    
    # Start from known config
    base = KNOWN_JUMPER_CONFIGS.get(config_name, JumperConfig())
    
    # Override with spec values
    overrides = {k: v for k, v in jumper_data.items() if k != "config_name" and hasattr(base, k)}
    
    # Replace fields
    from dataclasses import replace
    return replace(base, **overrides)


def stage_1_load_spec(spec_path: str) -> Dict:
    """Stage 1: Load and validate spec.yml."""
    spec = load_spec(spec_path)
    
    # Basic validation
    required = {"metadata", "pipe", "environment"}
    missing = required - set(spec.keys())
    if missing:
        raise ValueError(f"Spec missing required sections: {missing}")
    
    return spec


def stage_2_calculate(spec: Dict) -> Dict:
    """Stage 2: Run jumper_lift.py calculations.
    
    Takes parsed spec, creates JumperConfig, runs all calculation
    functions (24 functions from windows conversion), returns results.
    
    This is where the Excel-to-code conversion is exercised.
    """
    config = parse_jumper_config(spec)
    return run_jumper_analysis(config)


def stage_3_generate_yaml(results: Dict) -> str:
    """Stage 3: Generate OrcaFlex line-section YAML from calculation results."""
    return results.get("orcaflex_sections_yaml", "")


def stage_4_report(results: Dict, spec: Dict, yaml_sections: str) -> str:
    """Generate a summary report dict (to be rendered as HTML/PDF externally)."""
    pipe = results.get("pipe_properties", {})
    geom = results.get("pipe_geometry", {})
    wc = results.get("weight_check", {})
    crane = results.get("crane", {})
    
    report = {
        "jumper_name": spec.get("metadata", {}).get("name", "unknown"),
        "total_length_m": geom.get("total_length_m", 0) if isinstance(geom, dict) else 0,
        "pipe_od_mm": pipe.get("od_m", 0) * 1000 if isinstance(pipe, dict) else 0,
        "steel_linear_mass": pipe.get("steel_linear_wt_kg_m", 0) if isinstance(pipe, dict) else 0,
        "buoyancy_dry_weight_kg": results.get("buoyancy", {}).get("dry_weight_kg", 0) if isinstance(results.get("buoyancy", {}), dict) else 0,
        "strake_dry_weight_kg": results.get("strake", {}).get("dry_weight_kg", 0) if isinstance(results.get("strake", {}), dict) else 0,
        "grand_total_kg": wc.get("grand_total_kg", 0) if isinstance(wc, dict) else 0,
        "crane_configs": list(crane.get("cranes", {}).keys()) if isinstance(crane.get("cranes", {}), dict) else [],
        "orcaflex_section_count": len(results.get("orcaflex_sections", [])),
    }
    return json.dumps(report, indent=2)


def run_pipeline(spec_path: str, output_dir: str = "./output") -> PipelineOutput:
    """Execute the full jumper analysis pipeline.
    
    Stages:
      1. Load spec.yml
      2. Calculate jumper properties (jumper_lift.py – 24 functions)
      3. Generate OrcaFlex line-section YAML
      4. Produce summary output
    
    Args:
        spec_path: Path to jumper spec.yml (either ballymore_mf_plet or ballymore_plet_plem)
        output_dir: Directory for output files
    
    Returns:
        PipelineOutput with all results.
    """
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    output = PipelineOutput(config_name=Path(spec_path).parent.name)
    
    # Stage 1: Load spec
    output.spec_data = stage_1_load_spec(spec_path)
    
    # Stage 2: Calculate
    results = stage_2_calculate(output.spec_data)
    
    # Extract key values (handle dataclass and dict results)
    if hasattr(results, "pipe_properties"):
        p = results.pipe_properties
        output.pipe_properties = {
            "od_m": p.OD_m if hasattr(p, "OD_m") else getattr(p, "od_m", 0),
            "id_m": p.ID_m if hasattr(p, "ID_m") else getattr(p, "id_m", 0),
            "steel_linear_mass_kg_m": p.steel_linear_mass_kg_m if hasattr(p, "steel_linear_mass_kg_m") else p.steel_linear_wt_kg_m,
        }
    
    # Stage 3: Generate YAML
    if hasattr(results, "orcaflex_sections_yaml"):
        output.line_sections_yaml = results.orcaflex_sections_yaml
        output.orcaflex_sections = results.orcaflex_sections if hasattr(results, "orcaflex_sections") else []
    
    # Write sections YAML to file
    if output.line_sections_yaml:
        yaml_path = Path(output_dir) / "line_sections.yml"
        with open(yaml_path, "w") as f:
            f.write(output.line_sections_yaml)
    
    # Stage 4: Report
    report = stage_4_report(results, output.spec_data, output.line_sections_yaml)
    report_path = Path(output_dir) / "report.json"
    with open(report_path, "w") as f:
        f.write(report)
    output.report_path = str(report_path)
    
    return output
