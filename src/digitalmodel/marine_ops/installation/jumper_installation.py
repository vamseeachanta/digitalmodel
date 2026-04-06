"""Jumper Installation Analysis Pipeline.

End-to-end workflow:
  1. spec.yml (declarative input) 
  2. jumper_lift.py (calculate jumper properties from Excel conversion)
  3. go_no_go.py (DNV-compliant Go/No-Go decision)
  4. orcaflex model.yml (generate line-section config for model generator)
  5. OrcaFlex analysis (run solver)
  6. Post-processing (extract results, generate reports)
  7. Output delivery (PDF/HTML reports, data tables, Go/No-Go decisions)

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

from digitalmodel.marine_ops.installation.go_no_go import (
    GoNoGoDecision,
    evaluate_go_no_go,
    print_decision,
)


@dataclass
class PipelineConfig:
    """Configuration for the full jumper analysis pipeline.
    
    Attributes:
        spec_path: Path to spec.yml input file.
        output_dir: Directory for all output files.
        run_orcaflex: Whether to run OrcaFlex solver (requires installation).
        generate_report: Whether to generate HTML/PDF reports.
        run_go_no_go: Whether to evaluate Go/No-Go decision criteria.
        go_no_go_criteria: Custom criteria overrides (optional).
    """
    spec_path: str
    output_dir: str = "./output"
    run_orcaflex: bool = False
    generate_report: bool = True
    run_go_no_go: bool = True
    go_no_go_criteria: Optional[Dict[str, float]] = None


@dataclass
class PipelineOutput:
    """Results from the complete pipeline.
    
    Attributes:
        config_name: Name of jumper configuration used.
        timestamp: Time pipeline was executed.
        jumper_analysis: Results from jumper_lift.run_jumper_analysis().
        go_no_go: Go/No-Go decision (or None if not evaluated).
        line_sections_yaml: OrcaFlex line section YAML generation.
        report_path: Path to generated report file.
        output_files: List of all output files generated.
    """
    config_name: str
    timestamp: str = field(default_factory=lambda: datetime.datetime.now().isoformat())

    # Stage 2: jumper analysis results
    jumper_analysis: Optional[Dict[str, Any]] = None

    # Stage 3: Go/No-Go decision
    go_no_go: Optional[GoNoGoDecision] = None

    # Stage 4: OrcaFlex line section YAML
    line_sections_yaml: str = ""

    # Stage 6: Report and outputs
    report_path: str = ""
    output_files: List[str] = field(default_factory=list)


def load_spec(spec_path: str) -> Dict:
    """Load spec.yml and return as dict.
    
    Stage 1: Reads the declarative jumper installation spec.
    
    Args:
        spec_path: Path to spec.yml file.
    
    Returns:
        Parsed yaml spec data.
    
    Raises:
        FileNotFoundError: If spec file doesn't exist.
        ImportError: If PyYAML not installed.
    """
    path = Path(spec_path)
    if not path.exists():
        raise FileNotFoundError(f"Spec file not found: {spec_path}")
    
    if HAS_YAML:
        with open(path) as f:
            return yaml.safe_load(f)
    else:
        raise ImportError("PyYAML required: pip install pyyaml")


def parse_jumper_config(spec: Dict) -> JumperConfig:
    """Convert spec.yml jumper section to JumperConfig.
    
    Args:
        spec: Parsed spec.yml data.
    
    Returns:
        JumperConfig with values from spec, falling back to KNOWN_JUMPER_CONFIGS.
    """
    jumper_data = spec.get("jumper", {})
    config_name = jumper_data.get("config_name", "ballymore_mf_plet")
    
    base = KNOWN_JUMPER_CONFIGS.get(config_name, JumperConfig())
    overrides = {
        k: v for k, v in jumper_data.items() 
        if k != "config_name" and hasattr(base, k)
    }
    
    from dataclasses import replace
    return replace(base, **overrides)


def stage_1_load_spec(spec_path: str) -> Dict:
    """Stage 1: Load and validate spec.yml."""
    spec = load_spec(spec_path)
    
    required = {"metadata", "pipe", "environment"}
    missing = required - set(spec.keys())
    if missing:
        raise ValueError(f"Spec missing required sections: {missing}")
    
    return spec


def stage_2_calculate(spec: Dict) -> Dict:
    """Stage 2: Run jumper_lift.py calculations.
    
    Args:
        spec: Parsed spec.yml data.
    
    Returns:
        Results dict from run_jumper_analysis().
    """
    config = parse_jumper_config(spec)
    return run_jumper_analysis(config)


def stage_3_go_no_go(jumper_name: str, results: Dict, custom_criteria: Optional[Dict] = None) -> GoNoGoDecision:
    """Stage 3: Evaluate Go/No-Go decision criteria.
    
    Implements 12 DNV-compliant criteria including:
    - Crane SWL utilisation (SZ + DZ)
    - Dynamic capacity utilisation
    - Sling WLL safety margin
    - Sling stiffness adequacy
    - DAF min/max range
    - Minimum bend radius
    - Vessel deck payload
    - Total lift weight
    - Spreader bar adequacy
    
    Args:
        jumper_name: Name of jumper for report.
        results: Output from stage_2_calculate().
        custom_criteria: Optional custom criterion overrides.
    
    Returns:
        GoNoGoDecision with all criteria evaluated.
    """
    criteria_kwargs = custom_criteria or {}
    return evaluate_go_no_go(jumper_name, results, **criteria_kwargs)


def stage_4_generate_yaml(results: Dict) -> str:
    """Stage 4: Generate OrcaFlex line-section YAML.
    
    Args:
        results: Output from stage_2_calculate().
    
    Returns:
        YAML string defining line sections.
    """
    if isinstance(results, dict):
        return results.get("orcaflex_yaml", "")
    return ""


def stage_5_write_outputs(output_dir: str, output: PipelineOutput) -> List[str]:
    """Stage 5: Write all output files.
    
    Creates:
    - line_sections.yml
    - go_no_go_decision.txt
    - report.json (summary data)
    
    Args:
        output_dir: Target directory.
        output: Pipeline output with all results.
    
    Returns:
        List of output file paths created.
    """
    Path(output_dir).mkdir(parents=True, exist_ok=True)
    output_files = []
    
    # Write line sections YAML
    if output.line_sections_yaml:
        yaml_path = Path(output_dir) / "line_sections.yml"
        with open(yaml_path, "w") as f:
            f.write(output.line_sections_yaml)
        output_files.append(str(yaml_path))
    
    # Write Go/No-Go decision
    if output.go_no_go:
        gng_path = Path(output_dir) / "go_no_go_decision.txt"
        with open(gng_path, "w") as f:
            f.write(print_decision(output.go_no_go))
        output_files.append(str(gng_path))
        
        # Also write machine-readable JSON
        gng_json_path = Path(output_dir) / "go_no_go_decision.json"
        decision_data = {
            "jumper_name": output.go_no_go.jumper_name,
            "overall_state": output.go_no_go.overall_state.value,
            "criteria_count": len(output.go_no_go.criteria),
            "pass_count": sum(1 for c in output.go_no_go.criteria if c.state.name == "PASS"),
            "warning_count": sum(1 for c in output.go_no_go.criteria if c.state.name == "WARNING"),
            "fail_count": sum(1 for c in output.go_no_go.criteria if c.state.name == "FAIL"),
            "criteria": [
                {
                    "name": c.name,
                    "state": c.state.value,
                    "value": c.value,
                    "limit": c.limit,
                    "unit": c.unit,
                    "margin": c.margin,
                    "reference": c.reference,
                }
                for c in output.go_no_go.criteria
            ],
            "summary": output.go_no_go.summary,
        }
        with open(gng_json_path, "w") as f:
            json.dump(decision_data, f, indent=2)
        output_files.append(str(gng_json_path))
    
    # Write analysis summary
    if output.jumper_analysis:
        summary_path = Path(output_dir) / "analysis_summary.json"
        summary_data = {
            "config_name": output.config_name,
            "timestamp": output.timestamp,
            "pipe_properties": str(output.jumper_analysis.get("pipe_properties", {})),
            "total_weight_kg": output.jumper_analysis.get("weight_check_insulated", {}).get("grand_total_kg", 
                                   output.jumper_analysis.get("weight_check", {}).get("grand_total_kg", 0)),
            "total_length_m": output.jumper_analysis.get("pipe_geometry", {}).get("total_length_m", 0),
            "orcaflex_section_count": len(output.jumper_analysis.get("orcaflex_sections", [])),
        }
        with open(summary_path, "w") as f:
            json.dump(summary_data, f, indent=2)
        output_files.append(str(summary_path))
    
    return output_files


def run_pipeline(spec_path: str, output_dir: str = "./output", 
                 run_orcaflex: bool = False,
                 generate_report: bool = True,
                 run_go_no_go: bool = True) -> PipelineOutput:
    """Execute the full jumper installation analysis pipeline.
    
    Pipeline stages:
      1. Load spec.yml
      2. Calculate jumper properties (jumper_lift.py - 24 functions)
      3. Evaluate Go/No-Go decision (12 DNV-compliant criteria)
      4. Generate OrcaFlex line-section YAML
      5. Write output files
      6. (Optional) Run OrcaFlex analysis
      7. (Optional) Generate HTML/PDF reports
    
    Args:
        spec_path: Path to jumper spec.yml (ballymore_mf_plet or ballymore_plet_plem).
        output_dir: Directory for all output files.
        run_orcaflex: Whether to run OrcaFlex solver.
        generate_report: Whether to generate reports.
        run_go_no_go: Whether to evaluate Go/No-Go decision.
    
    Returns:
        PipelineOutput with all results and output file paths.
    """
    config_name = Path(spec_path).parent.name
    output = PipelineOutput(config_name=config_name)
    
    # Stage 1: Load and validate spec
    print(f"[Pipeline] Stage 1/5: Loading spec from {spec_path}")
    spec = stage_1_load_spec(spec_path)
    
    # Stage 2: Calculate jumper properties
    print(f"[Pipeline] Stage 2/5: Running jumper analysis ({config_name})")
    results = stage_2_calculate(spec)
    output.jumper_analysis = results
    
    # Stage 3: Go/No-Go Decision
    if run_go_no_go:
        print(f"[Pipeline] Stage 3/5: Evaluating Go/No-Go decision")
        output.go_no_go = stage_3_go_no_go(config_name, results)
        print(print_decision(output.go_no_go))
    else:
        print(f"[Pipeline] Stage 3/5: Skipped (run_go_no_go=False)")
    
    # Stage 4: Generate OrcaFlex YAML
    print(f"[Pipeline] Stage 4/5: Generating OrcaFlex line sections")
    output.line_sections_yaml = stage_4_generate_yaml(results)
    
    # Stage 5: Write outputs
    print(f"[Pipeline] Stage 5/5: Writing outputs to {output_dir}")
    output.output_files = stage_5_write_outputs(output_dir, output)
    
    print(f"\n[Pipeline] Complete - {len(output.output_files)} files written to {output_dir}")
    print(f"[Pipeline] Output files: {output.output_files}")
    
    return output
