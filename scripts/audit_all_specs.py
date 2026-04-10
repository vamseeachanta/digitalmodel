#!/usr/bin/env python3
"""Audit all spec.yml files for OrcaFlex and OrcaWave readiness.

Reports pass/fail/error for each spec across multiple validation stages.
"""

import sys
from pathlib import Path
import yaml
import traceback
import tempfile

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / "src"))

ORCAFLEX_SPEC_DIR = Path(__file__).parent.parent / "docs/domains/orcaflex"
ORCAWAVE_SPEC_DIR = Path(__file__).parent.parent / "docs/domains/orcawave"


def _is_different_schema(data: dict) -> str | None:
    """Detect specs that use a different schema than ProjectInputSpec.

    Returns a short label describing the alternative schema, or None if the
    data looks like a standard ProjectInputSpec.
    """
    # Passing-ship specs have moored_vessel / passing_ship top-level keys
    if "moored_vessel" in data or "passing_ship" in data:
        return "passing_ship"
    # Jumper installation specs use environment.metocean or have pipe: top-level key
    env = data.get("environment", {})
    if isinstance(env, dict) and "metocean" in env:
        return "jumper_installation"
    if "pipe" in data or "jumper" in data:
        return "jumper_installation"
    return None


def audit_orcaflex_spec(spec_path: Path) -> dict:
    """Audit a single OrcaFlex spec.yml file."""
    rel_base = spec_path.parent.parent.parent.parent.parent
    result = {
        "path": str(spec_path.relative_to(rel_base)),
        "yaml_load": "skip",
        "schema_valid": "skip",
        "model_type": "",
        "generation": "skip",
        "yaml_strict": "skip",
        "post_validation": "skip",
        "errors": [],
    }

    # Stage 1: YAML load
    try:
        with open(spec_path) as f:
            data = yaml.safe_load(f)
        result["yaml_load"] = "pass"
    except Exception as e:
        result["yaml_load"] = "fail"
        result["errors"].append(f"YAML load: {e}")
        return result

    if not isinstance(data, dict):
        result["yaml_load"] = "fail"
        result["errors"].append(f"YAML load: expected dict, got {type(data).__name__}")
        return result

    # Stage 1b: Detect non-ProjectInputSpec schemas
    alt_schema = _is_different_schema(data)
    if alt_schema:
        result["schema_valid"] = "different_schema"
        result["model_type"] = alt_schema
        result["generation"] = "different_schema"
        result["yaml_strict"] = "different_schema"
        result["post_validation"] = "different_schema"
        return result

    # Stage 2: Schema validation
    try:
        from digitalmodel.solvers.orcaflex.modular_generator.schema import ProjectInputSpec

        spec = ProjectInputSpec(**data)
        result["schema_valid"] = "pass"
        # Determine model type
        if spec.pipeline is not None:
            result["model_type"] = "pipeline"
        elif spec.riser is not None:
            result["model_type"] = "riser"
        elif spec.mooring is not None:
            result["model_type"] = "mooring"
        elif spec.generic is not None:
            result["model_type"] = "generic"
        else:
            result["model_type"] = "minimal"
    except Exception as e:
        result["schema_valid"] = "fail"
        result["errors"].append(f"Schema: {str(e)[:200]}")
        return result

    # Stage 3: Generation
    try:
        from digitalmodel.solvers.orcaflex.modular_generator import ModularModelGenerator

        with tempfile.TemporaryDirectory() as tmpdir:
            gen = ModularModelGenerator.from_spec(spec)
            gen.generate(Path(tmpdir))
            result["generation"] = "pass"

            # Stage 4: YAML-strict validation
            try:
                from digitalmodel.solvers.orcaflex.yaml_validator import (
                    OrcaFlexYAMLValidator,
                )

                validator = OrcaFlexYAMLValidator()
                includes_dir = Path(tmpdir) / "includes"
                target = includes_dir if includes_dir.exists() else Path(tmpdir)
                vresult = validator.validate_directory(target)
                if vresult.valid:
                    result["yaml_strict"] = "pass"
                else:
                    n_err = len(vresult.errors)
                    result["yaml_strict"] = f"fail({n_err})"
                    for issue in vresult.errors[:3]:
                        result["errors"].append(f"YAML: {issue}")
            except Exception as e:
                result["yaml_strict"] = f"error"
                result["errors"].append(f"YAML validator: {str(e)[:150]}")

            # Stage 5: Post-validation
            try:
                from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
                    PostGenerationValidator,
                )

                pv = PostGenerationValidator()
                includes_dir = Path(tmpdir) / "includes"
                target = includes_dir if includes_dir.exists() else Path(tmpdir)
                warnings = pv.validate_directory(target)
                errors = [w for w in warnings if w.level == "error"]
                if not errors:
                    result["post_validation"] = "pass"
                else:
                    n_err = len(errors)
                    result["post_validation"] = f"fail({n_err})"
                    for w in errors[:3]:
                        result["errors"].append(f"PostVal: {w.message}")
            except Exception as e:
                result["post_validation"] = "error"
                result["errors"].append(f"PostVal: {str(e)[:150]}")
    except Exception as e:
        result["generation"] = "fail"
        result["errors"].append(f"Generation: {str(e)[:200]}")

    return result


def audit_orcawave_spec(spec_path: Path) -> dict:
    """Audit a single OrcaWave spec.yml file."""
    rel_base = spec_path.parent.parent.parent.parent.parent
    result = {
        "path": str(spec_path.relative_to(rel_base)),
        "yaml_load": "skip",
        "schema_valid": "skip",
        "generation": "skip",
        "errors": [],
    }

    # Stage 1: YAML load
    try:
        with open(spec_path) as f:
            data = yaml.safe_load(f)
        result["yaml_load"] = "pass"
    except Exception as e:
        result["yaml_load"] = "fail"
        result["errors"].append(f"YAML load: {e}")
        return result

    if not isinstance(data, dict):
        result["yaml_load"] = "fail"
        result["errors"].append(f"YAML load: expected dict, got {type(data).__name__}")
        return result

    # Stage 2: Schema validation
    try:
        from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec

        spec = DiffractionSpec.model_validate(data)
        result["schema_valid"] = "pass"
    except Exception as e:
        result["schema_valid"] = "fail"
        result["errors"].append(f"Schema: {str(e)[:200]}")
        return result

    # Stage 3: Generation (OrcaWave backend)
    try:
        from digitalmodel.hydrodynamics.diffraction.orcawave_backend import (
            OrcaWaveBackend,
        )

        with tempfile.TemporaryDirectory() as tmpdir:
            backend = OrcaWaveBackend()
            backend.generate_single(spec, Path(tmpdir))
            result["generation"] = "pass"
    except Exception as e:
        result["generation"] = "fail"
        result["errors"].append(f"Generation: {str(e)[:200]}")

    return result


def main():
    print("=" * 80)
    print("OrcaFlex & OrcaWave Spec Readiness Audit")
    print("=" * 80)

    # Collect all specs
    orcaflex_specs = sorted(ORCAFLEX_SPEC_DIR.rglob("spec.yml"))
    orcawave_specs = sorted(ORCAWAVE_SPEC_DIR.rglob("spec.yml"))

    print(f"\nFound {len(orcaflex_specs)} OrcaFlex specs, {len(orcawave_specs)} OrcaWave specs\n")

    # Audit OrcaFlex
    print("=" * 80)
    print("ORCAFLEX SPECS")
    print("=" * 80)

    ofx_results = []
    for i, spec_path in enumerate(orcaflex_specs, 1):
        r = audit_orcaflex_spec(spec_path)
        ofx_results.append(r)
        is_alt = r["schema_valid"] == "different_schema"
        all_pass = is_alt or all(
            v == "pass"
            for k, v in r.items()
            if k not in ("path", "model_type", "errors")
        )
        status = "SKIP" if is_alt else ("PASS" if all_pass else "FAIL")
        short_path = r["path"].replace("docs/domains/orcaflex/", "")
        print(
            f"  [{i:3d}] {status:4s} | {r['model_type']:18s} | "
            f"schema={r['schema_valid']:16s} gen={r['generation']:16s} "
            f"yaml={r['yaml_strict']:16s} post={r['post_validation']:16s} | {short_path}"
        )
        if r["errors"]:
            for err in r["errors"][:2]:
                print(f"              > {str(err)[:140]}")

    # Audit OrcaWave
    print("\n" + "=" * 80)
    print("ORCAWAVE SPECS")
    print("=" * 80)

    ow_results = []
    for i, spec_path in enumerate(orcawave_specs, 1):
        r = audit_orcawave_spec(spec_path)
        ow_results.append(r)
        all_pass = all(
            v == "pass" for k, v in r.items() if k not in ("path", "errors")
        )
        status = "PASS" if all_pass else "FAIL"
        short_path = r["path"].replace("docs/domains/orcawave/", "")
        print(
            f"  [{i:3d}] {status:4s} | schema={r['schema_valid']:4s} "
            f"gen={r['generation']:10s} | {short_path}"
        )
        if r["errors"]:
            for err in r["errors"][:2]:
                print(f"              > {str(err)[:140]}")

    # Summary
    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)

    total_ofx = len(ofx_results)
    total_ow = len(ow_results)

    # Separate different-schema specs from standard ProjectInputSpec specs
    ofx_standard = [r for r in ofx_results if r["schema_valid"] != "different_schema"]
    ofx_alt = [r for r in ofx_results if r["schema_valid"] == "different_schema"]
    total_std = len(ofx_standard)

    ofx_yaml_pass = sum(1 for r in ofx_standard if r["yaml_load"] == "pass")
    ofx_schema_pass = sum(1 for r in ofx_standard if r["schema_valid"] == "pass")
    ofx_gen_pass = sum(1 for r in ofx_standard if r["generation"] == "pass")
    ofx_strict_pass = sum(1 for r in ofx_standard if r["yaml_strict"] == "pass")
    ofx_post_pass = sum(1 for r in ofx_standard if r["post_validation"] == "pass")
    ofx_full_pass = sum(
        1
        for r in ofx_standard
        if all(
            v == "pass"
            for k, v in r.items()
            if k not in ("path", "model_type", "errors")
        )
    )

    ow_yaml_pass = sum(1 for r in ow_results if r["yaml_load"] == "pass")
    ow_schema_pass = sum(1 for r in ow_results if r["schema_valid"] == "pass")
    ow_gen_pass = sum(1 for r in ow_results if r["generation"] == "pass")
    ow_full_pass = sum(
        1
        for r in ow_results
        if all(v == "pass" for k, v in r.items() if k not in ("path", "errors"))
    )

    def pct(n, total):
        return f"{100 * n / total:.0f}%" if total > 0 else "N/A"

    print(f"\nOrcaFlex ({total_ofx} total specs, {total_std} ProjectInputSpec, {len(ofx_alt)} different schema):")
    print(f"  YAML parseable:    {ofx_yaml_pass}/{total_std} ({pct(ofx_yaml_pass, total_std)})")
    print(f"  Schema validation: {ofx_schema_pass}/{total_std} ({pct(ofx_schema_pass, total_std)})")
    print(f"  YAML generation:   {ofx_gen_pass}/{total_std} ({pct(ofx_gen_pass, total_std)})")
    print(f"  YAML-strict:       {ofx_strict_pass}/{total_std} ({pct(ofx_strict_pass, total_std)})")
    print(f"  Post-validation:   {ofx_post_pass}/{total_std} ({pct(ofx_post_pass, total_std)})")
    print(f"  ALL STAGES PASS:   {ofx_full_pass}/{total_std} ({pct(ofx_full_pass, total_std)})")
    if ofx_alt:
        alt_types = {}
        for r in ofx_alt:
            t = r["model_type"]
            alt_types[t] = alt_types.get(t, 0) + 1
        print(f"  Different schemas: {alt_types}")

    print(f"\nOrcaWave ({total_ow} specs):")
    print(f"  YAML parseable:    {ow_yaml_pass}/{total_ow} ({pct(ow_yaml_pass, total_ow)})")
    print(f"  Schema validation: {ow_schema_pass}/{total_ow} ({pct(ow_schema_pass, total_ow)})")
    print(f"  YAML generation:   {ow_gen_pass}/{total_ow} ({pct(ow_gen_pass, total_ow)})")
    print(f"  ALL STAGES PASS:   {ow_full_pass}/{total_ow} ({pct(ow_full_pass, total_ow)})")

    # Model type distribution
    type_counts = {}
    for r in ofx_results:
        t = r["model_type"] or "unknown"
        type_counts[t] = type_counts.get(t, 0) + 1
    print(f"\nOrcaFlex model types: {type_counts}")

    # Common error patterns
    all_errors = []
    for r in ofx_results + ow_results:
        all_errors.extend(r["errors"])

    if all_errors:
        print(f"\nError pattern breakdown ({len(all_errors)} total errors):")
        patterns = {}
        for err in all_errors:
            err_str = str(err)
            # Extract a short pattern key
            if ":" in err_str:
                prefix = err_str.split(":")[0]
            else:
                prefix = err_str[:40]
            patterns[prefix] = patterns.get(prefix, 0) + 1
        for pattern, count in sorted(patterns.items(), key=lambda x: -x[1])[:15]:
            print(f"  {count:3d}x {pattern}")

    # Return results for report generation
    return ofx_results, ow_results


if __name__ == "__main__":
    ofx_results, ow_results = main()
