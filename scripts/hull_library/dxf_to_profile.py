"""Convert a selected DXF body plan and YAML configuration to hull artifacts."""

import argparse
import json
from pathlib import Path

import yaml

from digitalmodel.hydrodynamics.hull_library.curvature_screen import (
    hullprod_available,
    screen_profile,
)
from digitalmodel.hydrodynamics.hull_library.line_generator.dxf_lines_reader import (
    DxfLinesConfig,
    DxfLinesError,
    DxfReadReport,
    hull_line_definition_to_profile,
    read_dxf_body_plan_with_report,
)
from digitalmodel.hydrodynamics.hull_library.line_generator.exporter import (
    export_sections_svg,
)
from digitalmodel.hydrodynamics.hull_library.line_generator.line_parser import (
    HullLineDefinition,
)
from digitalmodel.visualization.design_tools.hull_hydrostatics import HullHydrostatics


def _write_json(path, value):
    path.write_text(json.dumps(value, indent=2, allow_nan=False), encoding="utf-8")


def _artifacts(defn, metadata, output, report):
    profile = hull_line_definition_to_profile(defn, **metadata)
    hydrostatics = HullHydrostatics(profile).compute_all()
    signature = None
    if hullprod_available():
        signature = screen_profile(profile).signature.model_dump(mode="json")
    else:
        report.warnings.append(
            "HullProd signature skipped; install digitalmodel[curvature]"
        )
    # Compute everything before publishing a success profile.
    svg_defn = HullLineDefinition.model_validate(defn.model_dump() | metadata)
    export_sections_svg(svg_defn, output / "sections.svg")
    _write_json(output / "hydrostatics.json", hydrostatics)
    if signature is not None:
        _write_json(output / "hullprod_signature.json", signature)
    profile.save_yaml(output / "profile.yaml")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("dxf", type=Path)
    parser.add_argument("config", type=Path)
    parser.add_argument("--output-dir", type=Path, required=True)
    args = parser.parse_args(argv)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    report = DxfReadReport()
    if any(args.output_dir.iterdir()):
        report.errors.append(
            "Use a fresh output directory; existing artifacts are preserved"
        )
        print(report.model_dump_json(indent=2))
        return 1
    try:
        settings = yaml.safe_load(args.config.read_text(encoding="utf-8"))
        if not isinstance(settings, dict) or set(settings) != {"reader", "profile"}:
            raise ValueError("Configuration requires reader and profile mappings")
        cfg = DxfLinesConfig.model_validate(settings["reader"])
        defn, report = read_dxf_body_plan_with_report(args.dxf, cfg)
        _artifacts(defn, settings["profile"], args.output_dir, report)
    except DxfLinesError as exc:
        if exc.report.entities_seen:
            report = exc.report
        else:
            report.errors.extend(exc.report.errors)
    except (
        ValueError,
        TypeError,
        OSError,
        ImportError,
        RuntimeError,
        yaml.YAMLError,
    ) as exc:
        # Do not serialize raw exception strings containing input paths or YAML data.
        report.errors.append(
            f"Conversion failed ({type(exc).__name__}); check config and dependencies"
        )
    _write_json(args.output_dir / "read_report.json", report.model_dump())
    if report.errors:
        print(report.model_dump_json(indent=2))
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
