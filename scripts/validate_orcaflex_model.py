"""
OrcaFlex Model Validation Script

Purpose: Reusable script to validate OrcaFlex YAML models by loading them
         with the OrcaFlex Python API and checking for errors.

Usage:
    python validate_orcaflex_model.py <model_file.yml>
    python validate_orcaflex_model.py --batch <directory>
    python validate_orcaflex_model.py --all  # Test all models in base_files

Features:
    - Load YAML models using OrcaFlex API
    - Detailed error reporting with stack traces
    - Validation of model structure and parameters
    - Summary report generation
    - Batch testing support
"""

import sys
import os
from pathlib import Path
from datetime import datetime
import traceback
import json

try:
    import OrcFxAPI
    ORCAFLEX_AVAILABLE = True
    try:
        ORCAFLEX_VERSION = OrcFxAPI.DLLVersion()
    except:
        ORCAFLEX_VERSION = "Unknown"
except ImportError:
    ORCAFLEX_AVAILABLE = False
    ORCAFLEX_VERSION = "Not installed"


class ModelValidator:
    """Validates OrcaFlex YAML models"""

    def __init__(self, verbose=True):
        self.verbose = verbose
        self.results = []

    def log(self, message, level="INFO"):
        """Print log message with timestamp"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        prefix = {
            "INFO": "ℹ",
            "SUCCESS": "✓",
            "ERROR": "✗",
            "WARNING": "⚠"
        }.get(level, "•")

        print(f"[{timestamp}] {prefix} {message}")

    def validate_file_exists(self, filepath):
        """Check if file exists and is readable"""
        if not os.path.exists(filepath):
            self.log(f"File not found: {filepath}", "ERROR")
            return False

        if not os.path.isfile(filepath):
            self.log(f"Path is not a file: {filepath}", "ERROR")
            return False

        if not os.access(filepath, os.R_OK):
            self.log(f"File is not readable: {filepath}", "ERROR")
            return False

        return True

    def check_orcaflex_installation(self):
        """Verify OrcaFlex Python API is available"""
        if not ORCAFLEX_AVAILABLE:
            self.log("OrcaFlex Python API not found!", "ERROR")
            self.log("Install with: pip install OrcFxAPI", "INFO")
            return False

        self.log(f"OrcaFlex API Version: {ORCAFLEX_VERSION}", "INFO")
        return True

    def load_model(self, filepath):
        """
        Load OrcaFlex model and return detailed results

        Returns:
            dict: Validation result with status, errors, warnings, and model info
        """
        result = {
            "file": filepath,
            "timestamp": datetime.now().isoformat(),
            "status": "unknown",
            "errors": [],
            "warnings": [],
            "model_info": {},
            "load_time_seconds": 0
        }

        # Check file exists
        if not self.validate_file_exists(filepath):
            result["status"] = "file_not_found"
            result["errors"].append(f"File not found: {filepath}")
            return result

        # Check OrcaFlex is available
        if not ORCAFLEX_AVAILABLE:
            result["status"] = "orcaflex_not_available"
            result["errors"].append("OrcaFlex Python API not installed")
            return result

        self.log(f"Loading model: {os.path.basename(filepath)}", "INFO")

        model = None
        start_time = datetime.now()

        try:
            # Attempt to load the model
            model = OrcFxAPI.Model(filepath)

            load_time = (datetime.now() - start_time).total_seconds()
            result["load_time_seconds"] = round(load_time, 3)

            self.log(f"Model loaded successfully in {load_time:.3f}s", "SUCCESS")
            result["status"] = "success"

            # Extract model information
            result["model_info"] = self.extract_model_info(model)

            # Validate model structure
            validation_warnings = self.validate_model_structure(model)
            result["warnings"].extend(validation_warnings)

            if validation_warnings:
                self.log(f"Found {len(validation_warnings)} warnings", "WARNING")
                for warning in validation_warnings:
                    self.log(f"  - {warning}", "WARNING")

        except Exception as e:
            load_time = (datetime.now() - start_time).total_seconds()
            result["load_time_seconds"] = round(load_time, 3)
            result["status"] = "error"

            # Capture detailed error information
            error_msg = str(e)
            error_type = type(e).__name__
            error_trace = traceback.format_exc()

            result["errors"].append({
                "type": error_type,
                "message": error_msg,
                "traceback": error_trace
            })

            self.log(f"Failed to load model: {error_type}", "ERROR")
            self.log(f"Error message: {error_msg}", "ERROR")

            if self.verbose:
                self.log("Full traceback:", "ERROR")
                print(error_trace)

            # Try to provide helpful suggestions
            suggestions = self.analyze_error(error_msg, error_type)
            if suggestions:
                result["suggestions"] = suggestions
                self.log("Suggestions:", "INFO")
                for suggestion in suggestions:
                    self.log(f"  • {suggestion}", "INFO")

        finally:
            # Clean up
            if model is not None:
                try:
                    model.destroy()
                except:
                    pass

        return result

    def extract_model_info(self, model):
        """Extract key information from loaded model"""
        info = {}

        try:
            general = model.general
            info["units_system"] = general.UnitsSystem
            info["dynamics_solution"] = general.DynamicsSolutionMethod
            info["stage_count"] = len(general.StageDuration)
            info["stage_durations"] = general.StageDuration

            # Count objects
            info["object_counts"] = {
                "vessels": len(model.vessels),
                "lines": len(model.lines),
                "buoys": len(model.buoys),
                "vessel_types": len(model.vesselTypes),
                "line_types": len(model.lineTypes)
            }

            # Environment info
            env = model.environment
            info["environment"] = {
                "water_depth": env.WaterDepth,
                "density": env.Density,
                "wave_train_count": len(env.WaveTrains) if hasattr(env, 'WaveTrains') else 0
            }

        except Exception as e:
            info["extraction_error"] = str(e)

        return info

    def validate_model_structure(self, model):
        """Validate model structure and return warnings"""
        warnings = []

        try:
            # Check for empty objects
            if len(model.vessels) == 0 and len(model.buoys) == 0:
                warnings.append("No vessels or buoys defined in model")

            if len(model.lines) == 0:
                warnings.append("No lines defined in model")

            # Check for undefined references
            for line in model.lines:
                if line.EndAConnection == "Free":
                    warnings.append(f"Line '{line.name}' has free End A")
                if line.EndBConnection == "Free":
                    warnings.append(f"Line '{line.name}' has free End B")

        except Exception as e:
            warnings.append(f"Error during structure validation: {str(e)}")

        return warnings

    def analyze_error(self, error_msg, error_type):
        """Analyze error and provide suggestions"""
        suggestions = []

        error_lower = error_msg.lower()

        if "file not found" in error_lower or "cannot find" in error_lower:
            suggestions.append("Check that all includefile paths are correct")
            suggestions.append("Verify that referenced module files exist")
            suggestions.append("Ensure file paths are relative to the main YAML file location")

        elif "syntax error" in error_lower or "parse" in error_lower:
            suggestions.append("Check YAML syntax is valid (indentation, colons, etc.)")
            suggestions.append("Verify no tabs are used (YAML requires spaces)")
            suggestions.append("Check for missing or extra quotes")

        elif "invalid" in error_lower:
            suggestions.append("Check parameter values are within valid ranges")
            suggestions.append("Verify parameter names are spelled correctly")
            suggestions.append("Ensure parameter types match OrcaFlex expectations")

        elif "permission" in error_lower or "access" in error_lower:
            suggestions.append("Check file permissions allow reading")
            suggestions.append("Verify no other program has the file locked")

        return suggestions

    def generate_report(self, results, output_file=None):
        """Generate summary report from validation results"""
        total = len(results)
        success = sum(1 for r in results if r["status"] == "success")
        failed = sum(1 for r in results if r["status"] == "error")

        report = {
            "summary": {
                "total_models": total,
                "successful": success,
                "failed": failed,
                "success_rate": f"{(success/total*100):.1f}%" if total > 0 else "0%"
            },
            "results": results,
            "generated_at": datetime.now().isoformat()
        }

        # Print summary
        print("\n" + "="*70)
        print("VALIDATION SUMMARY")
        print("="*70)
        print(f"Total models tested: {total}")
        print(f"Successful:          {success} ({success/total*100:.1f}%)" if total > 0 else "Successful: 0")
        print(f"Failed:              {failed} ({failed/total*100:.1f}%)" if total > 0 else "Failed: 0")
        print("="*70)

        # Detailed results
        if results:
            print("\nDETAILED RESULTS:")
            for i, result in enumerate(results, 1):
                status_symbol = "✓" if result["status"] == "success" else "✗"
                filename = os.path.basename(result["file"])
                print(f"\n{i}. {status_symbol} {filename}")
                print(f"   Status: {result['status']}")
                print(f"   Load time: {result['load_time_seconds']}s")

                if result["status"] == "success" and result.get("model_info"):
                    info = result["model_info"]
                    if "object_counts" in info:
                        counts = info["object_counts"]
                        print(f"   Objects: {counts.get('vessels', 0)} vessels, "
                              f"{counts.get('lines', 0)} lines, "
                              f"{counts.get('buoys', 0)} buoys")

                if result.get("errors"):
                    print(f"   Errors: {len(result['errors'])}")
                    for error in result["errors"]:
                        if isinstance(error, dict):
                            print(f"     - {error.get('type', 'Error')}: {error.get('message', 'Unknown')}")
                        else:
                            print(f"     - {error}")

                if result.get("warnings"):
                    print(f"   Warnings: {len(result['warnings'])}")

                if result.get("suggestions"):
                    print(f"   Suggestions:")
                    for suggestion in result["suggestions"]:
                        print(f"     • {suggestion}")

        # Save to file if requested
        if output_file:
            try:
                with open(output_file, 'w') as f:
                    json.dump(report, f, indent=2)
                self.log(f"Report saved to: {output_file}", "SUCCESS")
            except Exception as e:
                self.log(f"Failed to save report: {e}", "ERROR")

        return report


def main():
    """Main entry point"""
    import argparse

    parser = argparse.ArgumentParser(
        description="Validate OrcaFlex YAML models",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python validate_orcaflex_model.py model.yml
  python validate_orcaflex_model.py --batch base_files/
  python validate_orcaflex_model.py --all
  python validate_orcaflex_model.py model.yml --report validation_report.json
        """
    )

    parser.add_argument("model_file", nargs="?", help="Path to OrcaFlex YAML model file")
    parser.add_argument("--batch", metavar="DIR", help="Validate all YAML files in directory")
    parser.add_argument("--all", action="store_true", help="Validate all models in base_files/")
    parser.add_argument("--report", metavar="FILE", help="Save detailed report to JSON file")
    parser.add_argument("--quiet", action="store_true", help="Suppress verbose output")

    args = parser.parse_args()

    # Determine which files to validate
    files_to_validate = []

    if args.all:
        # Default location
        base_dir = Path(__file__).parent.parent / "projects" / "TEST_OPERABILITY" / "orcaflex" / "base_files"
        if base_dir.exists():
            files_to_validate = [
                base_dir / "calm_buoy_simple_base.yml",
                base_dir / "calm_buoy_discretised_base.yml"
            ]
        else:
            print(f"ERROR: Default base_files directory not found: {base_dir}")
            return 1

    elif args.batch:
        batch_dir = Path(args.batch)
        if not batch_dir.exists():
            print(f"ERROR: Directory not found: {batch_dir}")
            return 1
        files_to_validate = list(batch_dir.glob("*.yml")) + list(batch_dir.glob("*.yaml"))

    elif args.model_file:
        files_to_validate = [Path(args.model_file)]

    else:
        parser.print_help()
        return 1

    # Validate files
    validator = ModelValidator(verbose=not args.quiet)

    print("\n" + "="*70)
    print("ORCAFLEX MODEL VALIDATOR")
    print("="*70)
    print(f"OrcaFlex API: {ORCAFLEX_VERSION}")
    print(f"Files to validate: {len(files_to_validate)}")
    print("="*70 + "\n")

    results = []
    for filepath in files_to_validate:
        result = validator.load_model(str(filepath))
        results.append(result)
        print()  # Blank line between tests

    # Generate report
    report = validator.generate_report(results, args.report)

    # Return exit code based on results
    if all(r["status"] == "success" for r in results):
        return 0
    else:
        return 1


if __name__ == "__main__":
    sys.exit(main())
