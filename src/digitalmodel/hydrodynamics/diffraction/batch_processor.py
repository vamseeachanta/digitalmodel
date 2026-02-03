#!/usr/bin/env python3
"""
Batch Diffraction Processing Framework

ABOUTME: Batch processing framework for converting multiple vessels and configurations from AQWA/OrcaWave to unified schema.

Supports:
- Multiple vessel processing
- Multiple draft/heading configurations
- Parallel execution
- Progress tracking
- Consolidated reporting

Version: 3.0.0 (Phase 3)
Status: Batch processing automation
"""

import json
from pathlib import Path
from typing import List, Dict, Optional, Tuple
from dataclasses import dataclass, field, asdict
from datetime import datetime
from concurrent.futures import ProcessPoolExecutor, as_completed
import traceback

from digitalmodel.hydrodynamics.diffraction.output_schemas import DiffractionResults
from digitalmodel.hydrodynamics.diffraction.orcaflex_exporter import OrcaFlexExporter
from digitalmodel.hydrodynamics.diffraction.output_validator import validate_results


@dataclass
class BatchConfiguration:
    """Single vessel configuration for batch processing"""
    vessel_name: str
    source_type: str  # 'aqwa' or 'orcawave'
    source_path: Path
    water_depth: float
    output_dir: Path
    export_formats: List[str] = field(default_factory=lambda: ['all'])
    validate: bool = True


@dataclass
class BatchResult:
    """Result from processing a single configuration"""
    vessel_name: str
    source_type: str
    status: str  # 'success', 'error', 'warning'
    execution_time: float  # seconds
    output_files: Dict[str, Path] = field(default_factory=dict)
    validation_status: Optional[str] = None
    error_message: Optional[str] = None


@dataclass
class BatchReport:
    """Complete batch processing report"""
    start_time: str
    end_time: str
    total_duration: float  # seconds
    total_configurations: int
    successful: int
    failed: int
    warnings: int
    results: List[BatchResult] = field(default_factory=list)
    summary_statistics: Dict = field(default_factory=dict)


class BatchProcessor:
    """Batch processor for diffraction conversions"""

    def __init__(
        self,
        configurations: List[BatchConfiguration],
        max_workers: Optional[int] = None
    ):
        """
        Initialize batch processor

        Args:
            configurations: List of vessel configurations to process
            max_workers: Maximum parallel workers (None = auto)
        """
        self.configurations = configurations
        self.max_workers = max_workers or min(4, len(configurations))

        self.report = BatchReport(
            start_time="",
            end_time="",
            total_duration=0.0,
            total_configurations=len(configurations),
            successful=0,
            failed=0,
            warnings=0
        )

    def process_configuration(
        self,
        config: BatchConfiguration
    ) -> BatchResult:
        """
        Process a single configuration

        Args:
            config: Vessel configuration

        Returns:
            BatchResult object
        """
        start_time = datetime.now()

        result = BatchResult(
            vessel_name=config.vessel_name,
            source_type=config.source_type,
            status='pending',
            execution_time=0.0
        )

        try:
            print(f"\nProcessing: {config.vessel_name} ({config.source_type})")
            print("-" * 60)

            # Convert based on source type
            if config.source_type == 'aqwa':
                results = self._process_aqwa(config)
            elif config.source_type == 'orcawave':
                results = self._process_orcawave(config)
            else:
                raise ValueError(f"Unknown source type: {config.source_type}")

            # Validate if requested
            if config.validate:
                print(f"  Validating results...")
                validation_file = config.output_dir / f"{config.vessel_name}_validation.json"
                validation_report = validate_results(results, output_file=validation_file)
                result.validation_status = validation_report['overall_status']
                print(f"  Validation status: {result.validation_status}")

            # Export to requested formats
            print(f"  Exporting to OrcaFlex formats...")
            exporter = OrcaFlexExporter(results, config.output_dir)

            if 'all' in config.export_formats:
                output_files = exporter.export_all()
            else:
                output_files = {}
                for fmt in config.export_formats:
                    if fmt == 'vessel_type':
                        output_files['vessel_type'] = exporter.export_vessel_type()
                    elif fmt == 'rao_csv':
                        output_files['rao_csv'] = exporter.export_raos_csv()
                    elif fmt == 'added_mass_csv':
                        output_files['added_mass_csv'] = exporter.export_added_mass_csv()
                    elif fmt == 'damping_csv':
                        output_files['damping_csv'] = exporter.export_damping_csv()
                    elif fmt == 'excel':
                        output_files['excel'] = exporter.export_excel_workbook()
                    elif fmt == 'summary':
                        output_files['summary'] = exporter.export_summary()

            result.output_files = output_files
            result.status = 'success'

            if result.validation_status in ['WARNING', 'FAIL']:
                result.status = 'warning'

            print(f"  [OK] Completed: {config.vessel_name}")
            print(f"       Output files: {len(output_files)}")

        except Exception as e:
            result.status = 'error'
            result.error_message = str(e)
            print(f"  [ERROR] Failed: {config.vessel_name}")
            print(f"          {str(e)}")
            traceback.print_exc()

        # Calculate execution time
        end_time = datetime.now()
        result.execution_time = (end_time - start_time).total_seconds()

        return result

    def _process_aqwa(self, config: BatchConfiguration) -> DiffractionResults:
        """Process AQWA configuration"""
        from digitalmodel.hydrodynamics.diffraction.aqwa_converter import AQWAConverter

        converter = AQWAConverter(
            analysis_folder=config.source_path,
            vessel_name=config.vessel_name
        )

        results = converter.convert_to_unified_schema(
            water_depth=config.water_depth
        )

        return results

    def _process_orcawave(self, config: BatchConfiguration) -> DiffractionResults:
        """Process OrcaWave configuration"""
        from digitalmodel.diffraction import ORCAWAVE_AVAILABLE

        if not ORCAWAVE_AVAILABLE:
            raise ImportError("OrcFxAPI required for OrcaWave processing")

        from digitalmodel.hydrodynamics.diffraction.orcawave_converter import OrcaWaveConverter

        converter = OrcaWaveConverter(
            model_file=config.source_path,
            vessel_name=config.vessel_name
        )

        results = converter.convert_to_unified_schema(
            water_depth=config.water_depth
        )

        return results

    def run(self, parallel: bool = True) -> BatchReport:
        """
        Run batch processing

        Args:
            parallel: Enable parallel processing (default: True)

        Returns:
            BatchReport object
        """
        print("=" * 80)
        print("Batch Diffraction Processing")
        print("=" * 80)
        print(f"Total configurations: {len(self.configurations)}")
        print(f"Parallel processing: {parallel}")
        print(f"Max workers: {self.max_workers if parallel else 1}")
        print()

        start = datetime.now()
        self.report.start_time = start.strftime("%Y-%m-%d %H:%M:%S")

        if parallel and len(self.configurations) > 1:
            # Parallel execution
            with ProcessPoolExecutor(max_workers=self.max_workers) as executor:
                futures = {
                    executor.submit(self.process_configuration, config): config
                    for config in self.configurations
                }

                for future in as_completed(futures):
                    result = future.result()
                    self.report.results.append(result)

                    # Update counters
                    if result.status == 'success':
                        self.report.successful += 1
                    elif result.status == 'warning':
                        self.report.warnings += 1
                    elif result.status == 'error':
                        self.report.failed += 1
        else:
            # Sequential execution
            for config in self.configurations:
                result = self.process_configuration(config)
                self.report.results.append(result)

                # Update counters
                if result.status == 'success':
                    self.report.successful += 1
                elif result.status == 'warning':
                    self.report.warnings += 1
                elif result.status == 'error':
                    self.report.failed += 1

        # Finalize report
        end = datetime.now()
        self.report.end_time = end.strftime("%Y-%m-%d %H:%M:%S")
        self.report.total_duration = (end - start).total_seconds()

        # Calculate statistics
        self._calculate_statistics()

        # Print summary
        self._print_summary()

        return self.report

    def _calculate_statistics(self):
        """Calculate summary statistics"""
        if not self.report.results:
            return

        # Execution times
        exec_times = [r.execution_time for r in self.report.results]

        self.report.summary_statistics = {
            'avg_execution_time': sum(exec_times) / len(exec_times),
            'min_execution_time': min(exec_times),
            'max_execution_time': max(exec_times),
            'total_output_files': sum(len(r.output_files) for r in self.report.results),
            'success_rate': self.report.successful / self.report.total_configurations * 100
        }

    def _print_summary(self):
        """Print batch processing summary"""
        print("\n")
        print("=" * 80)
        print("Batch Processing Summary")
        print("=" * 80)
        print(f"Start time: {self.report.start_time}")
        print(f"End time: {self.report.end_time}")
        print(f"Total duration: {self.report.total_duration:.2f} seconds")
        print()
        print(f"Total configurations: {self.report.total_configurations}")
        print(f"  Successful: {self.report.successful}")
        print(f"  Warnings: {self.report.warnings}")
        print(f"  Failed: {self.report.failed}")
        print(f"  Success rate: {self.report.summary_statistics.get('success_rate', 0):.1f}%")
        print()

        if self.report.summary_statistics:
            stats = self.report.summary_statistics
            print(f"Execution time statistics:")
            print(f"  Average: {stats['avg_execution_time']:.2f} seconds")
            print(f"  Minimum: {stats['min_execution_time']:.2f} seconds")
            print(f"  Maximum: {stats['max_execution_time']:.2f} seconds")
            print()
            print(f"Total output files: {stats['total_output_files']}")

        print("=" * 80)

    def export_report(self, output_file: Path):
        """
        Export batch report to JSON

        Args:
            output_file: Path to output file
        """
        # Convert to dictionary
        report_dict = {
            'start_time': self.report.start_time,
            'end_time': self.report.end_time,
            'total_duration': self.report.total_duration,
            'total_configurations': self.report.total_configurations,
            'successful': self.report.successful,
            'failed': self.report.failed,
            'warnings': self.report.warnings,
            'summary_statistics': self.report.summary_statistics,
            'results': []
        }

        for result in self.report.results:
            result_dict = {
                'vessel_name': result.vessel_name,
                'source_type': result.source_type,
                'status': result.status,
                'execution_time': result.execution_time,
                'validation_status': result.validation_status,
                'output_files': {k: str(v) for k, v in result.output_files.items()},
                'error_message': result.error_message
            }
            report_dict['results'].append(result_dict)

        with open(output_file, 'w') as f:
            json.dump(report_dict, f, indent=2)

        print(f"\n[OK] Batch report exported to: {output_file}")


def process_batch_from_config_file(config_file: Path) -> BatchReport:
    """
    Process batch from JSON configuration file

    Args:
        config_file: Path to JSON config file

    Returns:
        BatchReport object
    """
    with open(config_file) as f:
        config_data = json.load(f)

    # Create configurations
    configurations = []
    for config_dict in config_data['configurations']:
        config = BatchConfiguration(
            vessel_name=config_dict['vessel_name'],
            source_type=config_dict['source_type'],
            source_path=Path(config_dict['source_path']),
            water_depth=config_dict['water_depth'],
            output_dir=Path(config_dict['output_dir']),
            export_formats=config_dict.get('export_formats', ['all']),
            validate=config_dict.get('validate', True)
        )
        configurations.append(config)

    # Process batch
    processor = BatchProcessor(
        configurations=configurations,
        max_workers=config_data.get('max_workers')
    )

    report = processor.run(parallel=config_data.get('parallel', True))

    # Export report
    if 'report_output' in config_data:
        processor.export_report(Path(config_data['report_output']))

    return report
