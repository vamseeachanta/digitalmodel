"""
ABOUTME: Pydantic models for standardized reporting
ABOUTME: Defines report structure, parameters, results, and parametric studies
"""

from pydantic import BaseModel, Field, field_validator
from typing import List, Dict, Any, Optional, Literal, Union
from datetime import datetime
from pathlib import Path
import json


class ParameterSet(BaseModel):
    """
    Single set of input parameters for an analysis.

    Example:
        >>> params = ParameterSet(
        ...     name="safety_factor",
        ...     value=1.5,
        ...     unit="dimensionless",
        ...     description="Global safety factor"
        ... )
    """
    name: str = Field(..., description="Parameter name")
    value: Union[float, int, str, bool] = Field(..., description="Parameter value")
    unit: Optional[str] = Field(None, description="Unit of measurement")
    description: Optional[str] = Field(None, description="Parameter description")

    class Config:
        json_schema_extra = {
            "example": {
                "name": "safety_factor",
                "value": 1.5,
                "unit": "dimensionless",
                "description": "Global safety factor for design"
            }
        }


class ValidationResult(BaseModel):
    """
    Validation check result.

    Example:
        >>> validation = ValidationResult(
        ...     check_name="stress_check",
        ...     passed=True,
        ...     message="Maximum stress within allowable limits",
        ...     severity="info"
        ... )
    """
    check_name: str = Field(..., description="Name of validation check")
    passed: bool = Field(..., description="Whether validation passed")
    message: str = Field(..., description="Validation result message")
    severity: Literal["info", "warning", "error", "critical"] = Field(
        default="info",
        description="Severity level of validation result"
    )
    details: Optional[Dict[str, Any]] = Field(
        default=None,
        description="Additional validation details"
    )


class AnalysisResult(BaseModel):
    """
    Single analysis result with metadata.

    Example:
        >>> result = AnalysisResult(
        ...     metric_name="max_stress",
        ...     value=250.5,
        ...     unit="MPa",
        ...     passed=True,
        ...     threshold=300.0
        ... )
    """
    metric_name: str = Field(..., description="Name of result metric")
    value: Union[float, int, str, bool, List, Dict] = Field(
        ...,
        description="Result value (can be scalar, array, or nested)"
    )
    unit: Optional[str] = Field(None, description="Unit of measurement")
    description: Optional[str] = Field(None, description="Result description")
    passed: Optional[bool] = Field(None, description="Pass/fail status")
    threshold: Optional[float] = Field(None, description="Pass/fail threshold")
    validation: Optional[ValidationResult] = Field(
        None,
        description="Associated validation result"
    )

    class Config:
        json_schema_extra = {
            "example": {
                "metric_name": "max_von_mises_stress",
                "value": 250.5,
                "unit": "MPa",
                "description": "Maximum von Mises stress",
                "passed": True,
                "threshold": 300.0
            }
        }


class ReportMetadata(BaseModel):
    """
    Report metadata and tracking information.
    """
    module: str = Field(..., description="Analysis module name")
    analysis_type: str = Field(..., description="Type of analysis performed")
    version: str = Field(default="1.0.0", description="Report format version")
    generated_at: datetime = Field(
        default_factory=datetime.now,
        description="Report generation timestamp"
    )
    generated_by: str = Field(
        default="digitalmodel",
        description="Tool/system that generated report"
    )
    execution_time_seconds: float = Field(
        ...,
        description="Total execution time in seconds"
    )
    status: Literal["success", "warning", "error"] = Field(
        default="success",
        description="Overall analysis status"
    )

    class Config:
        json_schema_extra = {
            "example": {
                "module": "structural_analysis",
                "analysis_type": "stress_analysis",
                "version": "1.0.0",
                "execution_time_seconds": 45.2,
                "status": "success"
            }
        }


class StandardReport(BaseModel):
    """
    Standard report structure for all digitalmodel analyses.

    Provides consistent format for:
    - Input parameters
    - Analysis results
    - Validation checks
    - Metadata and tracking

    Example:
        >>> report = StandardReport(
        ...     metadata=ReportMetadata(
        ...         module="fatigue_analysis",
        ...         analysis_type="rainflow_counting",
        ...         execution_time_seconds=12.5
        ...     ),
        ...     parameters=[
        ...         ParameterSet(name="safety_factor", value=1.5, unit="-"),
        ...         ParameterSet(name="material", value="S355", unit="-")
        ...     ],
        ...     results=[
        ...         AnalysisResult(
        ...             metric_name="fatigue_damage",
        ...             value=0.75,
        ...             unit="-",
        ...             passed=True,
        ...             threshold=1.0
        ...         )
        ...     ]
        ... )
    """
    metadata: ReportMetadata = Field(..., description="Report metadata")
    parameters: List[ParameterSet] = Field(
        default_factory=list,
        description="Input parameters used in analysis"
    )
    results: List[AnalysisResult] = Field(
        default_factory=list,
        description="Analysis results"
    )
    validations: List[ValidationResult] = Field(
        default_factory=list,
        description="Validation check results"
    )
    plots: List[Dict[str, Any]] = Field(
        default_factory=list,
        description="Plot metadata (filenames, types, descriptions)"
    )
    attachments: List[Dict[str, Any]] = Field(
        default_factory=list,
        description="Additional file attachments"
    )
    notes: List[str] = Field(
        default_factory=list,
        description="Additional notes or comments"
    )

    def add_parameter(
        self,
        name: str,
        value: Union[float, int, str, bool],
        unit: Optional[str] = None,
        description: Optional[str] = None
    ) -> None:
        """Add a parameter to the report"""
        param = ParameterSet(
            name=name,
            value=value,
            unit=unit,
            description=description
        )
        self.parameters.append(param)

    def add_result(
        self,
        metric_name: str,
        value: Union[float, int, str, bool, List, Dict],
        unit: Optional[str] = None,
        description: Optional[str] = None,
        passed: Optional[bool] = None,
        threshold: Optional[float] = None
    ) -> None:
        """Add a result to the report"""
        result = AnalysisResult(
            metric_name=metric_name,
            value=value,
            unit=unit,
            description=description,
            passed=passed,
            threshold=threshold
        )
        self.results.append(result)

    def add_validation(
        self,
        check_name: str,
        passed: bool,
        message: str,
        severity: Literal["info", "warning", "error", "critical"] = "info",
        details: Optional[Dict[str, Any]] = None
    ) -> None:
        """Add a validation check result"""
        validation = ValidationResult(
            check_name=check_name,
            passed=passed,
            message=message,
            severity=severity,
            details=details
        )
        self.validations.append(validation)

    def add_plot(
        self,
        filename: str,
        plot_type: str,
        description: Optional[str] = None,
        **metadata
    ) -> None:
        """Add plot metadata"""
        plot_info = {
            "filename": filename,
            "plot_type": plot_type,
            "description": description,
            **metadata
        }
        self.plots.append(plot_info)

    def get_summary(self) -> Dict[str, Any]:
        """Get summary statistics of the report"""
        return {
            "module": self.metadata.module,
            "analysis_type": self.metadata.analysis_type,
            "status": self.metadata.status,
            "execution_time_seconds": self.metadata.execution_time_seconds,
            "parameter_count": len(self.parameters),
            "result_count": len(self.results),
            "validation_count": len(self.validations),
            "validations_passed": sum(1 for v in self.validations if v.passed),
            "validations_failed": sum(1 for v in self.validations if not v.passed),
            "plot_count": len(self.plots),
        }

    def to_dict(self) -> Dict[str, Any]:
        """Convert report to dictionary"""
        return self.model_dump()

    def to_json(self, indent: int = 2) -> str:
        """Convert report to JSON string"""
        return self.model_dump_json(indent=indent)

    def save_json(self, filepath: Path) -> None:
        """Save report as JSON file"""
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)

        with open(filepath, 'w') as f:
            f.write(self.to_json())


class ParametricStudy(BaseModel):
    """
    Collection of StandardReports for parametric analysis.

    Enables:
    - Running multiple analyses with varying parameters
    - Comparing results across parameter sets
    - Generating comparison tables and plots

    Example:
        >>> study = ParametricStudy(
        ...     study_name="safety_factor_study",
        ...     description="Varying safety factor from 1.2 to 3.0",
        ...     parameter_name="safety_factor"
        ... )
        >>> study.add_report(report_sf_1_5)
        >>> study.add_report(report_sf_2_0)
        >>> comparison = study.get_comparison_table()
    """
    study_name: str = Field(..., description="Name of parametric study")
    description: Optional[str] = Field(None, description="Study description")
    parameter_name: str = Field(..., description="Primary parameter being varied")
    reports: List[StandardReport] = Field(
        default_factory=list,
        description="Collection of analysis reports"
    )
    created_at: datetime = Field(
        default_factory=datetime.now,
        description="Study creation timestamp"
    )

    def add_report(self, report: StandardReport) -> None:
        """Add a report to the parametric study"""
        self.reports.append(report)

    def get_parameter_values(self) -> List[Union[float, int, str]]:
        """Get all values of the varied parameter"""
        values = []
        for report in self.reports:
            for param in report.parameters:
                if param.name == self.parameter_name:
                    values.append(param.value)
                    break
        return values

    def get_comparison_table(
        self,
        metric_names: Optional[List[str]] = None
    ) -> Dict[str, List[Any]]:
        """
        Get comparison table of results across all reports.

        Args:
            metric_names: List of metric names to include (None = all)

        Returns:
            Dictionary with parameter values and metric values
        """
        # Get parameter values
        param_values = self.get_parameter_values()

        # Initialize table
        table = {
            self.parameter_name: param_values
        }

        # Get all unique metric names if not specified
        if metric_names is None:
            metric_names = set()
            for report in self.reports:
                for result in report.results:
                    metric_names.add(result.metric_name)
            metric_names = sorted(metric_names)

        # Populate table
        for metric_name in metric_names:
            values = []
            for report in self.reports:
                value = None
                for result in report.results:
                    if result.metric_name == metric_name:
                        value = result.value
                        break
                values.append(value)
            table[metric_name] = values

        return table

    def get_summary(self) -> Dict[str, Any]:
        """Get summary of parametric study"""
        return {
            "study_name": self.study_name,
            "parameter_name": self.parameter_name,
            "report_count": len(self.reports),
            "parameter_values": self.get_parameter_values(),
            "created_at": self.created_at.isoformat(),
        }

    def to_json(self, indent: int = 2) -> str:
        """Convert parametric study to JSON"""
        return self.model_dump_json(indent=indent)

    def save_json(self, filepath: Path) -> None:
        """Save parametric study as JSON file"""
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)

        with open(filepath, 'w') as f:
            f.write(self.to_json())
