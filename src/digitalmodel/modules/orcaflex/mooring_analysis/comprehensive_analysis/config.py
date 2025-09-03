"""Configuration management for comprehensive mooring analysis."""

from dataclasses import dataclass, field
from pathlib import Path
from typing import Dict, List, Optional, Any
import yaml


@dataclass
class ConvergenceCriteria:
    """Convergence criteria for pretension analysis."""
    tolerance: float = 0.05  # 5% default tolerance
    max_iterations: int = 100
    min_tension_kN: float = 10.0
    warning_threshold: float = 0.10  # 10% warning threshold


@dataclass
class StiffnessConfig:
    """Configuration for stiffness analysis."""
    compute_natural_periods: bool = True
    vessel_mass_tonnes: Optional[float] = None
    include_rotational_dof: bool = True
    eigenvalue_analysis: bool = True
    anisotropy_threshold: float = 2.0  # Ratio threshold for anisotropy detection


@dataclass
class FenderConfig:
    """Configuration for fender force analysis."""
    design_capacity_kN: Dict[str, float] = field(default_factory=dict)
    warning_utilization: float = 0.8  # 80% utilization warning
    critical_utilization: float = 0.95  # 95% utilization critical
    contact_time_threshold: float = 0.1  # 10% contact time threshold


@dataclass
class GroupingConfig:
    """Configuration for group identification and comparison."""
    auto_group: bool = True
    group_patterns: List[str] = field(default_factory=lambda: [
        r'(?P<vessel>lngc|fsru|fpso|fso)',
        r'(?P<depth>\d+km\d+)',
        r'(?P<condition>ballast|loaded|pb|sb)',
        r'(?P<environment>\d+yr|survival|operational)',
    ])
    min_group_size: int = 2
    compare_metrics: List[str] = field(default_factory=lambda: [
        'pretension_convergence',
        'max_stiffness',
        'max_fender_force',
        'system_natural_period',
    ])


@dataclass
class ReportConfig:
    """Configuration for report generation."""
    formats: List[str] = field(default_factory=lambda: ['markdown', 'html'])
    include_plots: bool = True
    plot_dpi: int = 150
    embed_images: bool = True
    executive_summary: bool = True
    technical_appendix: bool = True
    comparison_tables: bool = True


@dataclass
class ProcessingConfig:
    """Configuration for batch processing."""
    parallel: bool = True
    max_workers: Optional[int] = None  # None = CPU count
    chunk_size: int = 10
    progress_bar: bool = True
    cache_results: bool = True
    cache_dir: Path = Path('.cache/mooring_analysis')
    memory_limit_gb: float = 8.0


@dataclass
class IndustryStandards:
    """Industry standards and compliance criteria."""
    standard: str = "DNV-OS-E301"  # Default standard
    safety_factors: Dict[str, float] = field(default_factory=lambda: {
        'intact': 1.67,
        'damaged': 1.25,
        'transient': 1.35,
    })
    tension_limits: Dict[str, float] = field(default_factory=lambda: {
        'chain': 0.95,  # 95% of MBL
        'wire': 0.85,  # 85% of MBL
        'polyester': 0.50,  # 50% of MBL
    })
    api_rp_2sk_compliance: bool = True


@dataclass
class AnalysisConfig:
    """Main configuration for comprehensive mooring analysis."""
    # Core configurations
    input_directory: Path = Path.cwd()
    output_directory: Path = Path('output')
    file_pattern: str = "*.csv"
    recursive: bool = True
    
    # Analysis-specific configs
    convergence: ConvergenceCriteria = field(default_factory=ConvergenceCriteria)
    stiffness: StiffnessConfig = field(default_factory=StiffnessConfig)
    fender: FenderConfig = field(default_factory=FenderConfig)
    grouping: GroupingConfig = field(default_factory=GroupingConfig)
    report: ReportConfig = field(default_factory=ReportConfig)
    processing: ProcessingConfig = field(default_factory=ProcessingConfig)
    standards: IndustryStandards = field(default_factory=IndustryStandards)
    
    # LLM configuration
    use_llm_context: bool = True
    llm_model: str = "gpt-4"
    llm_temperature: float = 0.2
    
    # Logging
    log_level: str = "INFO"
    log_file: Optional[Path] = None
    
    @classmethod
    def from_yaml(cls, yaml_path: Path) -> 'AnalysisConfig':
        """Load configuration from YAML file.
        
        Args:
            yaml_path: Path to YAML configuration file
            
        Returns:
            AnalysisConfig instance
        """
        with open(yaml_path, 'r') as f:
            config_dict = yaml.safe_load(f)
        return cls.from_dict(config_dict)
    
    @classmethod
    def from_dict(cls, config_dict: Dict[str, Any]) -> 'AnalysisConfig':
        """Create configuration from dictionary.
        
        Args:
            config_dict: Configuration dictionary
            
        Returns:
            AnalysisConfig instance
        """
        # Convert paths
        if 'input_directory' in config_dict:
            config_dict['input_directory'] = Path(config_dict['input_directory'])
        if 'output_directory' in config_dict:
            config_dict['output_directory'] = Path(config_dict['output_directory'])
        
        # Create nested configs
        if 'convergence' in config_dict:
            config_dict['convergence'] = ConvergenceCriteria(**config_dict['convergence'])
        if 'stiffness' in config_dict:
            config_dict['stiffness'] = StiffnessConfig(**config_dict['stiffness'])
        if 'fender' in config_dict:
            config_dict['fender'] = FenderConfig(**config_dict['fender'])
        if 'grouping' in config_dict:
            config_dict['grouping'] = GroupingConfig(**config_dict['grouping'])
        if 'report' in config_dict:
            config_dict['report'] = ReportConfig(**config_dict['report'])
        if 'processing' in config_dict:
            if 'cache_dir' in config_dict['processing']:
                config_dict['processing']['cache_dir'] = Path(config_dict['processing']['cache_dir'])
            config_dict['processing'] = ProcessingConfig(**config_dict['processing'])
        if 'standards' in config_dict:
            config_dict['standards'] = IndustryStandards(**config_dict['standards'])
        
        return cls(**config_dict)
    
    def to_yaml(self, yaml_path: Path) -> None:
        """Save configuration to YAML file.
        
        Args:
            yaml_path: Path to save YAML configuration
        """
        import dataclasses
        
        def convert_to_dict(obj):
            """Recursively convert dataclass to dictionary."""
            if dataclasses.is_dataclass(obj):
                result = {}
                for field in dataclasses.fields(obj):
                    value = getattr(obj, field.name)
                    result[field.name] = convert_to_dict(value)
                return result
            elif isinstance(obj, Path):
                return str(obj)
            elif isinstance(obj, list):
                return [convert_to_dict(item) for item in obj]
            elif isinstance(obj, dict):
                return {k: convert_to_dict(v) for k, v in obj.items()}
            else:
                return obj
        
        config_dict = convert_to_dict(self)
        
        with open(yaml_path, 'w') as f:
            yaml.dump(config_dict, f, default_flow_style=False, sort_keys=False)
    
    def validate(self) -> List[str]:
        """Validate configuration settings.
        
        Returns:
            List of validation warnings/errors
        """
        warnings = []
        
        # Check paths
        if not self.input_directory.exists():
            warnings.append(f"Input directory does not exist: {self.input_directory}")
        
        # Check thresholds
        if self.convergence.tolerance <= 0:
            warnings.append("Convergence tolerance must be positive")
        
        if self.fender.warning_utilization >= self.fender.critical_utilization:
            warnings.append("Fender warning utilization should be less than critical")
        
        # Check processing limits
        if self.processing.memory_limit_gb <= 0:
            warnings.append("Memory limit must be positive")
        
        return warnings


def create_default_config(output_path: Optional[Path] = None) -> AnalysisConfig:
    """Create and optionally save a default configuration.
    
    Args:
        output_path: Optional path to save configuration YAML
        
    Returns:
        Default AnalysisConfig instance
    """
    config = AnalysisConfig()
    
    if output_path:
        config.to_yaml(output_path)
        print(f"Default configuration saved to: {output_path}")
    
    return config