"""
Analysis mode features for Create Go-By Folder Tool
"""

import re
import json
import yaml
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple, Set
from collections import defaultdict, Counter
from datetime import datetime
from dataclasses import dataclass, asdict
import logging
import statistics

logger = logging.getLogger(__name__)


@dataclass
class ParameterSweep:
    """Parameter sweep information."""
    parameter_name: str
    values: List[Any]
    value_type: str
    range_info: Optional[Dict] = None
    distribution: Optional[str] = None
    
    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        return asdict(self)


@dataclass
class BatchTemplate:
    """Batch processing template."""
    name: str
    pattern: str
    parameters: Dict[str, List[Any]]
    execution_order: Optional[List[str]] = None
    dependencies: Optional[Dict[str, List[str]]] = None
    
    def to_dict(self) -> Dict:
        """Convert to dictionary."""
        return asdict(self)


class AnalysisMode:
    """Advanced analysis features for go-by folders."""
    
    def __init__(self, source_path: Path, config: Optional[Dict] = None):
        """
        Initialize analysis mode.
        
        Args:
            source_path: Source folder path
            config: Configuration dictionary
        """
        self.source_path = Path(source_path)
        self.config = config or {}
        self.analysis_results = {}
    
    def detect_parameter_sweeps(self, files: List[Dict]) -> List[ParameterSweep]:
        """
        Detect parameter sweeps in file names.
        
        Args:
            files: List of file information
            
        Returns:
            List of detected parameter sweeps
        """
        sweeps = []
        parameter_values = defaultdict(set)
        
        # Extract parameters from file names
        for file_info in files:
            filename = file_info.get('name', '')
            
            # Look for common parameter patterns
            patterns = [
                r'_([a-zA-Z]+)=([0-9.]+)',  # param=value
                r'_([a-zA-Z]+)_([0-9.]+)',   # param_value
                r'\.([a-zA-Z]+)([0-9.]+)\.',  # .paramValue.
            ]
            
            for pattern in patterns:
                matches = re.findall(pattern, filename)
                for param, value in matches:
                    parameter_values[param].add(value)
        
        # Analyze each parameter
        for param_name, values in parameter_values.items():
            if len(values) < 2:
                continue  # Not a sweep if only one value
            
            # Convert and analyze values
            typed_values = self._type_values(list(values))
            
            if typed_values:
                sweep = ParameterSweep(
                    parameter_name=param_name,
                    values=sorted(typed_values),
                    value_type=type(typed_values[0]).__name__,
                    range_info=self._analyze_range(typed_values),
                    distribution=self._analyze_distribution(typed_values)
                )
                sweeps.append(sweep)
        
        logger.info(f"Detected {len(sweeps)} parameter sweeps")
        return sweeps
    
    def generate_batch_templates(self, 
                                files: List[Dict],
                                sweeps: List[ParameterSweep]) -> List[BatchTemplate]:
        """
        Generate batch processing templates.
        
        Args:
            files: List of file information
            sweeps: Detected parameter sweeps
            
        Returns:
            List of batch templates
        """
        templates = []
        
        # Group files by pattern
        pattern_groups = self._group_by_pattern(files)
        
        for pattern, group_files in pattern_groups.items():
            if len(group_files) < 2:
                continue
            
            # Extract parameters for this pattern
            pattern_params = {}
            for sweep in sweeps:
                if any(sweep.parameter_name in f.get('name', '') for f in group_files):
                    pattern_params[sweep.parameter_name] = sweep.values
            
            if pattern_params:
                template = BatchTemplate(
                    name=f"batch_{pattern.replace('*', 'X')}",
                    pattern=pattern,
                    parameters=pattern_params,
                    execution_order=self._determine_execution_order(pattern_params),
                    dependencies=self._detect_dependencies(group_files)
                )
                templates.append(template)
        
        logger.info(f"Generated {len(templates)} batch templates")
        return templates
    
    def analyze_workflows(self, files: List[Dict]) -> Dict[str, Any]:
        """
        Analyze workflow patterns.
        
        Args:
            files: List of file information
            
        Returns:
            Workflow analysis results
        """
        workflow = {
            'type': 'unknown',
            'stages': [],
            'data_flow': {},
            'parallelizable': [],
            'bottlenecks': []
        }
        
        # Categorize files by extension
        categories = self._categorize_files(files)
        
        # Detect workflow type
        if 'simulation' in categories and categories['simulation']:
            workflow['type'] = 'simulation'
            workflow['stages'] = self._analyze_simulation_workflow(categories)
        elif 'script' in categories and 'data' in categories:
            workflow['type'] = 'data_processing'
            workflow['stages'] = self._analyze_data_workflow(categories)
        elif 'config' in categories and len(categories['config']) > 5:
            workflow['type'] = 'batch_processing'
            workflow['stages'] = self._analyze_batch_workflow(categories)
        
        # Analyze data flow
        workflow['data_flow'] = self._analyze_data_flow(files)
        
        # Identify parallelizable operations
        workflow['parallelizable'] = self._identify_parallel_ops(workflow['stages'])
        
        # Detect bottlenecks
        workflow['bottlenecks'] = self._detect_bottlenecks(files, workflow['stages'])
        
        return workflow
    
    def generate_variation_scripts(self, 
                                  sweeps: List[ParameterSweep],
                                  templates: List[BatchTemplate]) -> Dict[str, str]:
        """
        Generate scripts for creating variations.
        
        Args:
            sweeps: Parameter sweeps
            templates: Batch templates
            
        Returns:
            Dictionary of script name to script content
        """
        scripts = {}
        
        # Generate Python script for variations
        python_script = self._generate_python_variation_script(sweeps, templates)
        if python_script:
            scripts['generate_variations.py'] = python_script
        
        # Generate batch configuration
        batch_config = self._generate_batch_config(templates)
        if batch_config:
            scripts['batch_config.yml'] = batch_config
        
        # Generate shell script for execution
        shell_script = self._generate_shell_script(templates)
        if shell_script:
            scripts['run_batch.sh'] = shell_script
        
        logger.info(f"Generated {len(scripts)} variation scripts")
        return scripts
    
    def create_analysis_report(self, 
                              files: List[Dict],
                              sweeps: List[ParameterSweep],
                              templates: List[BatchTemplate],
                              workflow: Dict[str, Any]) -> str:
        """
        Create comprehensive analysis report.
        
        Args:
            files: List of file information
            sweeps: Parameter sweeps
            templates: Batch templates
            workflow: Workflow analysis
            
        Returns:
            Markdown report content
        """
        report = f"""# Analysis Report

Generated: {datetime.now().isoformat()}
Source: {self.source_path}

## Executive Summary

- **Total Files**: {len(files)}
- **Parameter Sweeps**: {len(sweeps)}
- **Batch Templates**: {len(templates)}
- **Workflow Type**: {workflow.get('type', 'Unknown')}

## Parameter Sweeps

"""
        
        for sweep in sweeps:
            report += f"### {sweep.parameter_name}\n\n"
            report += f"- **Type**: {sweep.value_type}\n"
            report += f"- **Values**: {len(sweep.values)}\n"
            
            if sweep.range_info:
                report += f"- **Range**: {sweep.range_info.get('min')} to {sweep.range_info.get('max')}\n"
            
            if sweep.distribution:
                report += f"- **Distribution**: {sweep.distribution}\n"
            
            report += "\n"
        
        report += "## Batch Processing Templates\n\n"
        
        for template in templates:
            report += f"### {template.name}\n\n"
            report += f"- **Pattern**: `{template.pattern}`\n"
            report += f"- **Parameters**: {', '.join(template.parameters.keys())}\n"
            
            if template.execution_order:
                report += f"- **Execution Order**: {' → '.join(template.execution_order)}\n"
            
            report += "\n"
        
        report += f"""## Workflow Analysis

- **Type**: {workflow.get('type')}
- **Stages**: {len(workflow.get('stages', []))}
- **Parallelizable Operations**: {len(workflow.get('parallelizable', []))}
- **Identified Bottlenecks**: {len(workflow.get('bottlenecks', []))}

"""
        
        if workflow.get('stages'):
            report += "### Processing Stages\n\n"
            for i, stage in enumerate(workflow['stages'], 1):
                report += f"{i}. {stage}\n"
            report += "\n"
        
        report += """## Recommendations

"""
        
        recommendations = self._generate_recommendations(sweeps, templates, workflow)
        for rec in recommendations:
            report += f"- {rec}\n"
        
        report += """
## Next Steps

1. Review detected parameter sweeps for accuracy
2. Validate batch processing templates
3. Consider implementing parallel processing for identified operations
4. Use generated scripts for automation

---
*Generated by digitalmodel.modules.automation.go_by_folder*
"""
        
        return report
    
    def _type_values(self, values: List[str]) -> List[Any]:
        """Type convert string values."""
        typed = []
        
        for value in values:
            # Try integer
            try:
                typed.append(int(value))
                continue
            except ValueError:
                pass
            
            # Try float
            try:
                typed.append(float(value))
                continue
            except ValueError:
                pass
            
            # Keep as string
            typed.append(value)
        
        # Check consistency
        types = set(type(v) for v in typed)
        if len(types) > 1:
            # Mixed types, convert all to string
            return values
        
        return typed
    
    def _analyze_range(self, values: List[Any]) -> Optional[Dict]:
        """Analyze numeric range."""
        try:
            numeric_values = [float(v) for v in values]
            return {
                'min': min(numeric_values),
                'max': max(numeric_values),
                'mean': statistics.mean(numeric_values),
                'median': statistics.median(numeric_values),
                'count': len(numeric_values)
            }
        except (ValueError, TypeError):
            return None
    
    def _analyze_distribution(self, values: List[Any]) -> Optional[str]:
        """Analyze value distribution."""
        if len(values) < 3:
            return None
        
        try:
            numeric_values = sorted([float(v) for v in values])
            
            # Check for linear spacing
            diffs = [numeric_values[i+1] - numeric_values[i] 
                    for i in range(len(numeric_values)-1)]
            
            if diffs:
                avg_diff = statistics.mean(diffs)
                std_diff = statistics.stdev(diffs) if len(diffs) > 1 else 0
                
                if std_diff < avg_diff * 0.1:  # Less than 10% variation
                    return 'linear'
            
            # Check for logarithmic spacing
            if all(v > 0 for v in numeric_values):
                import math
                log_values = [math.log10(v) for v in numeric_values]
                log_diffs = [log_values[i+1] - log_values[i] 
                           for i in range(len(log_values)-1)]
                
                if log_diffs:
                    avg_log_diff = statistics.mean(log_diffs)
                    std_log_diff = statistics.stdev(log_diffs) if len(log_diffs) > 1 else 0
                    
                    if std_log_diff < avg_log_diff * 0.1:
                        return 'logarithmic'
            
            return 'irregular'
            
        except (ValueError, TypeError):
            return 'categorical'
    
    def _group_by_pattern(self, files: List[Dict]) -> Dict[str, List[Dict]]:
        """Group files by pattern."""
        groups = defaultdict(list)
        
        for file_info in files:
            # Create pattern by replacing numbers with wildcards
            pattern = re.sub(r'\d+', '*', file_info.get('name', ''))
            groups[pattern].append(file_info)
        
        return dict(groups)
    
    def _determine_execution_order(self, parameters: Dict[str, List]) -> List[str]:
        """Determine optimal execution order for parameters."""
        # Simple heuristic: order by number of values (fewer first)
        return sorted(parameters.keys(), key=lambda k: len(parameters[k]))
    
    def _detect_dependencies(self, files: List[Dict]) -> Dict[str, List[str]]:
        """Detect file dependencies."""
        dependencies = {}
        
        # Simple dependency detection based on timestamps
        sorted_files = sorted(files, key=lambda f: f.get('modified', 0))
        
        for i, file_info in enumerate(sorted_files):
            deps = []
            for j in range(max(0, i-5), i):  # Check previous 5 files
                if self._might_depend_on(file_info, sorted_files[j]):
                    deps.append(sorted_files[j].get('name', ''))
            
            if deps:
                dependencies[file_info.get('name', '')] = deps
        
        return dependencies
    
    def _might_depend_on(self, file1: Dict, file2: Dict) -> bool:
        """Check if file1 might depend on file2."""
        # Simple heuristic based on extensions
        ext1 = file1.get('extension', '')
        ext2 = file2.get('extension', '')
        
        dependencies_map = {
            '.out': ['.in', '.dat', '.cfg'],
            '.log': ['.py', '.sh', '.exe'],
            '.res': ['.sim', '.dat'],
        }
        
        return ext1 in dependencies_map and ext2 in dependencies_map[ext1]
    
    def _categorize_files(self, files: List[Dict]) -> Dict[str, List[Dict]]:
        """Categorize files by type."""
        categories = defaultdict(list)
        
        category_map = {
            'simulation': ['.sim', '.dat', '.res'],
            'script': ['.py', '.sh', '.m', '.r'],
            'config': ['.yml', '.yaml', '.ini', '.cfg'],
            'data': ['.csv', '.txt', '.dat'],
            'output': ['.out', '.log', '.res']
        }
        
        for file_info in files:
            ext = file_info.get('extension', '')
            for category, extensions in category_map.items():
                if ext in extensions:
                    categories[category].append(file_info)
                    break
        
        return dict(categories)
    
    def _analyze_simulation_workflow(self, categories: Dict) -> List[str]:
        """Analyze simulation workflow."""
        stages = []
        
        if 'config' in categories:
            stages.append('Configuration loading')
        if 'data' in categories:
            stages.append('Input data preparation')
        stages.append('Simulation execution')
        if 'output' in categories:
            stages.append('Results extraction')
        stages.append('Post-processing')
        
        return stages
    
    def _analyze_data_workflow(self, categories: Dict) -> List[str]:
        """Analyze data processing workflow."""
        stages = []
        
        stages.append('Data loading')
        if 'script' in categories:
            stages.append('Data transformation')
        stages.append('Analysis')
        if 'output' in categories:
            stages.append('Results generation')
        
        return stages
    
    def _analyze_batch_workflow(self, categories: Dict) -> List[str]:
        """Analyze batch processing workflow."""
        return [
            'Parameter generation',
            'Job submission',
            'Parallel execution',
            'Results collection',
            'Aggregation'
        ]
    
    def _analyze_data_flow(self, files: List[Dict]) -> Dict[str, List[str]]:
        """Analyze data flow between file types."""
        flow = {}
        
        # Simple flow based on common patterns
        ext_groups = defaultdict(list)
        for file_info in files:
            ext_groups[file_info.get('extension', '')].append(file_info)
        
        # Common flows
        flows = [
            ('.dat', '.sim'),
            ('.sim', '.res'),
            ('.csv', '.py'),
            ('.py', '.out'),
        ]
        
        for source, target in flows:
            if source in ext_groups and target in ext_groups:
                flow[f"{source} → {target}"] = [source, target]
        
        return flow
    
    def _identify_parallel_ops(self, stages: List[str]) -> List[str]:
        """Identify parallelizable operations."""
        parallel_keywords = ['execution', 'processing', 'analysis', 'generation']
        
        return [stage for stage in stages 
                if any(keyword in stage.lower() for keyword in parallel_keywords)]
    
    def _detect_bottlenecks(self, files: List[Dict], stages: List[str]) -> List[str]:
        """Detect potential bottlenecks."""
        bottlenecks = []
        
        # Large file processing
        large_files = [f for f in files if f.get('size', 0) > 100 * 1024 * 1024]  # >100MB
        if large_files:
            bottlenecks.append(f"Large file processing ({len(large_files)} files >100MB)")
        
        # Sequential dependencies
        if 'Simulation execution' in stages:
            bottlenecks.append("Sequential simulation execution")
        
        return bottlenecks
    
    def _generate_recommendations(self, 
                                 sweeps: List[ParameterSweep],
                                 templates: List[BatchTemplate],
                                 workflow: Dict) -> List[str]:
        """Generate recommendations."""
        recommendations = []
        
        if len(sweeps) > 2:
            recommendations.append(
                f"Consider parallel processing for {len(sweeps)} parameter sweeps"
            )
        
        if workflow.get('parallelizable'):
            recommendations.append(
                f"Implement parallel execution for {len(workflow['parallelizable'])} stages"
            )
        
        if workflow.get('bottlenecks'):
            recommendations.append(
                "Address identified bottlenecks to improve performance"
            )
        
        if templates:
            recommendations.append(
                f"Use generated batch templates for automation"
            )
        
        return recommendations
    
    def _generate_python_variation_script(self, 
                                         sweeps: List[ParameterSweep],
                                         templates: List[BatchTemplate]) -> str:
        """Generate Python script for variations."""
        if not sweeps:
            return ""
        
        script = '''#!/usr/bin/env python3
"""
Generated variation script for parameter sweeps
"""

import itertools
from pathlib import Path

# Parameter definitions
parameters = {
'''
        
        for sweep in sweeps:
            script += f"    '{sweep.parameter_name}': {sweep.values},\n"
        
        script += '''}

# Generate all combinations
combinations = list(itertools.product(*parameters.values()))
parameter_names = list(parameters.keys())

print(f"Generating {len(combinations)} variations...")

for combo in combinations:
    # Create filename
    param_str = '_'.join([f"{name}={value}" 
                          for name, value in zip(parameter_names, combo)])
    filename = f"variation_{param_str}.dat"
    
    # Create file (customize this part)
    print(f"  Creating: {filename}")
    # Path(filename).touch()

print("Done!")
'''
        
        return script
    
    def _generate_batch_config(self, templates: List[BatchTemplate]) -> str:
        """Generate batch configuration YAML."""
        if not templates:
            return ""
        
        config = {
            'batch_templates': [template.to_dict() for template in templates],
            'execution': {
                'parallel': True,
                'max_workers': 4,
                'timeout': 3600
            }
        }
        
        return yaml.dump(config, default_flow_style=False, sort_keys=False)
    
    def _generate_shell_script(self, templates: List[BatchTemplate]) -> str:
        """Generate shell script for batch execution."""
        if not templates:
            return ""
        
        script = '''#!/bin/bash
# Generated batch execution script

echo "Starting batch processing..."

# Process each template
'''
        
        for template in templates:
            script += f'''
# Template: {template.name}
echo "Processing {template.name}..."
for file in {template.pattern}; do
    echo "  Processing $file"
    # Add your processing command here
    # python process.py "$file"
done
'''
        
        script += '''
echo "Batch processing complete!"
'''
        
        return script