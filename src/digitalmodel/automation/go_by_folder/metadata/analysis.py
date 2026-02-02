"""
Analysis metadata generator for Create Go-By Folder Tool
"""

from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from datetime import datetime
import yaml
import json
import re
import logging

from .generator import BaseMetadataGenerator

logger = logging.getLogger(__name__)


class AnalysisMetadataGenerator(BaseMetadataGenerator):
    """Generate analysis metadata including variation mapping and workflow documentation."""
    
    def generate_metadata(self, scan_results: Dict, patterns: Dict, preservation_stats: Dict) -> Dict:
        """
        Generate analysis metadata files.
        
        Args:
            scan_results: Scanner results
            patterns: Detected patterns
            preservation_stats: Preservation statistics
            
        Returns:
            Dictionary of metadata files created
        """
        # Variation mapping
        variation_mapping = self._create_variation_mapping(patterns, scan_results)
        self.save_metadata(variation_mapping, 'VARIATION_MAPPING.yml')
        
        # Workflow analysis
        workflow_doc = self._create_workflow_analysis(patterns, scan_results)
        self.save_metadata(workflow_doc, 'metadata/WORKFLOW_ANALYSIS.json')
        
        # Batch processing templates
        batch_templates = self._create_batch_templates(patterns)
        self.save_metadata(batch_templates, 'metadata/BATCH_TEMPLATES.yml')
        
        # Parameter sweep analysis
        parameter_sweep = self._analyze_parameter_sweeps(patterns, scan_results)
        self.save_metadata(parameter_sweep, 'metadata/PARAMETER_SWEEP.json')
        
        # Dependency graph
        dependency_graph = self._create_dependency_graph(scan_results)
        self.save_metadata(dependency_graph, 'metadata/DEPENDENCY_GRAPH.json')
        
        return {
            'analysis_metadata_generated': True,
            'files_created': 5
        }
    
    def _create_variation_mapping(self, patterns: Dict, scan_results: Dict) -> Dict:
        """Create VARIATION_MAPPING.yml for parameter analysis."""
        mapping = {
            'metadata': {
                'version': '1.0.0',
                'created': datetime.now().isoformat(),
                'source': str(self.source_path),
                'total_files': scan_results.get('total_files', 0)
            },
            'variations': {},
            'parameters': {},
            'sequences': {},
            'groups': {}
        }
        
        # Process parameter variations
        if 'parameter_variations' in patterns:
            for param, values in patterns['parameter_variations'].items():
                mapping['parameters'][param] = {
                    'type': self._detect_parameter_type(values),
                    'values': values if isinstance(values, list) else [values],
                    'count': len(values) if isinstance(values, list) else 1,
                    'range': self._get_parameter_range(values) if isinstance(values, list) else None
                }
        
        # Process numeric sequences
        if 'numeric_sequences' in patterns:
            for idx, sequence in enumerate(patterns['numeric_sequences']):
                mapping['sequences'][f'sequence_{idx}'] = {
                    'pattern': sequence,
                    'type': 'numeric',
                    'examples': self._extract_sequence_examples(sequence, scan_results)
                }
        
        # Process file groups
        if 'file_groups' in patterns:
            for group_name, files in patterns['file_groups'].items():
                mapping['groups'][group_name] = {
                    'files': files if isinstance(files, list) else [files],
                    'count': len(files) if isinstance(files, list) else 1,
                    'pattern': self._extract_group_pattern(files)
                }
        
        # Analyze variations
        mapping['variations'] = self._analyze_variations(patterns, scan_results)
        
        # Add usage recommendations
        mapping['recommendations'] = self._generate_recommendations(mapping)
        
        return mapping
    
    def _detect_parameter_type(self, values: Any) -> str:
        """Detect the type of parameter values."""
        if not isinstance(values, list) or not values:
            return 'unknown'
        
        # Check if all numeric
        try:
            numeric_values = [float(v) for v in values]
            if all(v.is_integer() for v in numeric_values):
                return 'integer'
            return 'float'
        except (ValueError, TypeError):
            pass
        
        # Check if dates
        date_patterns = [
            r'\d{4}-\d{2}-\d{2}',
            r'\d{2}/\d{2}/\d{4}',
            r'\d{8}'
        ]
        if any(re.match(pattern, str(values[0])) for pattern in date_patterns):
            return 'date'
        
        # Check if boolean-like
        bool_values = {'true', 'false', 'yes', 'no', 'on', 'off', '0', '1'}
        if all(str(v).lower() in bool_values for v in values):
            return 'boolean'
        
        return 'string'
    
    def _get_parameter_range(self, values: List) -> Optional[Dict]:
        """Get range information for parameter values."""
        try:
            numeric_values = [float(v) for v in values]
            return {
                'min': min(numeric_values),
                'max': max(numeric_values),
                'mean': sum(numeric_values) / len(numeric_values),
                'count': len(numeric_values)
            }
        except (ValueError, TypeError):
            return None
    
    def _extract_sequence_examples(self, pattern: str, scan_results: Dict) -> List[str]:
        """Extract example files matching a sequence pattern."""
        examples = []
        # This would need actual file scanning logic
        # For now, return pattern as example
        return [pattern]
    
    def _extract_group_pattern(self, files: List) -> Optional[str]:
        """Extract common pattern from a group of files."""
        if not files or len(files) < 2:
            return None
        
        # Find common prefix
        common_prefix = ''
        for i in range(min(len(f) for f in files)):
            if all(f[i] == files[0][i] for f in files):
                common_prefix += files[0][i]
            else:
                break
        
        # Find common suffix
        common_suffix = ''
        for i in range(1, min(len(f) for f in files) + 1):
            if all(f[-i] == files[0][-i] for f in files):
                common_suffix = files[0][-i] + common_suffix
            else:
                break
        
        if common_prefix or common_suffix:
            return f"{common_prefix}*{common_suffix}"
        
        return None
    
    def _analyze_variations(self, patterns: Dict, scan_results: Dict) -> Dict:
        """Analyze file variations."""
        variations = {
            'total_variations': 0,
            'variation_types': [],
            'common_patterns': [],
            'suggested_templates': []
        }
        
        # Count variations
        if 'parameter_variations' in patterns:
            for param, values in patterns['parameter_variations'].items():
                if isinstance(values, list):
                    variations['total_variations'] += len(values)
                    variations['variation_types'].append({
                        'parameter': param,
                        'count': len(values),
                        'type': self._detect_parameter_type(values)
                    })
        
        # Identify common patterns
        if 'naming_conventions' in patterns:
            naming_conv = patterns['naming_conventions']
            if isinstance(naming_conv, dict):
                variations['common_patterns'] = list(naming_conv.keys())
            elif isinstance(naming_conv, list):
                variations['common_patterns'] = naming_conv
            else:
                variations['common_patterns'] = []
        
        # Suggest templates
        variations['suggested_templates'] = self._suggest_templates(patterns)
        
        return variations
    
    def _suggest_templates(self, patterns: Dict) -> List[Dict]:
        """Suggest template patterns based on analysis."""
        templates = []
        
        # Suggest numeric sequence template
        if 'numeric_sequences' in patterns and patterns['numeric_sequences']:
            templates.append({
                'type': 'numeric_sequence',
                'pattern': 'file_{number:04d}.ext',
                'description': 'Sequential numbering pattern',
                'example': 'file_0001.dat, file_0002.dat, ...'
            })
        
        # Suggest parameter template
        if 'parameter_variations' in patterns and isinstance(patterns['parameter_variations'], dict):
            params = list(patterns['parameter_variations'].keys())[:3]
            if params:
                param_str = '_'.join([f'{{{p}}}' for p in params])
                templates.append({
                    'type': 'parameter_template',
                    'pattern': f'result_{param_str}.out',
                    'description': 'Parameter variation pattern',
                    'parameters': params
                })
        
        return templates
    
    def _generate_recommendations(self, mapping: Dict) -> List[str]:
        """Generate usage recommendations based on analysis."""
        recommendations = []
        
        # Check for batch processing opportunity
        if mapping['sequences'] or mapping['parameters']:
            recommendations.append(
                "Suitable for batch processing - consider using digitalmodel batch tools"
            )
        
        # Check for parameter sweeps
        if 'parameters' in mapping and len(mapping['parameters']) > 1:
            recommendations.append(
                "Multiple parameters detected - suitable for parameter sweep analysis"
            )
        
        # Check for large variations
        total_variations = sum(
            p.get('count', 0) 
            for p in mapping.get('parameters', {}).values()
        )
        if total_variations > 10:
            recommendations.append(
                f"High variation count ({total_variations}) - consider parallel processing"
            )
        
        # Check for grouping
        if mapping.get('groups'):
            recommendations.append(
                "File groups detected - consider group-based processing"
            )
        
        return recommendations
    
    def _create_workflow_analysis(self, patterns: Dict, scan_results: Dict) -> Dict:
        """Create workflow analysis metadata."""
        workflow = {
            'timestamp': datetime.now().isoformat(),
            'source_folder': str(self.source_path),
            'workflow_type': self._detect_workflow_type(patterns, scan_results),
            'processing_stages': [],
            'data_flow': {},
            'dependencies': {},
            'recommendations': []
        }
        
        # Detect processing stages
        file_types = scan_results.get('file_types', {})
        
        # Input stage
        if any(ext in file_types for ext in ['.csv', '.dat', '.txt', '.json']):
            workflow['processing_stages'].append({
                'stage': 'input',
                'file_types': [ext for ext in ['.csv', '.dat', '.txt', '.json'] 
                             if ext in file_types],
                'description': 'Data input files'
            })
        
        # Processing stage
        if any(ext in file_types for ext in ['.py', '.m', '.r', '.jl']):
            workflow['processing_stages'].append({
                'stage': 'processing',
                'file_types': [ext for ext in ['.py', '.m', '.r', '.jl'] 
                             if ext in file_types],
                'description': 'Processing scripts'
            })
        
        # Configuration stage
        if any(ext in file_types for ext in ['.yml', '.yaml', '.ini', '.cfg']):
            workflow['processing_stages'].append({
                'stage': 'configuration',
                'file_types': [ext for ext in ['.yml', '.yaml', '.ini', '.cfg'] 
                             if ext in file_types],
                'description': 'Configuration files'
            })
        
        # Output stage
        if any(ext in file_types for ext in ['.out', '.res', '.log', '.pdf']):
            workflow['processing_stages'].append({
                'stage': 'output',
                'file_types': [ext for ext in ['.out', '.res', '.log', '.pdf'] 
                             if ext in file_types],
                'description': 'Output and result files'
            })
        
        # Analyze data flow
        workflow['data_flow'] = self._analyze_data_flow(workflow['processing_stages'])
        
        # Generate recommendations
        workflow['recommendations'] = self._generate_workflow_recommendations(workflow)
        
        return workflow
    
    def _detect_workflow_type(self, patterns: Dict, scan_results: Dict) -> str:
        """Detect the type of workflow based on patterns and files."""
        file_types = scan_results.get('file_types', {})
        
        # Check for simulation workflow
        if '.sim' in file_types or '.dat' in file_types:
            return 'simulation'
        
        # Check for data analysis workflow
        if '.csv' in file_types and ('.py' in file_types or '.r' in file_types):
            return 'data_analysis'
        
        # Check for batch processing workflow
        if patterns.get('numeric_sequences') or patterns.get('parameter_variations'):
            return 'batch_processing'
        
        # Check for development workflow
        if '.py' in file_types or '.js' in file_types or '.java' in file_types:
            return 'development'
        
        return 'general'
    
    def _analyze_data_flow(self, stages: List[Dict]) -> Dict:
        """Analyze data flow between stages."""
        flow = {
            'input_to_processing': False,
            'configuration_required': False,
            'outputs_generated': False,
            'flow_type': 'unknown'
        }
        
        stage_names = [s['stage'] for s in stages]
        
        if 'input' in stage_names and 'processing' in stage_names:
            flow['input_to_processing'] = True
        
        if 'configuration' in stage_names:
            flow['configuration_required'] = True
        
        if 'output' in stage_names:
            flow['outputs_generated'] = True
        
        # Determine flow type
        if all(s in stage_names for s in ['input', 'processing', 'output']):
            flow['flow_type'] = 'complete_pipeline'
        elif 'processing' in stage_names:
            flow['flow_type'] = 'processing_only'
        else:
            flow['flow_type'] = 'data_only'
        
        return flow
    
    def _generate_workflow_recommendations(self, workflow: Dict) -> List[str]:
        """Generate workflow recommendations."""
        recommendations = []
        
        if workflow['workflow_type'] == 'simulation':
            recommendations.append("Use OrcaFlex batch processing for simulation files")
        
        if workflow['workflow_type'] == 'batch_processing':
            recommendations.append("Implement parallel processing for better performance")
        
        if workflow['data_flow'].get('configuration_required'):
            recommendations.append("Centralize configuration management")
        
        if len(workflow['processing_stages']) > 3:
            recommendations.append("Consider pipeline automation for multi-stage workflow")
        
        return recommendations
    
    def _create_batch_templates(self, patterns: Dict) -> Dict:
        """Create batch processing templates."""
        templates = {
            'version': '1.0.0',
            'created': datetime.now().isoformat(),
            'templates': []
        }
        
        # Template for numeric sequences
        if patterns.get('numeric_sequences'):
            templates['templates'].append({
                'name': 'numeric_sequence_batch',
                'type': 'sequence',
                'pattern': patterns['numeric_sequences'][0] if patterns['numeric_sequences'] else 'file_{n:04d}',
                'config': {
                    'start': 1,
                    'end': 100,
                    'step': 1,
                    'format': '{:04d}'
                },
                'usage': 'Use for processing sequential files'
            })
        
        # Template for parameter variations
        params = patterns.get('parameter_variations', {})
        if params and isinstance(params, dict):
            param_list = list(params.keys())[:3]  # Take first 3 parameters
            
            templates['templates'].append({
                'name': 'parameter_sweep_batch',
                'type': 'parameter_sweep',
                'parameters': {
                    param: {
                        'values': params[param] if isinstance(params[param], list) else [params[param]],
                        'type': self._detect_parameter_type(params[param])
                    }
                    for param in param_list
                },
                'usage': 'Use for parameter sweep analysis'
            })
        
        # Generic batch template
        templates['templates'].append({
            'name': 'generic_batch',
            'type': 'generic',
            'file_pattern': '*.*',
            'filters': {
                'extensions': ['.dat', '.csv', '.txt'],
                'size_limit': '100MB',
                'recursive': True
            },
            'processing': {
                'parallel': True,
                'workers': 4,
                'chunk_size': 10
            },
            'usage': 'Generic batch processing template'
        })
        
        return templates
    
    def _analyze_parameter_sweeps(self, patterns: Dict, scan_results: Dict) -> Dict:
        """Analyze parameter sweep patterns."""
        sweep_analysis = {
            'timestamp': datetime.now().isoformat(),
            'sweep_detected': False,
            'parameters': [],
            'combinations': 0,
            'sweep_type': 'none',
            'visualization': {}
        }
        
        if not patterns.get('parameter_variations'):
            return sweep_analysis
        
        sweep_analysis['sweep_detected'] = True
        params = patterns['parameter_variations']
        
        # Analyze each parameter
        for param_name, values in params.items():
            if not isinstance(values, list):
                values = [values]
            
            param_info = {
                'name': param_name,
                'values': values,
                'count': len(values),
                'type': self._detect_parameter_type(values),
                'range': self._get_parameter_range(values)
            }
            sweep_analysis['parameters'].append(param_info)
        
        # Calculate combinations
        if sweep_analysis['parameters']:
            combinations = 1
            for param in sweep_analysis['parameters']:
                combinations *= param['count']
            sweep_analysis['combinations'] = combinations
        
        # Determine sweep type
        if len(sweep_analysis['parameters']) == 1:
            sweep_analysis['sweep_type'] = 'single_parameter'
        elif len(sweep_analysis['parameters']) == 2:
            sweep_analysis['sweep_type'] = 'two_parameter'
        elif len(sweep_analysis['parameters']) > 2:
            sweep_analysis['sweep_type'] = 'multi_parameter'
        
        # Suggest visualization
        sweep_analysis['visualization'] = self._suggest_visualization(sweep_analysis)
        
        return sweep_analysis
    
    def _suggest_visualization(self, sweep_analysis: Dict) -> Dict:
        """Suggest visualization for parameter sweep."""
        viz = {
            'recommended_plots': [],
            'data_structure': 'tabular',
            'tools': []
        }
        
        sweep_type = sweep_analysis['sweep_type']
        
        if sweep_type == 'single_parameter':
            viz['recommended_plots'] = ['line_plot', 'scatter_plot']
            viz['tools'] = ['matplotlib', 'plotly']
        elif sweep_type == 'two_parameter':
            viz['recommended_plots'] = ['heatmap', 'contour_plot', '3d_surface']
            viz['tools'] = ['matplotlib', 'plotly', 'seaborn']
        elif sweep_type == 'multi_parameter':
            viz['recommended_plots'] = ['parallel_coordinates', 'scatter_matrix']
            viz['tools'] = ['plotly', 'pandas']
            viz['data_structure'] = 'multi_dimensional'
        
        return viz
    
    def _create_dependency_graph(self, scan_results: Dict) -> Dict:
        """Create dependency graph for files."""
        graph = {
            'timestamp': datetime.now().isoformat(),
            'nodes': [],
            'edges': [],
            'clusters': {},
            'metrics': {}
        }
        
        # Create nodes for file types
        file_types = scan_results.get('file_types', {})
        for ext, info in file_types.items():
            count = info.get('count', 0) if isinstance(info, dict) else info
            graph['nodes'].append({
                'id': ext,
                'type': 'file_type',
                'count': count,
                'category': self.get_file_category(ext)
            })
        
        # Create edges based on typical dependencies
        dependencies = [
            ('.yml', '.py'),   # Config to script
            ('.csv', '.py'),   # Data to script
            ('.py', '.log'),   # Script to log
            ('.py', '.out'),   # Script to output
            ('.dat', '.sim'),  # Data to simulation
        ]
        
        for source, target in dependencies:
            if source in file_types and target in file_types:
                graph['edges'].append({
                    'source': source,
                    'target': target,
                    'type': 'dependency'
                })
        
        # Create clusters by category
        categories = {}
        for ext in file_types:
            category = self.get_file_category(ext)
            if category not in categories:
                categories[category] = []
            categories[category].append(ext)
        
        graph['clusters'] = categories
        
        # Calculate metrics
        graph['metrics'] = {
            'total_nodes': len(graph['nodes']),
            'total_edges': len(graph['edges']),
            'clusters': len(categories),
            'isolated_nodes': len([n for n in graph['nodes'] 
                                 if not any(e['source'] == n['id'] or e['target'] == n['id'] 
                                          for e in graph['edges'])])
        }
        
        return graph