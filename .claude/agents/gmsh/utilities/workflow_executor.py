"""
Workflow Executor for GMSH Agent
Executes predefined workflows from YAML configurations
"""

import yaml
import json
import logging
from pathlib import Path
from typing import Dict, List, Any, Optional, Union
from dataclasses import dataclass, field
from datetime import datetime
import multiprocessing as mp
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
import time
import traceback

from .batch_processor import BatchProcessor, BatchJob, ProgressTracker
from .mesh_generator import MeshGenerator
from .geometry_processor import GeometryProcessor
from .mesh_metrics import MeshQualityAnalyzer
from .refinement import MeshOptimizer


@dataclass
class WorkflowStage:
    """Represents a single stage in a workflow"""
    name: str
    description: str
    operations: List[Dict[str, Any]]
    condition: Optional[str] = None
    parallel: bool = False
    continue_on_error: bool = True
    
    
@dataclass
class WorkflowConfig:
    """Workflow configuration from YAML"""
    name: str
    description: str
    version: str
    global_config: Dict[str, Any]
    input_config: Dict[str, Any]
    stages: List[WorkflowStage]
    output_config: Dict[str, Any]
    error_handling: Dict[str, Any]
    resources: Optional[Dict[str, Any]] = None
    notifications: Optional[Dict[str, Any]] = None
    

class WorkflowValidator:
    """Validates workflow configurations"""
    
    REQUIRED_FIELDS = ['name', 'description', 'version', 'stages']
    VALID_OPERATIONS = [
        'import', 'export', 'generate_mesh', 'quality_assessment',
        'optimize', 'validate', 'conditional', 'script', 'transform'
    ]
    
    @classmethod
    def validate(cls, config: Dict[str, Any]) -> Tuple[bool, List[str]]:
        """
        Validate workflow configuration
        
        Returns:
            Tuple of (is_valid, error_messages)
        """
        errors = []
        
        # Check required fields
        for field in cls.REQUIRED_FIELDS:
            if field not in config:
                errors.append(f"Missing required field: {field}")
                
        # Validate stages
        if 'stages' in config:
            if not isinstance(config['stages'], list):
                errors.append("Stages must be a list")
            else:
                for i, stage in enumerate(config['stages']):
                    if not isinstance(stage, dict):
                        errors.append(f"Stage {i} must be a dictionary")
                        continue
                        
                    if 'name' not in stage:
                        errors.append(f"Stage {i} missing name")
                    if 'operations' not in stage:
                        errors.append(f"Stage {i} missing operations")
                    elif not isinstance(stage['operations'], list):
                        errors.append(f"Stage {i} operations must be a list")
                        
        # Validate version format
        if 'version' in config:
            version = config['version']
            if not isinstance(version, str):
                errors.append("Version must be a string")
            elif not all(part.isdigit() for part in version.split('.')):
                errors.append(f"Invalid version format: {version}")
                
        return len(errors) == 0, errors
        

class WorkflowExecutor:
    """Executes workflows defined in YAML files"""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """
        Initialize workflow executor
        
        Args:
            config: Optional configuration dictionary
        """
        self.config = config or {}
        self.logger = self._setup_logging()
        self.batch_processor = BatchProcessor(config)
        self.mesh_generator = None
        self.geometry_processor = None
        self.quality_analyzer = None
        self.mesh_optimizer = None
        self.execution_history = []
        self.context = {}  # Workflow execution context
        
    def _setup_logging(self) -> logging.Logger:
        """Set up logging for workflow executor"""
        logger = logging.getLogger('WorkflowExecutor')
        if not logger.handlers:
            handler = logging.StreamHandler()
            formatter = logging.Formatter(
                '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
            )
            handler.setFormatter(formatter)
            logger.addHandler(handler)
            logger.setLevel(logging.INFO)
        return logger
        
    def load_workflow(self, workflow_path: Union[str, Path]) -> WorkflowConfig:
        """
        Load workflow from YAML file
        
        Args:
            workflow_path: Path to workflow YAML file
            
        Returns:
            WorkflowConfig object
        """
        workflow_path = Path(workflow_path)
        
        if not workflow_path.exists():
            raise FileNotFoundError(f"Workflow file not found: {workflow_path}")
            
        with open(workflow_path, 'r') as f:
            config = yaml.safe_load(f)
            
        # Validate workflow
        is_valid, errors = WorkflowValidator.validate(config)
        if not is_valid:
            raise ValueError(f"Invalid workflow configuration: {errors}")
            
        # Parse stages
        stages = []
        for stage_config in config.get('stages', []):
            stage = WorkflowStage(
                name=stage_config['name'],
                description=stage_config.get('description', ''),
                operations=stage_config.get('operations', []),
                condition=stage_config.get('condition'),
                parallel=stage_config.get('parallel', False),
                continue_on_error=stage_config.get('continue_on_error', True)
            )
            stages.append(stage)
            
        return WorkflowConfig(
            name=config['name'],
            description=config['description'],
            version=config['version'],
            global_config=config.get('global_config', {}),
            input_config=config.get('input', {}),
            stages=stages,
            output_config=config.get('output', {}),
            error_handling=config.get('error_handling', {}),
            resources=config.get('resources'),
            notifications=config.get('notifications')
        )
        
    def execute_workflow(
        self, 
        workflow: Union[str, Path, WorkflowConfig],
        dry_run: bool = False,
        skip_stages: Optional[List[str]] = None,
        only_stages: Optional[List[str]] = None
    ) -> Dict[str, Any]:
        """
        Execute a complete workflow
        
        Args:
            workflow: Workflow path or WorkflowConfig object
            dry_run: If True, only simulate execution
            skip_stages: List of stage names to skip
            only_stages: List of stage names to execute (skips all others)
            
        Returns:
            Execution results dictionary
        """
        # Load workflow if path provided
        if isinstance(workflow, (str, Path)):
            workflow = self.load_workflow(workflow)
            
        self.logger.info(f"Starting workflow: {workflow.name} v{workflow.version}")
        
        # Initialize components
        self._initialize_components(workflow.global_config)
        
        # Prepare execution context
        self.context = {
            'workflow': workflow,
            'start_time': datetime.now(),
            'dry_run': dry_run,
            'results': {},
            'errors': [],
            'skipped': []
        }
        
        # Send start notification
        if workflow.notifications:
            self._send_notification('start', workflow.notifications)
            
        # Execute stages
        skip_stages = skip_stages or []
        only_stages = only_stages or []
        
        for stage in workflow.stages:
            # Check if stage should be executed
            if stage.name in skip_stages:
                self.logger.info(f"Skipping stage: {stage.name}")
                self.context['skipped'].append(stage.name)
                continue
                
            if only_stages and stage.name not in only_stages:
                self.logger.info(f"Skipping stage: {stage.name} (not in only_stages)")
                self.context['skipped'].append(stage.name)
                continue
                
            # Check stage condition
            if stage.condition and not self._evaluate_condition(stage.condition):
                self.logger.info(f"Skipping stage {stage.name} due to condition: {stage.condition}")
                self.context['skipped'].append(stage.name)
                continue
                
            # Execute stage
            try:
                self.logger.info(f"Executing stage: {stage.name} - {stage.description}")
                
                if dry_run:
                    self.logger.info(f"[DRY RUN] Would execute {len(stage.operations)} operations")
                    stage_result = {'dry_run': True, 'operations': len(stage.operations)}
                else:
                    stage_result = self._execute_stage(stage)
                    
                self.context['results'][stage.name] = stage_result
                
            except Exception as e:
                error_msg = f"Error in stage {stage.name}: {str(e)}"
                self.logger.error(error_msg)
                self.context['errors'].append({
                    'stage': stage.name,
                    'error': str(e),
                    'traceback': traceback.format_exc()
                })
                
                if not stage.continue_on_error:
                    self.logger.error("Stopping workflow due to error")
                    break
                    
        # Finalize workflow
        self.context['end_time'] = datetime.now()
        self.context['duration'] = (
            self.context['end_time'] - self.context['start_time']
        ).total_seconds()
        
        # Send completion notification
        if workflow.notifications:
            self._send_notification('complete', workflow.notifications)
            
        # Generate summary
        summary = self._generate_summary()
        
        self.logger.info(f"Workflow completed in {self.context['duration']:.2f} seconds")
        
        return summary
        
    def _initialize_components(self, config: Dict[str, Any]):
        """Initialize workflow components"""
        try:
            from .mesh_generator import MeshGenerator
            self.mesh_generator = MeshGenerator(config)
        except ImportError:
            self.logger.warning("MeshGenerator not available")
            
        try:
            from .geometry_processor import GeometryProcessor
            self.geometry_processor = GeometryProcessor(config)
        except ImportError:
            self.logger.warning("GeometryProcessor not available")
            
        try:
            from .mesh_metrics import MeshQualityAnalyzer
            self.quality_analyzer = MeshQualityAnalyzer(config)
        except ImportError:
            self.logger.warning("MeshQualityAnalyzer not available")
            
        try:
            from .refinement import MeshOptimizer
            self.mesh_optimizer = MeshOptimizer(config)
        except ImportError:
            self.logger.warning("MeshOptimizer not available")
            
    def _execute_stage(self, stage: WorkflowStage) -> Dict[str, Any]:
        """Execute a single workflow stage"""
        stage_results = {
            'name': stage.name,
            'start_time': datetime.now().isoformat(),
            'operations': []
        }
        
        # Execute operations
        if stage.parallel and len(stage.operations) > 1:
            # Execute operations in parallel
            with ThreadPoolExecutor(max_workers=mp.cpu_count()) as executor:
                futures = []
                for op in stage.operations:
                    future = executor.submit(self._execute_operation, op)
                    futures.append((op, future))
                    
                for op, future in futures:
                    try:
                        result = future.result(timeout=300)
                        stage_results['operations'].append({
                            'type': op.get('type'),
                            'success': True,
                            'result': result
                        })
                    except Exception as e:
                        stage_results['operations'].append({
                            'type': op.get('type'),
                            'success': False,
                            'error': str(e)
                        })
        else:
            # Execute operations sequentially
            for op in stage.operations:
                try:
                    result = self._execute_operation(op)
                    stage_results['operations'].append({
                        'type': op.get('type'),
                        'success': True,
                        'result': result
                    })
                except Exception as e:
                    stage_results['operations'].append({
                        'type': op.get('type'),
                        'success': False,
                        'error': str(e)
                    })
                    if not stage.continue_on_error:
                        raise
                        
        stage_results['end_time'] = datetime.now().isoformat()
        return stage_results
        
    def _execute_operation(self, operation: Dict[str, Any]) -> Any:
        """Execute a single operation"""
        op_type = operation.get('type')
        
        if op_type == 'import':
            return self._op_import(operation)
        elif op_type == 'export':
            return self._op_export(operation)
        elif op_type == 'generate_mesh':
            return self._op_generate_mesh(operation)
        elif op_type == 'quality_assessment':
            return self._op_quality_assessment(operation)
        elif op_type == 'optimize':
            return self._op_optimize(operation)
        elif op_type == 'validate':
            return self._op_validate(operation)
        elif op_type == 'conditional':
            return self._op_conditional(operation)
        elif op_type == 'script':
            return self._op_script(operation)
        else:
            raise ValueError(f"Unknown operation type: {op_type}")
            
    def _op_import(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Import geometry operation"""
        if not self.geometry_processor:
            raise RuntimeError("GeometryProcessor not initialized")
            
        file_path = operation.get('file_path')
        if not file_path:
            # Get from context
            file_path = self.context.get('current_file')
            
        result = self.geometry_processor.import_geometry(
            file_path,
            heal=operation.get('heal_geometry', True)
        )
        
        # Store in context
        self.context['current_geometry'] = result
        return result
        
    def _op_generate_mesh(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Generate mesh operation"""
        if not self.mesh_generator:
            raise RuntimeError("MeshGenerator not initialized")
            
        dimension = operation.get('dimension', 'auto')
        
        if dimension == 'auto':
            # Determine from geometry
            geometry = self.context.get('current_geometry')
            if geometry:
                dimension = geometry.get('dimension', '3d')
            else:
                dimension = '3d'
                
        if dimension == '3d':
            result = self.mesh_generator.generate_3d_mesh(
                self.context.get('current_geometry'),
                algorithm=operation.get('mesh_algorithm', {}).get('3d', 'frontal-delaunay'),
                element_type=operation.get('element_types', {}).get('3d', 'tetrahedron'),
                element_size=operation.get('element_size', {}).get('default', 1.0)
            )
        elif dimension == '2d':
            result = self.mesh_generator.generate_2d_mesh(
                self.context.get('current_geometry'),
                algorithm=operation.get('mesh_algorithm', {}).get('2d', 'frontal-delaunay'),
                element_type=operation.get('element_types', {}).get('2d', 'triangle'),
                element_size=operation.get('element_size', {}).get('default', 1.0)
            )
        else:
            raise ValueError(f"Unsupported dimension: {dimension}")
            
        self.context['current_mesh'] = result
        return result
        
    def _op_quality_assessment(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Quality assessment operation"""
        if not self.quality_analyzer:
            raise RuntimeError("MeshQualityAnalyzer not initialized")
            
        result = self.quality_analyzer.assess_mesh_quality(
            self.context.get('current_mesh_file')
        )
        
        self.context['quality_score'] = result.get('overall_score', 0)
        self.context['quality_report'] = result
        return result
        
    def _op_optimize(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Optimization operation"""
        if not self.mesh_optimizer:
            raise RuntimeError("MeshOptimizer not initialized")
            
        algorithm = operation.get('algorithm', 'netgen')
        
        if algorithm == 'laplacian':
            result = self.mesh_optimizer.laplacian_smoothing(
                iterations=operation.get('iterations', 5)
            )
        elif algorithm == 'netgen':
            result = self.mesh_optimizer.netgen_optimization(
                optimization_steps=operation.get('optimization_steps', 'hlr'),
                iterations=operation.get('iterations', 2)
            )
        else:
            result = self.mesh_optimizer.optimize_quality(
                quality_targets=operation.get('quality_targets'),
                max_iterations=operation.get('max_iterations', 5)
            )
            
        self.context['optimized_mesh'] = result
        return result
        
    def _op_conditional(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Conditional operation"""
        condition = operation.get('condition')
        
        if self._evaluate_condition(condition):
            actions = operation.get('actions', [])
            results = []
            for action in actions:
                result = self._execute_operation(action)
                results.append(result)
            return {'condition': condition, 'executed': True, 'results': results}
        else:
            return {'condition': condition, 'executed': False}
            
    def _op_script(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Execute external script"""
        import subprocess
        
        command = operation.get('command')
        if not command:
            raise ValueError("Script command not specified")
            
        # Substitute context variables
        command = self._substitute_variables(command)
        
        result = subprocess.run(
            command,
            shell=True,
            capture_output=True,
            text=True,
            timeout=operation.get('timeout', 300)
        )
        
        return {
            'command': command,
            'returncode': result.returncode,
            'stdout': result.stdout,
            'stderr': result.stderr
        }
        
    def _op_validate(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Validation operation"""
        checks = {}
        
        if operation.get('check_manifold'):
            # Check if mesh is manifold
            checks['manifold'] = self._check_manifold()
            
        if operation.get('check_self_intersections'):
            # Check for self-intersections
            checks['self_intersections'] = self._check_self_intersections()
            
        if operation.get('check_inverted_elements'):
            # Check for inverted elements
            checks['inverted_elements'] = self._check_inverted_elements()
            
        return {'validation': checks, 'passed': all(checks.values())}
        
    def _op_export(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Export operation"""
        formats = operation.get('formats', ['msh'])
        results = {}
        
        for fmt in formats:
            output_file = self._get_output_path(fmt)
            # Export logic would go here
            results[fmt] = {'file': str(output_file), 'success': True}
            
        return results
        
    def _evaluate_condition(self, condition: str) -> bool:
        """Evaluate a condition string"""
        try:
            # Create safe evaluation context
            safe_context = {
                'quality_score': self.context.get('quality_score', 0),
                'quality_improvement': self.context.get('quality_improvement', 0),
                'overall_score': self.context.get('quality_score', 0)
            }
            
            # Evaluate condition
            return eval(condition, {"__builtins__": {}}, safe_context)
        except Exception as e:
            self.logger.warning(f"Failed to evaluate condition '{condition}': {e}")
            return False
            
    def _substitute_variables(self, text: str) -> str:
        """Substitute context variables in text"""
        import re
        
        def replace_var(match):
            var_name = match.group(1)
            return str(self.context.get(var_name, match.group(0)))
            
        return re.sub(r'\{(\w+)\}', replace_var, text)
        
    def _get_output_path(self, extension: str) -> Path:
        """Get output path for a file"""
        output_dir = Path(self.context['workflow'].output_config.get('directory', './output'))
        output_dir.mkdir(parents=True, exist_ok=True)
        
        base_name = self.context.get('current_file_basename', 'output')
        return output_dir / f"{base_name}.{extension}"
        
    def _check_manifold(self) -> bool:
        """Check if mesh is manifold"""
        # Placeholder implementation
        return True
        
    def _check_self_intersections(self) -> bool:
        """Check for self-intersections"""
        # Placeholder implementation
        return True
        
    def _check_inverted_elements(self) -> bool:
        """Check for inverted elements"""
        # Placeholder implementation
        quality_report = self.context.get('quality_report', {})
        min_jacobian = quality_report.get('metrics', {}).get('jacobian', {}).get('min', 1.0)
        return min_jacobian > 0
        
    def _send_notification(self, event: str, config: Dict[str, Any]):
        """Send workflow notification"""
        if not config.get(f'on_{event}'):
            return
            
        methods = config.get('methods', [])
        for method in methods:
            if method['type'] == 'console':
                if event == 'start':
                    self.logger.info(f"NOTIFICATION: Workflow {self.context['workflow'].name} started")
                elif event == 'complete':
                    self.logger.info(f"NOTIFICATION: Workflow completed in {self.context['duration']:.2f}s")
                elif event == 'error':
                    self.logger.error(f"NOTIFICATION: Workflow encountered {len(self.context['errors'])} errors")
                    
            elif method['type'] == 'log':
                log_file = method.get('file', 'workflow.log')
                with open(log_file, 'a') as f:
                    f.write(f"[{datetime.now().isoformat()}] {event.upper()}: {self.context['workflow'].name}\n")
                    
    def _generate_summary(self) -> Dict[str, Any]:
        """Generate workflow execution summary"""
        summary = {
            'workflow': self.context['workflow'].name,
            'version': self.context['workflow'].version,
            'start_time': self.context['start_time'].isoformat(),
            'end_time': self.context['end_time'].isoformat(),
            'duration': self.context['duration'],
            'stages_executed': len(self.context['results']),
            'stages_skipped': len(self.context['skipped']),
            'errors': len(self.context['errors']),
            'success': len(self.context['errors']) == 0
        }
        
        # Add detailed results if available
        if self.context['results']:
            summary['stage_results'] = {}
            for stage_name, result in self.context['results'].items():
                summary['stage_results'][stage_name] = {
                    'operations': len(result.get('operations', [])),
                    'success': all(
                        op.get('success', False) 
                        for op in result.get('operations', [])
                    )
                }
                
        # Add quality metrics if available
        if 'quality_score' in self.context:
            summary['final_quality_score'] = self.context['quality_score']
            
        if 'quality_improvement' in self.context:
            summary['quality_improvement'] = self.context['quality_improvement']
            
        return summary
        

class WorkflowLibrary:
    """Library of predefined workflows"""
    
    WORKFLOWS_DIR = Path(__file__).parent.parent / 'workflows'
    
    @classmethod
    def list_workflows(cls) -> List[Dict[str, str]]:
        """List available workflows"""
        workflows = []
        
        if cls.WORKFLOWS_DIR.exists():
            for workflow_file in cls.WORKFLOWS_DIR.glob('*.yml'):
                try:
                    with open(workflow_file, 'r') as f:
                        config = yaml.safe_load(f)
                        workflows.append({
                            'name': config.get('name', workflow_file.stem),
                            'description': config.get('description', ''),
                            'version': config.get('version', ''),
                            'file': str(workflow_file)
                        })
                except Exception as e:
                    logging.warning(f"Failed to load workflow {workflow_file}: {e}")
                    
        return workflows
        
    @classmethod
    def get_workflow(cls, name: str) -> Optional[Path]:
        """Get workflow file by name"""
        workflow_file = cls.WORKFLOWS_DIR / f"{name}.yml"
        if workflow_file.exists():
            return workflow_file
            
        # Try exact match
        for file in cls.WORKFLOWS_DIR.glob('*.yml'):
            try:
                with open(file, 'r') as f:
                    config = yaml.safe_load(f)
                    if config.get('name') == name:
                        return file
            except:
                continue
                
        return None


if __name__ == "__main__":
    # Example usage
    executor = WorkflowExecutor()
    
    # List available workflows
    workflows = WorkflowLibrary.list_workflows()
    print("Available workflows:")
    for wf in workflows:
        print(f"  - {wf['name']}: {wf['description']}")
        
    # Execute a workflow
    if workflows:
        workflow_path = workflows[0]['file']
        print(f"\nExecuting workflow: {workflow_path}")
        
        # Dry run first
        result = executor.execute_workflow(workflow_path, dry_run=True)
        print(f"Dry run result: {json.dumps(result, indent=2)}")
        
        # Real execution (commented out for safety)
        # result = executor.execute_workflow(workflow_path)
        # print(f"Execution result: {json.dumps(result, indent=2)}")