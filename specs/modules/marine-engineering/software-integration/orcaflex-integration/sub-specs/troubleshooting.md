# OrcaFlex Troubleshooting Framework - Technical Specification

## Overview

This specification provides a comprehensive framework for troubleshooting OrcaFlex integration issues, with emphasis on systematic error diagnosis, automated resolution, and prevention strategies. It consolidates proven methodologies for handling common OrcaFlex API issues, model validation problems, and workflow failures.

## Core Troubleshooting Framework

### Primary Issue Types

#### 1. Missing Object Errors (`NoneType` Attribute Errors)
**Error Pattern**: `'NoneType' object has no attribute 'name'`  
**Root Cause**: OrcaFlex models with missing, damaged, or incomplete objects  
**Impact**: Analysis failure, batch processing interruption

#### 2. Configuration Errors  
**Error Pattern**: `KeyError: 'summary_settings'`, Configuration validation failures  
**Root Cause**: Incomplete or malformed YAML configuration files  
**Impact**: Workflow initialization failure, incorrect analysis parameters

#### 3. API Integration Errors
**Error Pattern**: OrcaFlex API connection failures, method call errors  
**Root Cause**: Version incompatibilities, licensing issues, installation problems  
**Impact**: Complete integration failure, unable to load models

#### 4. Performance and Resource Issues
**Error Pattern**: Memory exhaustion, timeout errors, processing slowdowns  
**Root Cause**: Large models, insufficient resources, inefficient algorithms  
**Impact**: Analysis completion failure, system instability

## Systematic Diagnosis Framework

### Phase 1: Error Classification and Initial Assessment

```python
class OrcaFlexErrorClassifier:
    """Systematic error classification for OrcaFlex issues."""
    
    def __init__(self):
        self.error_patterns = {
            'missing_object': {
                'patterns': [
                    r"'NoneType' object has no attribute 'name'",
                    r"'NoneType' object has no attribute 'type'",
                    r"Object .* not found in model"
                ],
                'severity': 'critical',
                'category': 'model_integrity'
            },
            'configuration': {
                'patterns': [
                    r"KeyError: 'summary_settings'",
                    r"KeyError: 'parameters'",
                    r"Invalid configuration structure"
                ],
                'severity': 'high',
                'category': 'configuration'
            },
            'api_integration': {
                'patterns': [
                    r"OrcaFlex API not available",
                    r"Failed to load OrcaFlex model",
                    r"License check failed"
                ],
                'severity': 'critical', 
                'category': 'integration'
            },
            'performance': {
                'patterns': [
                    r"TimeoutError",
                    r"MemoryError",
                    r"Processing timeout exceeded"
                ],
                'severity': 'medium',
                'category': 'performance'
            }
        }
    
    def classify_error(self, error_message: str, context: Dict) -> ErrorClassification:
        """Classify error and provide initial diagnosis."""
        
        for error_type, config in self.error_patterns.items():
            for pattern in config['patterns']:
                if re.search(pattern, error_message, re.IGNORECASE):
                    return ErrorClassification(
                        type=error_type,
                        severity=config['severity'],
                        category=config['category'],
                        pattern_matched=pattern,
                        context=context,
                        diagnosis=self._generate_initial_diagnosis(error_type, context)
                    )
        
        return ErrorClassification(
            type='unknown',
            severity='medium',
            category='general',
            pattern_matched=None,
            context=context,
            diagnosis=self._generate_generic_diagnosis(error_message, context)
        )
```

### Phase 2: Detailed Root Cause Analysis

```python
class OrcaFlexRootCauseAnalyzer:
    """Deep root cause analysis for OrcaFlex issues."""
    
    def analyze_missing_object_error(self, error_context: ErrorContext) -> RootCauseReport:
        """Comprehensive analysis of missing object errors."""
        
        analysis_steps = []
        findings = []
        
        # Step 1: Model structure analysis
        analysis_steps.append("Analyzing OrcaFlex model structure...")
        model_structure = self._analyze_model_structure(error_context.model_path)
        findings.append(f"Model contains {model_structure.total_objects} objects")
        
        # Step 2: Object integrity checking
        analysis_steps.append("Checking object integrity...")
        integrity_report = self._check_object_integrity(error_context.model_path)
        findings.extend(integrity_report.issues)
        
        # Step 3: Configuration compatibility analysis
        analysis_steps.append("Analyzing configuration compatibility...")
        config_compatibility = self._analyze_config_compatibility(
            error_context.configuration, model_structure
        )
        findings.extend(config_compatibility.compatibility_issues)
        
        # Step 4: Code path analysis
        analysis_steps.append("Analyzing error location in code...")
        code_analysis = self._analyze_error_location(error_context.stack_trace)
        findings.extend(code_analysis.problematic_patterns)
        
        return RootCauseReport(
            primary_cause=self._determine_primary_cause(findings),
            contributing_factors=self._identify_contributing_factors(findings),
            analysis_steps=analysis_steps,
            detailed_findings=findings,
            recommended_solutions=self._generate_solutions(findings)
        )
    
    def _analyze_model_structure(self, model_path: str) -> ModelStructureReport:
        """Analyze OrcaFlex model structure and object availability."""
        
        try:
            import OrcFxAPI
            model = OrcFxAPI.Model(model_path)
            
            structure_report = ModelStructureReport()
            
            # Analyze different object types
            object_types = ['Line', 'Vessel', 'Winch', 'Link', '6DBuoy', 'Constraint']
            
            for obj_type in object_types:
                try:
                    objects = model.objects(obj_type)
                    valid_objects = []
                    invalid_objects = []
                    
                    for obj in objects:
                        try:
                            # Test object integrity
                            if obj is not None and hasattr(obj, 'type') and obj.type is not None:
                                if hasattr(obj.type, 'name') and obj.type.name is not None:
                                    valid_objects.append(obj.name)
                                else:
                                    invalid_objects.append(f"Object with invalid type.name")
                            else:
                                invalid_objects.append(f"Object with invalid type")
                        except Exception as e:
                            invalid_objects.append(f"Object error: {str(e)}")
                    
                    structure_report.object_types[obj_type] = ObjectTypeReport(
                        total_count=len(objects),
                        valid_objects=valid_objects,
                        invalid_objects=invalid_objects
                    )
                    
                except Exception as e:
                    structure_report.object_types[obj_type] = ObjectTypeReport(
                        total_count=0,
                        valid_objects=[],
                        invalid_objects=[f"Failed to access {obj_type} objects: {str(e)}"]
                    )
            
            return structure_report
            
        except Exception as e:
            return ModelStructureReport(
                error=f"Failed to load model: {str(e)}",
                object_types={}
            )
```

### Phase 3: Automated Resolution Framework

```python
class OrcaFlexAutoResolver:
    """Automated resolution system for common OrcaFlex issues."""
    
    def __init__(self):
        self.resolution_strategies = {
            'missing_object': [
                self._apply_safe_object_access_pattern,
                self._implement_object_validation,
                self._add_error_recovery_mechanisms
            ],
            'configuration': [
                self._validate_configuration_structure,
                self._generate_missing_sections,
                self._apply_configuration_templates
            ],
            'api_integration': [
                self._verify_orcaflex_installation,
                self._check_license_status,
                self._test_api_connectivity
            ],
            'performance': [
                self._optimize_memory_usage,
                self._implement_timeout_handling,
                self._add_progress_monitoring
            ]
        }
    
    def resolve_missing_object_error(self, error_context: ErrorContext) -> ResolutionResult:
        """Automated resolution for missing object errors."""
        
        resolution_steps = []
        applied_fixes = []
        
        # Fix 1: Implement safe object access patterns
        resolution_steps.append("Implementing safe object access patterns...")
        safe_access_result = self._apply_safe_object_access_pattern(error_context)
        if safe_access_result.success:
            applied_fixes.append("Safe object access patterns implemented")
        
        # Fix 2: Add object validation
        resolution_steps.append("Adding object validation mechanisms...")
        validation_result = self._implement_object_validation(error_context)
        if validation_result.success:
            applied_fixes.append("Object validation mechanisms added")
        
        # Fix 3: Implement error recovery
        resolution_steps.append("Implementing error recovery mechanisms...")
        recovery_result = self._add_error_recovery_mechanisms(error_context)
        if recovery_result.success:
            applied_fixes.append("Error recovery mechanisms implemented")
        
        # Test resolution
        resolution_steps.append("Testing resolution effectiveness...")
        test_result = self._test_resolution(error_context, applied_fixes)
        
        return ResolutionResult(
            success=test_result.success,
            applied_fixes=applied_fixes,
            resolution_steps=resolution_steps,
            test_results=test_result,
            remaining_issues=test_result.remaining_issues if not test_result.success else []
        )
    
    def _apply_safe_object_access_pattern(self, error_context: ErrorContext) -> FixResult:
        """Apply safe object access patterns to prevent NoneType errors."""
        
        code_modifications = []
        
        # Identify files that need modification
        target_files = [
            'src/digitalmodel/modules/orcaflex/orcaflex_objects.py',
            'src/digitalmodel/modules/orcaflex/all_vars.py'
        ]
        
        for file_path in target_files:
            if os.path.exists(file_path):
                modifications = self._modify_file_for_safe_access(file_path)
                code_modifications.extend(modifications)
        
        return FixResult(
            success=len(code_modifications) > 0,
            modifications=code_modifications,
            description="Applied safe object access patterns to prevent NoneType errors"
        )
    
    def _modify_file_for_safe_access(self, file_path: str) -> List[CodeModification]:
        """Modify specific file to implement safe object access."""
        
        modifications = []
        
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Pattern 1: Replace direct object.type.name access
        pattern_1 = r'object\.type\.name'
        replacement_1 = '''(object.type.name if (object is not None and 
                           hasattr(object, 'type') and object.type is not None and 
                           hasattr(object.type, 'name') and object.type.name is not None) 
                           else None)'''
        
        if re.search(pattern_1, content):
            content = re.sub(pattern_1, replacement_1, content)
            modifications.append(CodeModification(
                pattern=pattern_1,
                replacement=replacement_1,
                description="Safe object.type.name access"
            ))
        
        # Pattern 2: Add try-catch blocks around object iterations
        pattern_2 = r'for\s+(\w+)\s+in\s+(\w+):'
        if re.search(pattern_2, content):
            # This would require more sophisticated parsing
            # For now, document the need for manual review
            modifications.append(CodeModification(
                pattern=pattern_2,
                replacement="Manual review required",
                description="Add try-catch blocks around object iterations"
            ))
        
        # Write modified content back
        if modifications:
            with open(file_path, 'w') as f:
                f.write(content)
        
        return modifications
```

## Specific Issue Resolution Procedures

### Missing Object Error Resolution

#### Problem: `'NoneType' object has no attribute 'name'`

**Systematic Resolution Process:**

1. **Immediate Diagnosis**
```bash
# Check error logs for patterns
find . -name "*.sim_error.log" | xargs grep -l "NoneType.*attribute.*name"

# Identify affected files
grep -r "object\.type\.name" src/digitalmodel/modules/orcaflex/
```

2. **Model Validation**
```python
def validate_orcaflex_model(model_path: str) -> ModelValidationReport:
    """Validate OrcaFlex model integrity."""
    
    try:
        import OrcFxAPI
        model = OrcFxAPI.Model(model_path)
        
        validation_report = ModelValidationReport()
        
        # Check each object type
        object_types = ['Line', 'Vessel', 'Winch', 'Link', '6DBuoy']
        
        for obj_type in object_types:
            objects = model.objects(obj_type)
            
            for i, obj in enumerate(objects):
                try:
                    # Test critical properties
                    name = obj.name
                    type_name = obj.type.name if obj.type else None
                    
                    validation_report.valid_objects.append({
                        'type': obj_type,
                        'index': i,
                        'name': name,
                        'type_name': type_name
                    })
                    
                except Exception as e:
                    validation_report.invalid_objects.append({
                        'type': obj_type,
                        'index': i,
                        'error': str(e)
                    })
        
        return validation_report
        
    except Exception as e:
        return ModelValidationReport(
            model_load_error=str(e),
            valid_objects=[],
            invalid_objects=[]
        )
```

3. **Code Modification Implementation**
```python
def implement_safe_object_access():
    """Implement safe object access patterns."""
    
    # Template for safe object access
    safe_access_template = '''
    def safe_get_object_property(obj, property_path, default=None):
        """Safely access nested object properties."""
        try:
            current = obj
            for prop in property_path.split('.'):
                if current is None or not hasattr(current, prop):
                    return default
                current = getattr(current, prop)
                if current is None:
                    return default
            return current
        except Exception:
            return default
    
    # Usage examples:
    # object_type_name = safe_get_object_property(obj, 'type.name', 'Unknown')
    # object_name = safe_get_object_property(obj, 'name', f'Object_{index}')
    '''
    
    return safe_access_template
```

4. **Configuration Updates**
```yaml
# Updated configuration with error handling
orcaflex:
  postprocess:
    error_handling:
      missing_objects: 'warn_and_continue'  # 'fail', 'warn_and_continue', 'ignore'
      invalid_properties: 'use_defaults'
      object_validation: true
      
    summary:
      flag: true
      validation_required: true  # Validate objects before processing
      fallback_to_available_objects: true
      
  object_safety:
    enable_safe_access: true
    default_object_name: 'Unknown_Object'
    log_missing_objects: true
    continue_on_errors: true
```

### Configuration Error Resolution

#### Problem: `KeyError: 'summary_settings'`

**Resolution Steps:**

1. **Configuration Validation**
```python
def validate_orcaflex_configuration(config: Dict) -> ConfigValidationReport:
    """Validate OrcaFlex configuration completeness."""
    
    required_sections = {
        'meta': ['basename', 'library', 'label'],
        'default': ['log_level', 'config'],
        'parameters': ['VarNames'],
        'orcaflex': ['postprocess'],
        'file_management': ['flag', 'input_directory', 'output_directory']
    }
    
    conditional_sections = {
        'summary_settings': 'orcaflex.postprocess.summary.flag == true',
        'time_series_settings': 'orcaflex.postprocess.time_series.flag == true'
    }
    
    validation_report = ConfigValidationReport()
    
    # Check required sections
    for section, required_keys in required_sections.items():
        if section not in config:
            validation_report.missing_sections.append(section)
        else:
            for key in required_keys:
                if key not in config[section]:
                    validation_report.missing_keys.append(f"{section}.{key}")
    
    # Check conditional sections
    for section, condition in conditional_sections.items():
        if eval_condition(condition, config) and section not in config:
            validation_report.missing_conditional_sections.append({
                'section': section,
                'condition': condition
            })
    
    return validation_report
```

2. **Auto-Generation of Missing Sections**
```python
def generate_missing_configuration_sections(config: Dict) -> Dict:
    """Auto-generate missing configuration sections."""
    
    updated_config = config.copy()
    
    # Generate summary_settings if summary processing is enabled
    if (config.get('orcaflex', {}).get('postprocess', {}).get('summary', {}).get('flag') == True 
        and 'summary_settings' not in config):
        
        updated_config['summary_settings'] = {
            'groups': [{
                'Label': config.get('meta', {}).get('label', 'Analysis'),
                'Columns': [{
                    'ObjectName': 'Line',  # Generic - should be replaced with actual names
                    'VarName': ['End force', 'End moment', 'Effective tension']
                }]
            }]
        }
    
    # Generate time_series_settings if time series processing is enabled  
    if (config.get('orcaflex', {}).get('postprocess', {}).get('time_series', {}).get('flag') == True
        and 'time_series_settings' not in config):
        
        updated_config['time_series_settings'] = {
            'data': True,
            'sample_rate': 'auto',
            'duration': 'full'
        }
    
    return updated_config
```

## Prevention Strategies

### Proactive Model Validation

```python
class OrcaFlexModelValidator:
    """Proactive validation system for OrcaFlex models."""
    
    def __init__(self):
        self.validation_rules = [
            self._validate_object_integrity,
            self._validate_object_naming,
            self._validate_model_completeness,
            self._validate_configuration_compatibility
        ]
    
    def validate_before_processing(self, model_path: str, config: Dict) -> ValidationResult:
        """Comprehensive pre-processing validation."""
        
        validation_results = []
        
        for rule in self.validation_rules:
            result = rule(model_path, config)
            validation_results.append(result)
        
        overall_valid = all(result.passed for result in validation_results)
        
        return ValidationResult(
            overall_valid=overall_valid,
            rule_results=validation_results,
            recommendations=self._generate_validation_recommendations(validation_results)
        )
    
    def _validate_object_integrity(self, model_path: str, config: Dict) -> RuleResult:
        """Validate all objects have required properties."""
        
        try:
            import OrcFxAPI
            model = OrcFxAPI.Model(model_path)
            
            integrity_issues = []
            
            # Check all object types mentioned in configuration
            var_names = config.get('parameters', {}).get('VarNames', {})
            
            for object_type in var_names.keys():
                objects = model.objects(object_type)
                
                for i, obj in enumerate(objects):
                    if obj is None:
                        integrity_issues.append(f"{object_type}[{i}] is None")
                    elif not hasattr(obj, 'type') or obj.type is None:
                        integrity_issues.append(f"{object_type}[{i}] has invalid type")
                    elif not hasattr(obj.type, 'name') or obj.type.name is None:
                        integrity_issues.append(f"{object_type}[{i}] has invalid type.name")
            
            return RuleResult(
                rule_name="Object Integrity",
                passed=len(integrity_issues) == 0,
                issues=integrity_issues,
                recommendation="Fix object integrity issues before processing" if integrity_issues else "All objects have valid integrity"
            )
            
        except Exception as e:
            return RuleResult(
                rule_name="Object Integrity",
                passed=False,
                issues=[f"Failed to validate model: {str(e)}"],
                recommendation="Fix model loading issues before processing"
            )
```

### Configuration Templates and Standards

```yaml
# Standard OrcaFlex Configuration Template
orcaflex_standard_template:
  meta:
    basename: orcaflex_post_process
    library: digitalmodel
    label: '${ANALYSIS_LABEL}'
    description: '${ANALYSIS_DESCRIPTION}'
    
  validation:
    enable_model_validation: true
    enable_config_validation: true
    fail_on_validation_error: false
    log_validation_warnings: true
    
  error_handling:
    missing_objects: 'warn_and_continue'
    invalid_properties: 'use_defaults'  
    processing_errors: 'log_and_continue'
    max_errors_before_abort: 10
    
  default:
    log_level: INFO
    config:
      overwrite:
        output: true
        
  parameters:
    VarNames:
      Line: [End force, End moment, Effective tension, Wall tension, Bend moment, Curvature]
      Vessel: [Position, Velocity, Acceleration]
      # Add other object types as needed
      
  orcaflex:
    postprocess:
      validation_enabled: true
      safe_object_access: true
      
      visualization: 
        flag: false
        
      summary:
        flag: ${ENABLE_SUMMARY|true}
        validation_required: true
        statistics: 
          Minimum: true
          Maximum: true  
          Mean: true
          StdDev: true
        min: true
        max: true
        mean: true
        
      linked_statistics: 
        flag: ${ENABLE_LINKED_STATS|false}
        
      RangeGraph: 
        flag: ${ENABLE_RANGE_GRAPH|false}
        
      time_series: 
        flag: ${ENABLE_TIME_SERIES|false}
        
      cummulative_histograms: 
        flag: ${ENABLE_HISTOGRAMS|false}
        
  summary_settings:
    validation_mode: 'strict'  # 'strict', 'lenient', 'auto_fix'
    groups:
      - Label: '${ANALYSIS_LABEL}'
        Columns:
          - ObjectName: "${PRIMARY_OBJECT_NAME}"
            VarName: ${PRIMARY_VAR_NAMES}
            
  time_series_settings:
    data: ${ENABLE_TIME_SERIES|false}
    validation_required: true
    
  file_management:
    flag: true
    validation_enabled: true
    input_directory: '${INPUT_DIRECTORY}'
    output_directory: '${OUTPUT_DIRECTORY}'
    filename:
      extension: [sim]
      pattern: '${FILENAME_PATTERN}'
      filters:
        contains: ${FILENAME_CONTAINS|[]}
        not_contains: ${FILENAME_NOT_CONTAINS|[]}
        
  parallel_processing:
    enabled: false
    max_workers: 1
    timeout_per_file: 1800
    save_error_reports: true
    progress_reporting: true
    error_recovery: true
```

## Monitoring and Alerting

### Real-time Error Monitoring

```python
class OrcaFlexErrorMonitor:
    """Real-time monitoring system for OrcaFlex errors."""
    
    def __init__(self, alert_config: AlertConfig):
        self.alert_config = alert_config
        self.error_patterns = self._load_error_patterns()
        self.alert_manager = AlertManager(alert_config)
        
    def monitor_analysis_run(self, analysis_id: str) -> MonitoringSession:
        """Monitor analysis run for errors and issues."""
        
        session = MonitoringSession(analysis_id)
        
        # Set up real-time log monitoring
        log_monitor = LogMonitor(
            log_path=f"logs/{analysis_id}",
            patterns=self.error_patterns,
            callback=self._handle_error_detection
        )
        
        # Start monitoring
        log_monitor.start()
        session.add_monitor(log_monitor)
        
        return session
    
    def _handle_error_detection(self, error_event: ErrorEvent) -> None:
        """Handle detected error events."""
        
        # Classify error
        classification = self.classify_error(error_event.message)
        
        # Determine if immediate action is needed
        if classification.severity in ['critical', 'high']:
            # Try automated resolution
            resolution_result = self.attempt_automated_resolution(error_event)
            
            if not resolution_result.success:
                # Send alert
                self.alert_manager.send_alert(
                    severity=classification.severity,
                    message=f"OrcaFlex error requires attention: {error_event.message}",
                    context=error_event.context,
                    recommended_actions=classification.recommended_actions
                )
        
        # Log all errors for analysis
        self._log_error_for_analysis(error_event, classification)
```

## Testing and Validation

### Troubleshooting Test Suite

```python
class TroubleshootingTestSuite:
    """Comprehensive test suite for troubleshooting framework."""
    
    def __init__(self):
        self.test_models = self._load_test_models()
        self.test_configurations = self._load_test_configurations()
        
    def test_error_detection_and_resolution(self) -> TestResults:
        """Test complete error detection and resolution cycle."""
        
        test_results = {}
        
        # Test missing object error handling
        test_results['missing_objects'] = self._test_missing_object_handling()
        
        # Test configuration error handling
        test_results['configuration'] = self._test_configuration_error_handling()
        
        # Test API integration error handling
        test_results['api_integration'] = self._test_api_error_handling()
        
        # Test automated resolution
        test_results['automated_resolution'] = self._test_automated_resolution()
        
        # Test prevention strategies
        test_results['prevention'] = self._test_prevention_strategies()
        
        return TestResults(test_results)
    
    def _test_missing_object_handling(self) -> TestResult:
        """Test handling of missing object errors."""
        
        # Create model with missing objects
        test_model = self._create_model_with_missing_objects()
        
        # Process with troubleshooting framework
        processor = OrcaFlexProcessor(enable_troubleshooting=True)
        
        try:
            result = processor.process_model(test_model)
            
            return TestResult(
                success=result.success,
                warnings=result.warnings,
                errors_resolved=len(result.resolved_errors),
                description="Missing object handling test"
            )
            
        except Exception as e:
            return TestResult(
                success=False,
                error=str(e),
                description="Missing object handling test failed"
            )
```

---

*This comprehensive troubleshooting framework provides systematic approaches to diagnosing, resolving, and preventing OrcaFlex integration issues with professional-grade error handling and automated resolution capabilities.*