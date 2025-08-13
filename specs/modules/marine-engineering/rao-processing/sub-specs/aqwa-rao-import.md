# ANSYS AQWA RAO Data Import - Technical Specification

## Overview

This specification details the technical implementation for importing and processing Response Amplitude Operator (RAO) data from ANSYS AQWA hydrodynamic analysis results. AQWA is the industry standard for hydrodynamic analysis, making robust AQWA integration essential for marine engineering workflows.

## AQWA File Format Analysis

### .LIS File Structure

AQWA .lis files use FORTRAN fixed-width format with specific patterns:

```fortran
R.A.O.S-VARIATION WITH WAVE DIRECTION
    PERIOD = 15.56  FREQ. = 0.404  DIRECTION = 0. AND DATA FOR MODES-
      1    2    3    4    5    6  
      0.000E+00  0.000E+00  5.312E-02  0.000E+00  7.985E-01  0.000E+00
      0.00  0.00 115.67  0.00 -89.46  0.00
   DIRECTION = 30. AND DATA FOR MODES-
      1    2    3    4    5    6  
      0.000E+00  5.312E-02  7.985E-01  0.000E+00  6.781E-01  4.125E-02
      0.00 115.67 -89.46  0.00 -92.34  84.23
```

Key characteristics:
- **Header Pattern**: "R.A.O.S-VARIATION WITH WAVE DIRECTION"
- **Frequency Block**: Period, frequency, and directional data
- **Mode Ordering**: Modes 1-6 correspond to surge, sway, heave, roll, pitch, yaw
- **Data Format**: First line amplitude, second line phase (degrees)
- **Multiple Iterations**: Later iterations override earlier data

## Technical Implementation

### Core AQWA Parser

```python
class AQWARAOParser:
    """Enhanced ANSYS AQWA RAO data parser with comprehensive format support."""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        self.search_pattern = "R.A.O.S-VARIATION WITH WAVE DIRECTION"
        
    def parse_rao_file(self, file_path: str, encoding: str = 'utf-8') -> AQWARAOData:
        """Parse AQWA .lis file with comprehensive error handling."""
        
        try:
            with open(file_path, 'r', encoding=encoding, errors='replace') as f:
                content = f.read()
        except Exception as e:
            raise AQWAParsingError(f"Failed to read AQWA file {file_path}: {e}")
        
        # Find all RAO data blocks
        rao_blocks = self._find_rao_blocks(content)
        
        if not rao_blocks:
            raise AQWAParsingError("No RAO data blocks found in file")
        
        # Use the last iteration (most recent analysis results)
        final_rao_data = self._parse_rao_block(rao_blocks[-1])
        
        # Validate parsed data
        validation_report = self._validate_aqwa_data(final_rao_data)
        
        return AQWARAOData(
            raw_data=final_rao_data,
            validation_report=validation_report,
            source_file=file_path,
            aqwa_version=self._detect_aqwa_version(content),
            parsing_metadata=self._generate_parsing_metadata()
        )
    
    def _find_rao_blocks(self, content: str) -> List[str]:
        """Extract all RAO data blocks from AQWA file."""
        
        blocks = []
        current_pos = 0
        
        while True:
            # Find next RAO block start
            start_pos = content.find(self.search_pattern, current_pos)
            if start_pos == -1:
                break
            
            # Find block end (next RAO block or end of file)
            next_start = content.find(self.search_pattern, start_pos + 1)
            end_pos = next_start if next_start != -1 else len(content)
            
            # Extract block
            block_content = content[start_pos:end_pos]
            blocks.append(block_content)
            
            current_pos = start_pos + 1
        
        self.logger.info(f"Found {len(blocks)} RAO data blocks")
        return blocks
    
    def _parse_rao_block(self, block_content: str) -> Dict:
        """Parse individual RAO data block."""
        
        lines = block_content.split('\n')
        rao_data = {
            'frequencies': [],
            'periods': [],
            'headings': [],
            'amplitudes': {dof: [] for dof in self.dof_names},
            'phases': {dof: [] for dof in self.dof_names}
        }
        
        i = 1  # Skip header line
        while i < len(lines):
            line = lines[i].strip()
            
            # Check for frequency block header
            if 'PERIOD =' in line and 'FREQ. =' in line and 'DIRECTION =' in line:
                freq_data = self._parse_frequency_header(line)
                rao_data['periods'].append(freq_data['period'])
                rao_data['frequencies'].append(freq_data['frequency'])
                
                # Parse directional data for this frequency
                i, freq_headings, freq_amplitudes, freq_phases = self._parse_frequency_block(
                    lines, i + 1
                )
                
                # Store data (first frequency defines heading structure)
                if not rao_data['headings']:
                    rao_data['headings'] = freq_headings
                
                # Store amplitude and phase data
                for j, heading in enumerate(freq_headings):
                    for k, dof in enumerate(self.dof_names):
                        if j == 0:  # Initialize lists for first heading
                            rao_data['amplitudes'][dof].append([])
                            rao_data['phases'][dof].append([])
                        
                        rao_data['amplitudes'][dof][-1].append(freq_amplitudes[j][k])
                        rao_data['phases'][dof][-1].append(freq_phases[j][k])
                
            i += 1
        
        # Convert to numpy arrays for efficient processing
        rao_data = self._convert_to_numpy_arrays(rao_data)
        
        return rao_data
    
    def _parse_frequency_header(self, line: str) -> Dict:
        """Parse frequency block header line."""
        
        import re
        
        # Extract period, frequency, and initial direction
        period_match = re.search(r'PERIOD\s*=\s*([\d.E+-]+)', line)
        freq_match = re.search(r'FREQ\.\s*=\s*([\d.E+-]+)', line)
        direction_match = re.search(r'DIRECTION\s*=\s*([\d.E+-]+)', line)
        
        if not all([period_match, freq_match, direction_match]):
            raise AQWAParsingError(f"Invalid frequency header: {line}")
        
        return {
            'period': float(period_match.group(1)),
            'frequency': float(freq_match.group(1)),
            'initial_direction': float(direction_match.group(1))
        }
    
    def _parse_frequency_block(self, lines: List[str], start_idx: int) -> Tuple[int, List, List, List]:
        """Parse complete frequency block with all directions."""
        
        headings = []
        amplitudes = []
        phases = []
        
        i = start_idx
        current_heading = None
        
        while i < len(lines):
            line = lines[i].strip()
            
            # Check for end of block
            if not line or line == '1' or 'PERIOD =' in line:
                break
            
            # Check for direction line
            if 'DIRECTION =' in line:
                direction_match = re.search(r'DIRECTION\s*=\s*([\d.E+-]+)', line)
                if direction_match:
                    current_heading = float(direction_match.group(1))
                    headings.append(current_heading)
                i += 1
                continue
            
            # Check for mode header (skip)
            if all(c in '123456 ' for c in line) and len(line.split()) == 6:
                i += 1
                continue
            
            # Parse amplitude or phase data
            if self._is_data_line(line):
                values = self._parse_data_line(line)
                
                # Determine if amplitude or phase based on magnitude
                if any(abs(v) > 100 for v in values if v != 0):
                    # Likely phase data (degrees can be >100)
                    phases.append(values)
                else:
                    # Likely amplitude data
                    amplitudes.append(values)
            
            i += 1
        
        return i, headings, amplitudes, phases
    
    def _is_data_line(self, line: str) -> bool:
        """Check if line contains RAO data values."""
        
        try:
            values = line.split()
            if len(values) != 6:
                return False
            
            # Try to convert all values to float
            [float(v) for v in values]
            return True
            
        except (ValueError, IndexError):
            return False
    
    def _parse_data_line(self, line: str) -> List[float]:
        """Parse line of RAO data values."""
        
        values = line.split()
        if len(values) != 6:
            raise AQWAParsingError(f"Expected 6 values in data line, got {len(values)}: {line}")
        
        try:
            return [float(v) for v in values]
        except ValueError as e:
            raise AQWAParsingError(f"Failed to parse data values: {line}. Error: {e}")
```

### Enhanced Data Processing

```python
class AQWADataProcessor:
    """Process and enhance AQWA RAO data quality."""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        
    def process_aqwa_data(self, aqwa_data: AQWARAOData) -> ProcessedRAOData:
        """Process AQWA data with quality enhancements."""
        
        # Phase 1: Data validation and cleaning
        cleaned_data = self._clean_aqwa_data(aqwa_data.raw_data)
        
        # Phase 2: Handle AQWA-specific conventions
        standardized_data = self._standardize_aqwa_conventions(cleaned_data)
        
        # Phase 3: Quality enhancement
        enhanced_data = self._enhance_data_quality(standardized_data)
        
        # Phase 4: Generate output structure
        processed_data = self._generate_processed_structure(enhanced_data)
        
        return ProcessedRAOData(
            source_format='aqwa',
            processed_data=processed_data,
            quality_metrics=self._compute_quality_metrics(processed_data),
            processing_notes=self._generate_processing_notes()
        )
    
    def _clean_aqwa_data(self, raw_data: Dict) -> Dict:
        """Clean and validate AQWA raw data."""
        
        cleaned_data = raw_data.copy()
        
        # Remove invalid data points
        cleaned_data = self._remove_invalid_values(cleaned_data)
        
        # Fix common AQWA parsing issues
        cleaned_data = self._fix_aqwa_parsing_issues(cleaned_data)
        
        # Validate data consistency
        self._validate_data_consistency(cleaned_data)
        
        return cleaned_data
    
    def _standardize_aqwa_conventions(self, data: Dict) -> Dict:
        """Convert AQWA conventions to standard format."""
        
        standardized = data.copy()
        
        # AQWA uses degrees for phases - ensure this is preserved
        # AQWA uses specific coordinate system - document and preserve
        
        # Convert to standard DOF ordering if needed
        # AQWA: surge(1), sway(2), heave(3), roll(4), pitch(5), yaw(6)
        # Standard: surge, sway, heave, roll, pitch, yaw (same)
        
        # Handle AQWA unit conventions
        standardized = self._handle_aqwa_units(standardized)
        
        return standardized
    
    def _enhance_data_quality(self, data: Dict) -> Dict:
        """Apply quality enhancement algorithms."""
        
        enhanced = data.copy()
        
        # Symmetry enforcement for appropriate DOFs
        enhanced = self._enforce_symmetry(enhanced)
        
        # Outlier detection and correction
        enhanced = self._detect_and_correct_outliers(enhanced)
        
        # Phase continuity correction
        enhanced = self._correct_phase_discontinuities(enhanced)
        
        # Physical limit validation
        enhanced = self._validate_physical_limits(enhanced)
        
        return enhanced
    
    def _enforce_symmetry(self, data: Dict) -> Dict:
        """Enforce symmetry for appropriate DOFs."""
        
        # Surge, heave, pitch should be symmetric about 0°/180°
        # Sway, roll, yaw should be anti-symmetric
        
        symmetric_dofs = ['surge', 'heave', 'pitch']
        antisymmetric_dofs = ['sway', 'roll', 'yaw']
        
        enhanced_data = data.copy()
        headings = np.array(data['headings'])
        
        for dof in symmetric_dofs:
            enhanced_data['amplitudes'][dof] = self._apply_symmetric_constraint(
                data['amplitudes'][dof], headings
            )
            enhanced_data['phases'][dof] = self._apply_symmetric_phase_constraint(
                data['phases'][dof], headings
            )
        
        for dof in antisymmetric_dofs:
            enhanced_data['amplitudes'][dof] = self._apply_antisymmetric_constraint(
                data['amplitudes'][dof], headings
            )
            enhanced_data['phases'][dof] = self._apply_antisymmetric_phase_constraint(
                data['phases'][dof], headings
            )
        
        return enhanced_data
```

### Validation Framework

```python
class AQWAValidationFramework:
    """Comprehensive validation for AQWA RAO data."""
    
    def __init__(self):
        self.validation_criteria = self._load_aqwa_validation_criteria()
        
    def validate_aqwa_data(self, aqwa_data: AQWARAOData) -> AQWAValidationReport:
        """Perform comprehensive AQWA data validation."""
        
        validation_results = {}
        
        # Format-specific validation
        validation_results['format'] = self._validate_aqwa_format(aqwa_data)
        
        # Physical validation
        validation_results['physical'] = self._validate_physical_reasonableness(aqwa_data)
        
        # Numerical validation
        validation_results['numerical'] = self._validate_numerical_quality(aqwa_data)
        
        # AQWA-specific validation
        validation_results['aqwa_specific'] = self._validate_aqwa_specifics(aqwa_data)
        
        # Overall assessment
        overall_score = self._compute_overall_validation_score(validation_results)
        
        return AQWAValidationReport(
            validation_results=validation_results,
            overall_score=overall_score,
            recommendations=self._generate_aqwa_recommendations(validation_results),
            aqwa_version_compatibility=self._assess_version_compatibility(aqwa_data)
        )
    
    def _validate_aqwa_format(self, aqwa_data: AQWARAOData) -> FormatValidationResult:
        """Validate AQWA-specific format requirements."""
        
        checks = {}
        
        # Check for required AQWA patterns
        checks['rao_pattern_found'] = bool(aqwa_data.raw_data)
        
        # Validate frequency range (typical AQWA range)
        freq_range = (min(aqwa_data.raw_data['frequencies']), 
                     max(aqwa_data.raw_data['frequencies']))
        checks['frequency_range_valid'] = (
            freq_range[0] >= 0.05 and freq_range[1] <= 5.0
        )
        
        # Check heading coverage (AQWA typically uses 0-360° or 0-180°)
        headings = aqwa_data.raw_data['headings']
        checks['heading_coverage'] = len(headings) >= 7  # Minimum reasonable coverage
        
        # Validate data completeness
        checks['data_completeness'] = self._check_aqwa_data_completeness(aqwa_data)
        
        return FormatValidationResult(
            checks=checks,
            overall_valid=all(checks.values()),
            format_specific_notes=self._generate_aqwa_format_notes(checks)
        )
    
    def _validate_aqwa_specifics(self, aqwa_data: AQWARAOData) -> AQWASpecificValidation:
        """Validate AQWA-specific characteristics."""
        
        specific_checks = {}
        
        # Check AQWA coordinate system consistency
        specific_checks['coordinate_system'] = self._validate_aqwa_coordinates(aqwa_data)
        
        # Validate AQWA phase conventions (degrees)
        specific_checks['phase_convention'] = self._validate_aqwa_phase_convention(aqwa_data)
        
        # Check for AQWA version compatibility issues
        specific_checks['version_compatibility'] = self._check_version_compatibility(aqwa_data)
        
        # Validate AQWA unit system
        specific_checks['unit_system'] = self._validate_aqwa_units(aqwa_data)
        
        return AQWASpecificValidation(
            checks=specific_checks,
            aqwa_version=aqwa_data.aqwa_version,
            compatibility_notes=self._generate_compatibility_notes(specific_checks)
        )
```

## Integration Specifications

### Ship Dynamics Module Integration

```python
class AQWARAOIntegration:
    """Integration interface for AQWA RAO data with ship dynamics analysis."""
    
    def __init__(self):
        self.parser = AQWARAOParser()
        self.processor = AQWADataProcessor()
        self.validator = AQWAValidationFramework()
        
    def prepare_for_motion_analysis(self, aqwa_file: str) -> MotionAnalysisRAOData:
        """Prepare AQWA RAO data for 6-DOF motion analysis."""
        
        # Parse AQWA file
        aqwa_data = self.parser.parse_rao_file(aqwa_file)
        
        # Validate data quality
        validation_report = self.validator.validate_aqwa_data(aqwa_data)
        
        if validation_report.overall_score < 0.8:
            warnings.warn(f"AQWA data quality score: {validation_report.overall_score}")
        
        # Process for motion analysis
        processed_data = self.processor.process_aqwa_data(aqwa_data)
        
        # Convert to motion analysis format
        motion_rao_data = self._convert_to_motion_analysis_format(processed_data)
        
        return motion_rao_data
    
    def _convert_to_motion_analysis_format(self, processed_data: ProcessedRAOData) -> MotionAnalysisRAOData:
        """Convert processed AQWA data to motion analysis format."""
        
        # Create standardized RAO data structure
        frequencies = processed_data.processed_data['frequencies']
        headings = processed_data.processed_data['headings']
        
        # Build RAO matrices for each DOF
        rao_matrices = {}
        for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
            amplitude_matrix = np.array(processed_data.processed_data['amplitudes'][dof])
            phase_matrix = np.array(processed_data.processed_data['phases'][dof])
            
            rao_matrices[dof] = RAOMatrix(
                frequencies=frequencies,
                headings=headings,
                amplitudes=amplitude_matrix,
                phases=phase_matrix,
                units=self._get_dof_units(dof)
            )
        
        return MotionAnalysisRAOData(
            rao_matrices=rao_matrices,
            source_format='aqwa',
            quality_score=processed_data.quality_metrics['overall_score'],
            metadata=self._generate_motion_analysis_metadata(processed_data)
        )
```

## Performance Optimization

### Large Dataset Handling

```python
class AQWAPerformanceOptimizer:
    """Performance optimization for large AQWA datasets."""
    
    def __init__(self):
        self.memory_threshold = 1e6  # 1M data points
        self.chunk_size = 10000
        
    def optimize_large_dataset_parsing(self, file_path: str) -> OptimizedAQWAData:
        """Optimized parsing for large AQWA files."""
        
        file_size = os.path.getsize(file_path)
        
        if file_size > 100e6:  # 100MB
            return self._chunk_based_parsing(file_path)
        else:
            return self._standard_parsing(file_path)
    
    def _chunk_based_parsing(self, file_path: str) -> OptimizedAQWAData:
        """Parse large files in chunks to manage memory usage."""
        
        # Implementation for chunk-based parsing
        # - Read file in chunks
        # - Parse each chunk independently
        # - Merge results efficiently
        
        pass
    
    def optimize_memory_usage(self, aqwa_data: AQWARAOData) -> MemoryOptimizedRAOData:
        """Optimize memory usage for large RAO datasets."""
        
        # Use sparse matrices for data with many zeros
        # Compress data using appropriate data types
        # Implement lazy loading for large datasets
        
        pass
```

## Error Handling and Diagnostics

```python
class AQWADiagnostics:
    """Comprehensive error handling and diagnostics for AQWA parsing."""
    
    def __init__(self):
        self.error_patterns = self._load_common_aqwa_errors()
        
    def diagnose_parsing_error(self, error: Exception, file_path: str) -> DiagnosticReport:
        """Diagnose and provide solutions for AQWA parsing errors."""
        
        error_type = type(error).__name__
        error_message = str(error)
        
        # Check against known error patterns
        diagnostic = self._match_error_pattern(error_type, error_message)
        
        # Analyze file for common issues
        file_issues = self._analyze_file_issues(file_path)
        
        # Generate recommendations
        recommendations = self._generate_error_recommendations(diagnostic, file_issues)
        
        return DiagnosticReport(
            error_type=error_type,
            error_message=error_message,
            diagnostic=diagnostic,
            file_issues=file_issues,
            recommendations=recommendations,
            suggested_fixes=self._suggest_fixes(diagnostic)
        )
    
    def _load_common_aqwa_errors(self) -> Dict:
        """Load database of common AQWA parsing errors and solutions."""
        
        return {
            'encoding_error': {
                'pattern': 'UnicodeDecodeError',
                'cause': 'File encoding not UTF-8',
                'solution': 'Try different encoding (latin-1, cp1252)'
            },
            'format_error': {
                'pattern': 'No RAO data blocks found',
                'cause': 'File does not contain RAO data or uses different format',
                'solution': 'Verify file is AQWA .lis output with RAO analysis'
            },
            'data_error': {
                'pattern': 'Expected 6 values in data line',
                'cause': 'FORTRAN formatting issue or corrupted data',
                'solution': 'Check for missing or extra spaces in data lines'
            }
        }
```

## Testing and Validation

### Test Suite Implementation

```python
class AQWATestSuite:
    """Comprehensive test suite for AQWA RAO import functionality."""
    
    def __init__(self):
        self.test_data_path = "tests/data/aqwa/"
        self.benchmark_data = self._load_benchmark_data()
        
    def run_comprehensive_tests(self) -> TestResults:
        """Run all AQWA-related tests."""
        
        test_results = {}
        
        # Format parsing tests
        test_results['parsing'] = self.test_aqwa_parsing()
        
        # Data validation tests
        test_results['validation'] = self.test_aqwa_validation()
        
        # Integration tests
        test_results['integration'] = self.test_aqwa_integration()
        
        # Performance tests
        test_results['performance'] = self.test_aqwa_performance()
        
        return TestResults(test_results)
    
    def test_aqwa_parsing(self) -> ParsingTestResults:
        """Test AQWA file parsing accuracy."""
        
        test_files = [
            'fpso_standard.lis',
            'semi_submersible.lis',
            'ship_container.lis',
            'multiple_iterations.lis',
            'abbreviated_format.lis'
        ]
        
        parsing_results = {}
        
        for test_file in test_files:
            file_path = os.path.join(self.test_data_path, test_file)
            
            try:
                parser = AQWARAOParser()
                result = parser.parse_rao_file(file_path)
                
                # Validate against expected results
                validation_result = self._validate_against_expected(
                    result, test_file
                )
                
                parsing_results[test_file] = ParsingTestResult(
                    success=True,
                    result=result,
                    validation=validation_result
                )
                
            except Exception as e:
                parsing_results[test_file] = ParsingTestResult(
                    success=False,
                    error=e,
                    diagnostic=AQWADiagnostics().diagnose_parsing_error(e, file_path)
                )
        
        return ParsingTestResults(parsing_results)
```

---

*This technical specification provides comprehensive guidance for robust ANSYS AQWA RAO data import with industry-standard accuracy and professional error handling capabilities.*