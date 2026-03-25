#!/bin/bash
# Verification Hooks for All Repository Operations
# MANDATORY: Use these hooks for ALL commands, tasks, and operations

set -euo pipefail  # Exit on error, undefined variables, and pipe failures

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ============================================================================
# CORE VERIFICATION FUNCTIONS
# ============================================================================

# Check command return code
verify_return_code() {
    local cmd="$1"
    local description="${2:-Command execution}"
    
    echo -e "${BLUE}[RUNNING]${NC} $description"
    echo -e "${BLUE}[COMMAND]${NC} $cmd"
    
    if eval "$cmd"; then
        local ret=$?
        echo -e "${GREEN}[✓ PASS]${NC} $description (Return code: $ret)"
        return 0
    else
        local ret=$?
        echo -e "${RED}[✗ FAIL]${NC} $description (Return code: $ret)"
        return $ret
    fi
}

# Verify file exists
verify_file_exists() {
    local file="$1"
    local description="${2:-File verification}"
    
    echo -e "${BLUE}[CHECK]${NC} $description: $file"
    if [[ -f "$file" ]]; then
        local size=$(ls -lh "$file" | awk '{print $5}')
        echo -e "${GREEN}[✓ EXISTS]${NC} $file (Size: $size)"
        return 0
    else
        echo -e "${RED}[✗ MISSING]${NC} $file"
        return 1
    fi
}

# Verify directory exists
verify_dir_exists() {
    local dir="$1"
    local description="${2:-Directory verification}"
    
    echo -e "${BLUE}[CHECK]${NC} $description: $dir"
    if [[ -d "$dir" ]]; then
        local count=$(ls -1 "$dir" 2>/dev/null | wc -l)
        echo -e "${GREEN}[✓ EXISTS]${NC} $dir (Files: $count)"
        return 0
    else
        echo -e "${RED}[✗ MISSING]${NC} $dir"
        return 1
    fi
}

# Verify file was created/modified
verify_file_created() {
    local file="$1"
    local reference_time="${2:-5}"  # Check if modified in last N minutes
    local description="${3:-File creation verification}"
    
    echo -e "${BLUE}[CHECK]${NC} $description: $file"
    if [[ -f "$file" ]]; then
        if [[ $(find "$file" -mmin -"$reference_time" 2>/dev/null) ]]; then
            echo -e "${GREEN}[✓ CREATED]${NC} $file (Modified within $reference_time minutes)"
            return 0
        else
            echo -e "${YELLOW}[⚠ OLD]${NC} $file (Not recently modified)"
            return 1
        fi
    else
        echo -e "${RED}[✗ NOT CREATED]${NC} $file"
        return 1
    fi
}

# Verify process output contains expected text
verify_output_contains() {
    local cmd="$1"
    local expected="$2"
    local description="${3:-Output verification}"
    
    echo -e "${BLUE}[CHECK]${NC} $description"
    echo -e "${BLUE}[COMMAND]${NC} $cmd"
    echo -e "${BLUE}[EXPECT]${NC} '$expected'"
    
    local output=$(eval "$cmd" 2>&1)
    if echo "$output" | grep -q "$expected"; then
        echo -e "${GREEN}[✓ FOUND]${NC} Expected text in output"
        return 0
    else
        echo -e "${RED}[✗ NOT FOUND]${NC} Expected text not in output"
        echo -e "${YELLOW}[OUTPUT]${NC} ${output:0:200}..."
        return 1
    fi
}

# Verify Python module import
verify_python_module() {
    local module="$1"
    local venv_path="${2:-.venv}"
    
    echo -e "${BLUE}[CHECK]${NC} Python module: $module"
    if source "$venv_path/Scripts/activate" 2>/dev/null && python -c "import $module" 2>/dev/null; then
        echo -e "${GREEN}[✓ AVAILABLE]${NC} $module"
        return 0
    else
        echo -e "${RED}[✗ MISSING]${NC} $module"
        return 1
    fi
}

# Verify CSV file structure
verify_csv_structure() {
    local file="$1"
    local expected_columns="$2"
    local min_rows="${3:-1}"
    
    echo -e "${BLUE}[CHECK]${NC} CSV structure: $file"
    
    if [[ ! -f "$file" ]]; then
        echo -e "${RED}[✗ MISSING]${NC} CSV file not found"
        return 1
    fi
    
    local header=$(head -1 "$file")
    local row_count=$(wc -l < "$file")
    
    echo -e "${BLUE}[HEADER]${NC} $header"
    echo -e "${BLUE}[ROWS]${NC} $row_count"
    
    if [[ "$header" == *"$expected_columns"* ]]; then
        echo -e "${GREEN}[✓ COLUMNS]${NC} Expected columns found"
    else
        echo -e "${RED}[✗ COLUMNS]${NC} Missing expected columns: $expected_columns"
        return 1
    fi
    
    if [[ $row_count -ge $min_rows ]]; then
        echo -e "${GREEN}[✓ ROWS]${NC} Has $row_count rows (minimum: $min_rows)"
    else
        echo -e "${RED}[✗ ROWS]${NC} Only $row_count rows (minimum: $min_rows)"
        return 1
    fi
    
    return 0
}

# ============================================================================
# EXECUTION VERIFICATION HOOKS
# ============================================================================

# Pre-execution verification
pre_execution_check() {
    local working_dir="$1"
    local required_files=("${@:2}")
    
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    echo -e "${YELLOW}PRE-EXECUTION VERIFICATION${NC}"
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    
    # Check working directory
    verify_dir_exists "$working_dir" "Working directory"
    
    # Check required files
    for file in "${required_files[@]}"; do
        verify_file_exists "$working_dir/$file" "Required file"
    done
    
    # Check Python environment
    verify_file_exists "$working_dir/../../../../.venv/Scripts/activate" "Python venv"
    
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
}

# Post-execution verification
post_execution_check() {
    local return_code="$1"
    local output_files=("${@:2}")
    
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    echo -e "${YELLOW}POST-EXECUTION VERIFICATION${NC}"
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    
    # Check return code
    if [[ $return_code -eq 0 ]]; then
        echo -e "${GREEN}[✓ SUCCESS]${NC} Command completed successfully (Return code: $return_code)"
    else
        echo -e "${RED}[✗ ERROR]${NC} Command failed (Return code: $return_code)"
    fi
    
    # Check output files
    for file in "${output_files[@]}"; do
        verify_file_created "$file" 10 "Output file"
    done
    
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    
    return $return_code
}

# ============================================================================
# ORCAFLEX SPECIFIC HOOKS
# ============================================================================

# Verify OrcaFlex license
verify_orcaflex_license() {
    echo -e "${BLUE}[CHECK]${NC} OrcaFlex license availability"
    
    local check_cmd="source .venv/Scripts/activate && python -c 'from digitalmodel.orcaflex.orcaflex_utilities import is_orcaflex_available; print(is_orcaflex_available())'"
    
    if eval "$check_cmd" 2>/dev/null | grep -q "True"; then
        echo -e "${GREEN}[✓ AVAILABLE]${NC} OrcaFlex license"
        return 0
    else
        echo -e "${YELLOW}[⚠ UNAVAILABLE]${NC} OrcaFlex license (will use mock mode)"
        return 1
    fi
}

# Verify OrcaFlex simulation output
verify_orcaflex_output() {
    local output_dir="$1"
    local pattern="${2:-*.sim}"
    
    echo -e "${BLUE}[CHECK]${NC} OrcaFlex simulation outputs"
    
    local sim_files=$(find "$output_dir" -name "$pattern" -mmin -60 2>/dev/null | wc -l)
    
    if [[ $sim_files -gt 0 ]]; then
        echo -e "${GREEN}[✓ FOUND]${NC} $sim_files simulation files created"
        find "$output_dir" -name "$pattern" -mmin -60 -exec ls -lh {} \;
        return 0
    else
        echo -e "${RED}[✗ MISSING]${NC} No simulation files found"
        return 1
    fi
}

# ============================================================================
# BATCH OPERATION HOOKS
# ============================================================================

# Verify batch operation
verify_batch_operation() {
    local total_expected="$1"
    local successful="$2"
    local failed="$3"
    local description="${4:-Batch operation}"
    
    echo -e "${BLUE}[BATCH]${NC} $description"
    echo -e "${BLUE}[STATS]${NC} Total: $total_expected, Success: $successful, Failed: $failed"
    
    local total_processed=$((successful + failed))
    
    if [[ $total_processed -eq $total_expected ]]; then
        echo -e "${GREEN}[✓ COMPLETE]${NC} All items processed"
    else
        echo -e "${YELLOW}[⚠ INCOMPLETE]${NC} Only $total_processed of $total_expected processed"
    fi
    
    if [[ $failed -eq 0 ]]; then
        echo -e "${GREEN}[✓ SUCCESS]${NC} All operations successful"
        return 0
    else
        echo -e "${RED}[✗ FAILURES]${NC} $failed operations failed"
        return 1
    fi
}

# ============================================================================
# LOG VERIFICATION HOOKS
# ============================================================================

# Verify log contains no errors
verify_log_no_errors() {
    local log_file="$1"
    local ignore_patterns="${2:-}"
    
    echo -e "${BLUE}[CHECK]${NC} Log file for errors: $log_file"
    
    if [[ ! -f "$log_file" ]]; then
        echo -e "${YELLOW}[⚠ NO LOG]${NC} Log file not found"
        return 1
    fi
    
    local error_count=$(grep -iE "error|exception|failed|fatal" "$log_file" | grep -vE "$ignore_patterns" | wc -l)
    
    if [[ $error_count -eq 0 ]]; then
        echo -e "${GREEN}[✓ CLEAN]${NC} No errors found in log"
        return 0
    else
        echo -e "${RED}[✗ ERRORS]${NC} Found $error_count error messages"
        grep -iE "error|exception|failed|fatal" "$log_file" | head -5
        return 1
    fi
}

# ============================================================================
# SUMMARY REPORT
# ============================================================================

# Generate verification summary
verification_summary() {
    local checks_passed="$1"
    local checks_failed="$2"
    local start_time="$3"
    local end_time="$4"
    
    local duration=$((end_time - start_time))
    
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    echo -e "${YELLOW}VERIFICATION SUMMARY${NC}"
    echo -e "${YELLOW}═══════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}Duration:${NC} ${duration}s"
    echo -e "${GREEN}Passed:${NC} $checks_passed"
    echo -e "${RED}Failed:${NC} $checks_failed"
    
    if [[ $checks_failed -eq 0 ]]; then
        echo -e "${GREEN}╔═══════════════════════════════════╗${NC}"
        echo -e "${GREEN}║     ALL CHECKS PASSED ✓           ║${NC}"
        echo -e "${GREEN}╚═══════════════════════════════════╝${NC}"
        return 0
    else
        echo -e "${RED}╔═══════════════════════════════════╗${NC}"
        echo -e "${RED}║     VERIFICATION FAILED ✗         ║${NC}"
        echo -e "${RED}╚═══════════════════════════════════╝${NC}"
        return 1
    fi
}

# Export all functions
export -f verify_return_code
export -f verify_file_exists
export -f verify_dir_exists
export -f verify_file_created
export -f verify_output_contains
export -f verify_python_module
export -f verify_csv_structure
export -f pre_execution_check
export -f post_execution_check
export -f verify_orcaflex_license
export -f verify_orcaflex_output
export -f verify_batch_operation
export -f verify_log_no_errors
export -f verification_summary

echo -e "${GREEN}Verification hooks loaded successfully${NC}"