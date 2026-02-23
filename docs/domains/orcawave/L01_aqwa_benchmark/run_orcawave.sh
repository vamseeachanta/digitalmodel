#!/usr/bin/env bash
################################################################################
# OrcaWave Shell Execution Script
#
# Usage:
#   bash run_orcawave.sh <config_file.yml>
#   ./run_orcawave.sh orcawave_001_ship_raos_rev2.yml
#
# Features:
#   - Cross-platform (Git Bash, Linux, macOS)
#   - Auto-detects OrcaWave executable
#   - Validates configuration
#   - Captures logs
#   - Progress monitoring
################################################################################

set -euo pipefail  # Exit on error, undefined vars, pipe failures

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

print_header() {
    echo ""
    echo "============================================================"
    echo "$1"
    echo "============================================================"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_info() {
    echo -e "${BLUE}ℹ${NC} $1"
}

# ============================================================================
# ORCAWAVE DETECTION
# ============================================================================

find_orcawave() {
    local orcawave_exe=""

    # Detect platform
    case "$(uname -s)" in
        MINGW*|MSYS*|CYGWIN*)
            # Windows (Git Bash, MSYS2, Cygwin)
            print_info "Platform: Windows (Git Bash)"

            # Common Windows paths
            local win_paths=(
                "/c/Program Files (x86)/Orcina/OrcaFlex/11.6/OrcaWave64.exe"
                "/c/Program Files (x86)/Orcina/OrcaFlex/11.6/OrcaWave.exe"
                "/c/Program Files/Orcina/OrcaFlex/11.6/OrcaWave64.exe"
                "/c/Program Files/Orcina/OrcaFlex/11.6/OrcaWave.exe"
                "/c/Program Files/Orcina/OrcaWave/OrcaWave.exe"
                "/c/Program Files (x86)/Orcina/OrcaWave/OrcaWave.exe"
                "/c/Program Files/Orcina/OrcaFlex/OrcaWave.exe"
                "/d/OrcaWave/OrcaWave.exe"
            )

            for path in "${win_paths[@]}"; do
                if [ -f "$path" ]; then
                    orcawave_exe="$path"
                    break
                fi
            done
            ;;

        Linux)
            print_info "Platform: Linux"

            # Common Linux paths
            local linux_paths=(
                "/usr/local/bin/orcawave"
                "/opt/orcina/orcawave/bin/orcawave"
                "$HOME/orcawave/bin/orcawave"
            )

            for path in "${linux_paths[@]}"; do
                if [ -f "$path" ]; then
                    orcawave_exe="$path"
                    break
                fi
            done

            # Try which
            if [ -z "$orcawave_exe" ]; then
                if command -v orcawave &> /dev/null; then
                    orcawave_exe="$(which orcawave)"
                fi
            fi
            ;;

        Darwin)
            print_info "Platform: macOS"

            # Try which
            if command -v orcawave &> /dev/null; then
                orcawave_exe="$(which orcawave)"
            fi
            ;;

        *)
            print_warning "Unknown platform: $(uname -s)"
            ;;
    esac

    if [ -n "$orcawave_exe" ] && [ -f "$orcawave_exe" ]; then
        print_success "Found OrcaWave: $orcawave_exe"
        echo "$orcawave_exe"
        return 0
    else
        print_error "OrcaWave executable not found!"
        return 1
    fi
}

# ============================================================================
# CONFIGURATION VALIDATION
# ============================================================================

validate_config() {
    local config_file="$1"

    print_header "CONFIGURATION VALIDATION"

    # Check file exists
    if [ ! -f "$config_file" ]; then
        print_error "Config file not found: $config_file"
        return 1
    fi

    print_success "Config file: $config_file"

    # Basic YAML validation (check for syntax errors)
    if command -v python3 &> /dev/null; then
        if ! python3 -c "import yaml; yaml.safe_load(open('$config_file'))" 2>/dev/null; then
            print_error "YAML syntax error in config file"
            return 1
        fi
        print_success "YAML syntax valid"
    fi

    # Check for required fields (basic grep check)
    local required_fields=("UnitsSystem" "WaterDepth" "Bodies")
    local missing=0

    for field in "${required_fields[@]}"; do
        if ! grep -q "$field" "$config_file"; then
            print_warning "Missing field: $field"
            missing=$((missing + 1))
        fi
    done

    if [ $missing -gt 0 ]; then
        print_error "$missing required fields missing"
        return 1
    fi

    print_success "All required fields present"

    # Extract analysis parameters
    if command -v python3 &> /dev/null; then
        python3 <<EOF
import yaml
with open('$config_file', 'r') as f:
    cfg = yaml.safe_load(f)
    print(f"  - Water Depth: {cfg.get('WaterDepth')} m")
    print(f"  - Periods: {len(cfg.get('PeriodOrFrequency', []))} values")
    print(f"  - Headings: {len(cfg.get('WaveHeading', []))} values")
    print(f"  - Bodies: {len(cfg.get('Bodies', []))}")
EOF
    fi

    echo ""
    return 0
}

# ============================================================================
# ORCAWAVE EXECUTION
# ============================================================================

execute_orcawave() {
    local config_file="$1"
    local orcawave_exe="$2"
    local timeout="${3:-3600}"  # Default 1 hour

    print_header "ORCAWAVE EXECUTION"

    # Setup directories
    local work_dir="$(dirname "$config_file")"
    local log_dir="$work_dir/logs"
    local output_dir="$work_dir/orcawave_output"

    mkdir -p "$log_dir"
    mkdir -p "$output_dir"

    local log_file="$log_dir/orcawave_${TIMESTAMP}.log"

    print_info "Working Directory: $work_dir"
    print_info "Log File: $log_file"
    print_info "Timeout: ${timeout}s"

    # Prepare command
    local cmd="\"$orcawave_exe\" \"$config_file\""

    print_info "Command: $cmd"
    echo ""

    # Execute with timeout
    local start_time=$(date +%s)
    local return_code=0

    print_info "Starting OrcaWave analysis..."
    echo "--- OrcaWave Output ---"

    # Execute and capture output
    if timeout "${timeout}s" bash -c "$cmd" 2>&1 | tee "$log_file"; then
        return_code=0
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))

        echo ""
        print_success "OrcaWave completed successfully"
        print_info "Duration: ${duration}s ($((duration / 60))m $((duration % 60))s)"
    else
        return_code=$?
        local end_time=$(date +%s)
        local duration=$((end_time - start_time))

        echo ""
        if [ $return_code -eq 124 ]; then
            print_error "Process timed out after ${timeout}s"
        else
            print_error "OrcaWave failed with code: $return_code"
        fi
        print_info "Duration: ${duration}s"
    fi

    # Check output files
    echo ""
    print_info "Checking output files..."

    local output_count=0
    for file in "$output_dir"/*.sim "$output_dir"/*.csv "$output_dir"/*.dat; do
        if [ -f "$file" ]; then
            local size=$(du -h "$file" | cut -f1)
            print_success "$(basename "$file") ($size)"
            output_count=$((output_count + 1))
        fi
    done

    if [ $output_count -eq 0 ]; then
        print_warning "No output files found"
    fi

    echo ""
    return $return_code
}

# ============================================================================
# MAIN
# ============================================================================

main() {
    # Check arguments
    if [ $# -lt 1 ]; then
        echo "Usage: $0 <config_file.yml> [timeout_seconds]"
        echo ""
        echo "Examples:"
        echo "  $0 orcawave_001_ship_raos_rev2.yml"
        echo "  $0 my_config.yml 7200"
        exit 1
    fi

    local config_file="$1"
    local timeout="${2:-3600}"

    # Make absolute path
    if [[ ! "$config_file" = /* ]]; then
        config_file="$SCRIPT_DIR/$config_file"
    fi

    print_header "ORCAWAVE SHELL EXECUTION"
    print_info "Platform: $(uname -s)"
    print_info "Script Dir: $SCRIPT_DIR"
    print_info "Timestamp: $TIMESTAMP"
    echo ""

    # Find OrcaWave
    local orcawave_exe
    if ! orcawave_exe=$(find_orcawave); then
        print_error "Cannot proceed without OrcaWave executable"
        return 1
    fi

    echo ""

    # Validate config
    if ! validate_config "$config_file"; then
        print_error "Configuration validation failed"
        return 1
    fi

    # Execute
    if ! execute_orcawave "$config_file" "$orcawave_exe" "$timeout"; then
        print_error "Execution failed"
        return 1
    fi

    # Summary
    print_header "EXECUTION COMPLETE"
    print_success "Analysis finished successfully"
    print_info "Check logs: $SCRIPT_DIR/logs/"
    print_info "Check outputs: $SCRIPT_DIR/orcawave_output/"
    echo ""

    return 0
}

# Run main
main "$@"
