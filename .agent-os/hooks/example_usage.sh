#!/bin/bash
# Example usage of verification hooks for mooring tension iteration

# Source the verification hooks
source "$(dirname "$0")/verification_hooks.sh"

# ============================================================================
# EXAMPLE: Complete OrcaFlex Mooring Tension Iteration Verification
# ============================================================================

run_mooring_tension_iteration() {
    local WORKING_DIR="specs/modules/orcaflex/mooring-tension-iteration/go-by"
    local VENV_PATH=".venv"
    
    # Start timer
    local start_time=$(date +%s)
    local checks_passed=0
    local checks_failed=0
    
    echo -e "${YELLOW}╔═══════════════════════════════════════════════════════╗${NC}"
    echo -e "${YELLOW}║     MOORING TENSION ITERATION VERIFICATION           ║${NC}"
    echo -e "${YELLOW}╚═══════════════════════════════════════════════════════╝${NC}"
    
    # ========================================
    # PHASE 1: Pre-execution checks
    # ========================================
    pre_execution_check "$WORKING_DIR" \
        "dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml" \
        "dm_ofx_post_fsts_lngc.yml" \
        "fsts_l015_125km3_pb_target_mooring_pretension.csv" \
        "fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof.dat"
    
    # Verify CSV structure
    verify_csv_structure \
        "$WORKING_DIR/fsts_l015_125km3_pb_target_mooring_pretension.csv" \
        "ObjectName,target_tension" \
        16  # Expect 16 mooring lines
    
    # Check OrcaFlex license
    verify_orcaflex_license
    
    # ========================================
    # PHASE 2: Execute commands with verification
    # ========================================
    
    # Step 1: Tension calculation
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}STEP 1: TENSION CALCULATION${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
    
    cd "$WORKING_DIR"
    source "$VENV_PATH/Scripts/activate"
    
    # Run command and capture return code
    python -m digitalmodel dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml
    local ret1=$?
    
    # Verify execution
    if [[ $ret1 -eq 0 ]]; then
        echo -e "${GREEN}[✓ PASS]${NC} Tension calculation completed (Return code: $ret1)"
        ((checks_passed++))
        
        # Verify output files were created
        verify_file_created "results/dm_ofx_anal_mooring_fsts_l015_125km3_pb.yml" 5 "Config output"
        
    else
        echo -e "${RED}[✗ FAIL]${NC} Tension calculation failed (Return code: $ret1)"
        ((checks_failed++))
    fi
    
    # Step 2: OrcaFlex analysis
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}STEP 2: ORCAFLEX ANALYSIS${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
    
    python -m digitalmodel.modules.orcaflex.universal \
        pattern="fsts*125km3*pb_*.yml" \
        input_directory="." \
        output_directory="." \
        validate=false
    local ret2=$?
    
    if [[ $ret2 -eq 0 ]]; then
        echo -e "${GREEN}[✓ PASS]${NC} OrcaFlex analysis completed (Return code: $ret2)"
        ((checks_passed++))
        
        # Verify simulation outputs
        verify_orcaflex_output "." "*.sim"
        
    else
        echo -e "${RED}[✗ FAIL]${NC} OrcaFlex analysis failed (Return code: $ret2)"
        ((checks_failed++))
    fi
    
    # Step 3: Post-processing
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}STEP 3: POST-PROCESSING${NC}"
    echo -e "${BLUE}═══════════════════════════════════════════════════${NC}"
    
    python -m digitalmodel dm_ofx_post_fsts_lngc.yml --workers 30
    local ret3=$?
    
    if [[ $ret3 -eq 0 ]]; then
        echo -e "${GREEN}[✓ PASS]${NC} Post-processing completed (Return code: $ret3)"
        ((checks_passed++))
        
        # Verify final output CSV
        verify_file_created \
            "results/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv" \
            10 \
            "Pretension analysis output"
        
        # Verify CSV has correct data
        verify_csv_structure \
            "results/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv" \
            "ObjectName,EffectiveTension" \
            16
            
    else
        echo -e "${RED}[✗ FAIL]${NC} Post-processing failed (Return code: $ret3)"
        ((checks_failed++))
    fi
    
    # ========================================
    # PHASE 3: Post-execution verification
    # ========================================
    
    # Check all expected outputs
    post_execution_check 0 \
        "results/fsts_l015_hwl_125km3_l100_pb_vessel_statics_6dof_pretension_analysis.csv"
    
    # Check logs for errors
    if [[ -f "logs/iteration.log" ]]; then
        verify_log_no_errors "logs/iteration.log" "OrcaFlex license"
    fi
    
    # ========================================
    # PHASE 4: Summary
    # ========================================
    
    local end_time=$(date +%s)
    verification_summary "$checks_passed" "$checks_failed" "$start_time" "$end_time"
    
    return $?
}

# ============================================================================
# EXAMPLE: Generic command execution with full verification
# ============================================================================

run_with_verification() {
    local cmd="$1"
    local expected_output="$2"
    local output_files=("${@:3}")
    
    echo -e "${YELLOW}Running with verification: $cmd${NC}"
    
    # Pre-execution
    pre_execution_check "." ""
    
    # Execute
    verify_return_code "$cmd" "Command execution"
    
    # Verify output
    if [[ -n "$expected_output" ]]; then
        verify_output_contains "$cmd" "$expected_output" "Output check"
    fi
    
    # Post-execution
    post_execution_check $? "${output_files[@]}"
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

# Check if being sourced or executed
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    # Script is being executed directly
    
    # Parse command line arguments
    case "${1:-}" in
        "mooring")
            run_mooring_tension_iteration
            ;;
        "test")
            # Run a simple test
            run_with_verification "echo 'Hello World'" "Hello" 
            ;;
        *)
            echo "Usage: $0 {mooring|test}"
            echo ""
            echo "Examples:"
            echo "  $0 mooring  - Run mooring tension iteration with full verification"
            echo "  $0 test     - Run simple test with verification"
            exit 1
            ;;
    esac
fi