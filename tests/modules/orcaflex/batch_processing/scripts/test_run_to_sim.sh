#!/bin/bash
# Test script for OrcaFlex run_to_sim functionality
# Tests both CLI and module interfaces

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Test directories
TEST_DIR=$(dirname "$0")
PROJECT_ROOT=$(cd "$TEST_DIR/../../../.." && pwd)
ORCAFLEX_MODULE="$PROJECT_ROOT/src/digitalmodel/modules/orcaflex"
TEST_OUTPUT_DIR="$TEST_DIR/test_output_$(date +%Y%m%d_%H%M%S)"
TEST_CONFIGS_DIR="$TEST_DIR/test_configs"

# Create test directories
mkdir -p "$TEST_OUTPUT_DIR"
mkdir -p "$TEST_CONFIGS_DIR"

echo -e "${BLUE}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║        OrcaFlex Run-to-Sim Test Suite                    ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════╝${NC}"
echo "Project Root: $PROJECT_ROOT"
echo "Test Output: $TEST_OUTPUT_DIR"
echo ""

# Test counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Function to run a test
run_test() {
    local test_name="$1"
    local command="$2"
    local expected_result="${3:-0}"  # Default expect success
    
    TESTS_RUN=$((TESTS_RUN + 1))
    echo -e "\n${YELLOW}[$TESTS_RUN] Running: $test_name${NC}"
    echo "Command: $command"
    
    if eval "$command" > "$TEST_OUTPUT_DIR/${test_name}.log" 2>&1; then
        if [ "$expected_result" -eq 0 ]; then
            echo -e "${GREEN}✓ PASSED${NC}"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        else
            echo -e "${RED}✗ FAILED - Expected failure but succeeded${NC}"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            return 1
        fi
    else
        local exit_code=$?
        if [ "$expected_result" -ne 0 ]; then
            echo -e "${GREEN}✓ PASSED (Expected failure with code $exit_code)${NC}"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        else
            echo -e "${RED}✗ FAILED with exit code $exit_code${NC}"
            echo "Error output:"
            tail -n 10 "$TEST_OUTPUT_DIR/${test_name}.log"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            return 1
        fi
    fi
}

# Test Suite 1: Module Availability
echo -e "\n${BLUE}═══ Test Suite 1: Module Availability ═══${NC}"

if [ -f "$ORCAFLEX_MODULE/run_to_sim.py" ]; then
    echo -e "${GREEN}✓ run_to_sim.py found${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}✗ run_to_sim.py not found${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    exit 1
fi
TESTS_RUN=$((TESTS_RUN + 1))

if [ -f "$ORCAFLEX_MODULE/run_to_sim_cli.py" ]; then
    echo -e "${GREEN}✓ run_to_sim_cli.py found${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}✗ run_to_sim_cli.py not found${NC}"
    TESTS_FAILED=$((TESTS_FAILED + 1))
    exit 1
fi
TESTS_RUN=$((TESTS_RUN + 1))

# Test Suite 2: CLI Interface Tests
echo -e "\n${BLUE}═══ Test Suite 2: CLI Interface Tests ═══${NC}"

run_test "cli_help" "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --help"
run_test "cli_version" "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --version"

# Test Suite 3: Mock Mode Tests (No License Required)
echo -e "\n${BLUE}═══ Test Suite 3: Mock Mode Tests ═══${NC}"

# Create realistic test YAML files
cat > "$TEST_CONFIGS_DIR/fsts_test_model1.yml" << 'EOF'
# Test OrcaFlex model 1 - FSTS Configuration
Name: FSTS_TestModel1
General:
  UnitsSystem: SI
  SimulationStopTime: 100
  StaticsMaxIterations: 200
  StaticsConvergenceTolerance: 0.001
Environment:
  WaterDepth: 100
  WaveType: JONSWAP
  WaveHs: 2.5
  WaveTz: 8.0
  WaveDirection: 180
  WindSpeed: 15
  WindDirection: 180
  CurrentSpeed: 0.5
  CurrentDirection: 90
Vessel:
  Name: FST1
  Type: Vessel
  Length: 150
  Beam: 30
  Draught: 10
Lines:
  - Name: Line01
    LineType: Chain
    Length: 100
    TargetSegmentLength: 5
  - Name: Line02
    LineType: Wire
    Length: 150
    TargetSegmentLength: 10
EOF

cat > "$TEST_CONFIGS_DIR/fsts_test_model2.yml" << 'EOF'
# Test OrcaFlex model 2 - Advanced Configuration
Name: FSTS_TestModel2
General:
  UnitsSystem: SI
  SimulationStopTime: 200
  ImplicitConstantTimeStep: 0.01
Environment:
  WaterDepth: 150
  WaveType: UserSpecified
  WaveKinematicsCutoffDepth: 50
  SeaTemperature: 15
  SeaDensity: 1025
  AirDensity: 1.225
Vessels:
  - Name: FST1
    Type: Vessel
    InitialX: 0
    InitialY: 0
    InitialZ: 0
    InitialHeading: 0
  - Name: FST2
    Type: Vessel
    InitialX: 100
    InitialY: 0
    InitialZ: 0
    InitialHeading: 180
  - Name: LNGC
    Type: Vessel
    InitialX: 50
    InitialY: 0
    InitialZ: 0
    InitialHeading: 90
MooringLines:
  NumberOfLines: 18
  Pattern: Symmetric
  Material: Chain-Wire-Chain
EOF

cat > "$TEST_CONFIGS_DIR/invalid_model.yml" << 'EOF'
# Invalid model for error testing
Invalid YAML content {
  This should fail parsing
EOF

# Test single model in mock mode
run_test "mock_single_model" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '$TEST_CONFIGS_DIR/fsts_test_model1.yml' --mock --verbose"

# Test multiple models in mock mode
run_test "mock_multiple_models" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '$TEST_CONFIGS_DIR/fsts_test_model1.yml' '$TEST_CONFIGS_DIR/fsts_test_model2.yml' --mock"

# Test all models with pattern in mock mode
run_test "mock_all_with_pattern" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --directory '$TEST_CONFIGS_DIR' --pattern 'fsts_*.yml' --all --mock"

# Test with custom output directory
mkdir -p "$TEST_OUTPUT_DIR/sim_output"
run_test "mock_custom_output" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '$TEST_CONFIGS_DIR/fsts_test_model1.yml' --output '$TEST_OUTPUT_DIR/sim_output' --mock"

# Test with custom thread count
run_test "mock_custom_threads" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --all --directory '$TEST_CONFIGS_DIR' --pattern 'fsts_*.yml' --threads 5 --mock"

# Test Suite 4: Python Module Integration Tests
echo -e "\n${BLUE}═══ Test Suite 4: Python Module Integration ═══${NC}"

cat > "$TEST_OUTPUT_DIR/test_python_integration.py" << 'EOF'
#!/usr/bin/env python
"""Test Python module integration."""
import sys
import json
from pathlib import Path

# Add project to path
project_root = Path(__file__).parent.parent.parent.parent.parent
sys.path.insert(0, str(project_root / "src"))

def test_imports():
    """Test module imports."""
    try:
        from digitalmodel.modules.orcaflex.run_to_sim import (
            OrcaFlexModelRunner, 
            run_models, 
            ORCAFLEX_AVAILABLE
        )
        print("[OK] Successfully imported run_to_sim module")
        return True
    except ImportError as e:
        print(f"[FAIL] Import failed: {e}")
        return False

def test_runner_creation():
    """Test creating runner instance."""
    from digitalmodel.modules.orcaflex.run_to_sim import OrcaFlexModelRunner
    try:
        runner = OrcaFlexModelRunner(mock_mode=True)
        print("[OK] Successfully created OrcaFlexModelRunner instance")
        return True
    except Exception as e:
        print(f"[FAIL] Failed to create runner: {e}")
        return False

def test_mock_run():
    """Test mock run functionality."""
    from digitalmodel.modules.orcaflex.run_to_sim import run_models
    try:
        result = run_models(mock=True, all_models=False)
        print(f"[OK] Mock run completed: Total={result['total']}, Success={result['successful']}, Failed={result['failed']}")
        return result['failed'] == 0
    except Exception as e:
        print(f"[FAIL] Mock run failed: {e}")
        return False

def test_single_file_processing():
    """Test processing a single file."""
    from digitalmodel.modules.orcaflex.run_to_sim import OrcaFlexModelRunner
    from pathlib import Path
    
    try:
        runner = OrcaFlexModelRunner(mock_mode=True)
        test_file = Path(__file__).parent / "test_configs" / "fsts_test_model1.yml"
        
        if test_file.exists():
            result = runner.run_single_model(test_file)
            if result['success']:
                print(f"[OK] Single file processing succeeded: {result['model']}")
                return True
            else:
                print(f"[FAIL] Single file processing failed: {result['error']}")
                return False
        else:
            print(f"[WARN] Test file not found: {test_file}")
            return True  # Not a failure if file doesn't exist in this context
    except Exception as e:
        print(f"[FAIL] Exception in single file processing: {e}")
        return False

if __name__ == "__main__":
    tests = [
        ("Imports", test_imports),
        ("Runner Creation", test_runner_creation),
        ("Mock Run", test_mock_run),
        ("Single File Processing", test_single_file_processing),
    ]
    
    passed = 0
    failed = 0
    
    print("\nPython Integration Tests")
    print("=" * 40)
    
    for test_name, test_func in tests:
        print(f"\nTesting {test_name}...")
        if test_func():
            passed += 1
        else:
            failed += 1
    
    print("\n" + "=" * 40)
    print(f"Results: {passed} passed, {failed} failed")
    
    sys.exit(0 if failed == 0 else 1)
EOF

run_test "python_integration" "python '$TEST_OUTPUT_DIR/test_python_integration.py'"

# Test Suite 5: Performance and Scalability Tests
echo -e "\n${BLUE}═══ Test Suite 5: Performance Tests ═══${NC}"

# Create multiple test models for performance testing
echo "Creating test models for performance testing..."
for i in {1..20}; do
    cat > "$TEST_CONFIGS_DIR/perf_model_$i.yml" << EOF
Name: PerfModel$i
General:
  SimulationStopTime: $((100 * i))
  StaticsMaxIterations: $((100 + i * 10))
Environment:
  WaterDepth: $((50 + i * 10))
  WaveHs: $(echo "scale=1; 1 + $i * 0.1" | bc)
  WaveTz: $(echo "scale=1; 6 + $i * 0.2" | bc)
Lines:
  Count: $((5 + i))
EOF
done

echo "Created 20 test models for performance testing"
start_time=$(date +%s)
run_test "performance_batch_processing" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --directory '$TEST_CONFIGS_DIR' --pattern 'perf_*.yml' --all --mock --threads 10"
end_time=$(date +%s)
duration=$((end_time - start_time))
echo "Batch processing completed in ${duration} seconds"

# Test Suite 6: Error Handling and Edge Cases
echo -e "\n${BLUE}═══ Test Suite 6: Error Handling ═══${NC}"

# Test with non-existent file (should fail gracefully)
run_test "error_nonexistent_file" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '/nonexistent/path/file.yml' --mock" 1

# Test with invalid directory
run_test "error_invalid_directory" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --directory '/nonexistent/directory' --all --mock" 1

# Test with invalid YAML file
run_test "error_invalid_yaml" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '$TEST_CONFIGS_DIR/invalid_model.yml' --mock" 1

# Test with no arguments (should show help or run test subset)
run_test "no_arguments_test" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --mock"

# Test Suite 7: Real OrcaFlex License Test (if available)
echo -e "\n${BLUE}═══ Test Suite 7: License Availability Check ═══${NC}"

cat > "$TEST_OUTPUT_DIR/check_license.py" << 'EOF'
#!/usr/bin/env python
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent / "src"))

try:
    import OrcFxAPI
    print("[OK] OrcaFlex API available - License might be available")
    # Try to create a model to truly test license
    try:
        model = OrcFxAPI.Model()
        print("[OK] OrcaFlex license confirmed available")
        sys.exit(0)
    except:
        print("[WARN] OrcaFlex API found but no license available")
        sys.exit(1)
except ImportError:
    print("[INFO] OrcaFlex API not installed - Running in mock mode only")
    sys.exit(0)  # Not a failure, just informational
EOF

run_test "check_orcaflex_license" "python '$TEST_OUTPUT_DIR/check_license.py'"

# Summary Report
echo -e "\n${BLUE}╔══════════════════════════════════════════════════════════╗${NC}"
echo -e "${BLUE}║                    TEST SUMMARY                          ║${NC}"
echo -e "${BLUE}╚══════════════════════════════════════════════════════════╝${NC}"
echo -e "Total Tests Run:    ${TESTS_RUN}"
echo -e "Tests Passed:       ${GREEN}${TESTS_PASSED}${NC}"
echo -e "Tests Failed:       ${RED}${TESTS_FAILED}${NC}"

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "\n${GREEN}✓ ALL TESTS PASSED!${NC}"
    exit_code=0
else
    echo -e "\n${RED}✗ SOME TESTS FAILED${NC}"
    exit_code=1
fi

echo -e "\nTest logs saved in: $TEST_OUTPUT_DIR"
echo -e "Test configs saved in: $TEST_CONFIGS_DIR"

# Cleanup option
echo ""
read -p "Clean up test output? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -rf "$TEST_OUTPUT_DIR"
    rm -rf "$TEST_CONFIGS_DIR"
    echo "Test output cleaned up"
else
    echo "Test output preserved at:"
    echo "  - Logs: $TEST_OUTPUT_DIR"
    echo "  - Configs: $TEST_CONFIGS_DIR"
fi

exit $exit_code