#!/bin/bash
# Test script for OrcaFlex run_to_sim functionality
# Tests both CLI and module interfaces

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test directories
TEST_DIR=$(dirname "$0")
PROJECT_ROOT=$(cd "$TEST_DIR/../../../.." && pwd)
ORCAFLEX_MODULE="$PROJECT_ROOT/src/digitalmodel/modules/orcaflex"
TEST_OUTPUT_DIR="$TEST_DIR/test_output_$(date +%Y%m%d_%H%M%S)"

# Create test output directory
mkdir -p "$TEST_OUTPUT_DIR"

echo -e "${GREEN}========================================${NC}"
echo -e "${GREEN}OrcaFlex Run-to-Sim Test Suite${NC}"
echo -e "${GREEN}========================================${NC}"
echo "Project Root: $PROJECT_ROOT"
echo "Test Output: $TEST_OUTPUT_DIR"
echo ""

# Function to run a test
run_test() {
    local test_name="$1"
    local command="$2"
    local expected_result="${3:-0}"  # Default expect success
    
    echo -e "${YELLOW}Running: $test_name${NC}"
    echo "Command: $command"
    
    if eval "$command" > "$TEST_OUTPUT_DIR/${test_name}.log" 2>&1; then
        if [ "$expected_result" -eq 0 ]; then
            echo -e "${GREEN}✓ PASSED${NC}"
            return 0
        else
            echo -e "${RED}✗ FAILED - Expected failure but succeeded${NC}"
            return 1
        fi
    else
        local exit_code=$?
        if [ "$expected_result" -ne 0 ]; then
            echo -e "${GREEN}✓ PASSED (Expected failure)${NC}"
            return 0
        else
            echo -e "${RED}✗ FAILED with exit code $exit_code${NC}"
            cat "$TEST_OUTPUT_DIR/${test_name}.log"
            return 1
        fi
    fi
}

# Test 1: Check if modules exist
echo -e "\n${YELLOW}Test 1: Module Availability${NC}"
if [ -f "$ORCAFLEX_MODULE/run_to_sim.py" ]; then
    echo -e "${GREEN}✓ run_to_sim.py found${NC}"
else
    echo -e "${RED}✗ run_to_sim.py not found${NC}"
    exit 1
fi

if [ -f "$ORCAFLEX_MODULE/run_to_sim_cli.py" ]; then
    echo -e "${GREEN}✓ run_to_sim_cli.py found${NC}"
else
    echo -e "${RED}✗ run_to_sim_cli.py not found${NC}"
    exit 1
fi

# Test 2: Test CLI help
echo -e "\n${YELLOW}Test 2: CLI Help${NC}"
run_test "cli_help" "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --help"

# Test 3: Test with mock mode (no OrcaFlex license required)
echo -e "\n${YELLOW}Test 3: Mock Mode Tests${NC}"

# Create test YAML files
cat > "$TEST_OUTPUT_DIR/test_model1.yml" << 'EOF'
# Test OrcaFlex model 1
Name: TestModel1
General:
  UnitsSystem: SI
  SimulationStopTime: 100
Environment:
  WaterDepth: 100
  WaveType: Regular
  WaveHeight: 2.0
  WavePeriod: 8.0
EOF

cat > "$TEST_OUTPUT_DIR/test_model2.yml" << 'EOF'
# Test OrcaFlex model 2
Name: TestModel2
General:
  UnitsSystem: SI
  SimulationStopTime: 200
Environment:
  WaterDepth: 150
  WaveType: JONSWAP
  WaveHs: 3.0
  WaveTz: 10.0
EOF

# Test single model in mock mode
run_test "mock_single_model" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '$TEST_OUTPUT_DIR/test_model1.yml' --mock --verbose"

# Test multiple models in mock mode
run_test "mock_multiple_models" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '$TEST_OUTPUT_DIR/test_model1.yml' '$TEST_OUTPUT_DIR/test_model2.yml' --mock"

# Test all models with pattern in mock mode
run_test "mock_all_with_pattern" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --directory '$TEST_OUTPUT_DIR' --pattern 'test_*.yml' --all --mock"

# Test with custom output directory
run_test "mock_custom_output" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '$TEST_OUTPUT_DIR/test_model1.yml' --output '$TEST_OUTPUT_DIR/sim_output' --mock"

# Test with custom thread count
run_test "mock_custom_threads" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --all --directory '$TEST_OUTPUT_DIR' --threads 5 --mock"

# Test 4: Python module import test
echo -e "\n${YELLOW}Test 4: Python Module Import${NC}"
cat > "$TEST_OUTPUT_DIR/test_import.py" << 'EOF'
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent / "src"))

try:
    from digitalmodel.modules.orcaflex.run_to_sim import OrcaFlexModelRunner, run_models
    print("✓ Successfully imported run_to_sim module")
    
    # Test creating runner instance
    runner = OrcaFlexModelRunner(mock_mode=True)
    print("✓ Successfully created OrcaFlexModelRunner instance")
    
    # Test mock run
    result = run_models(mock=True, all_models=False)
    print(f"✓ Mock run completed: {result}")
    
except ImportError as e:
    print(f"✗ Import failed: {e}")
    sys.exit(1)
except Exception as e:
    print(f"✗ Unexpected error: {e}")
    sys.exit(1)
EOF

run_test "python_import" "python '$TEST_OUTPUT_DIR/test_import.py'"

# Test 5: Performance test with multiple files
echo -e "\n${YELLOW}Test 5: Performance Test (Mock)${NC}"
# Create 10 test models
for i in {1..10}; do
    cat > "$TEST_OUTPUT_DIR/perf_model_$i.yml" << EOF
Name: PerfModel$i
General:
  SimulationStopTime: $((100 * i))
Environment:
  WaterDepth: $((50 + i * 10))
EOF
done

echo "Created 10 test models for performance testing"
start_time=$(date +%s)
run_test "performance_test" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --directory '$TEST_OUTPUT_DIR' --pattern 'perf_*.yml' --all --mock --threads 10"
end_time=$(date +%s)
duration=$((end_time - start_time))
echo "Performance test completed in ${duration} seconds"

# Test 6: Error handling tests
echo -e "\n${YELLOW}Test 6: Error Handling${NC}"

# Test with non-existent file (should fail)
run_test "error_nonexistent_file" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --models '/nonexistent/file.yml' --mock" 1

# Test with invalid directory (should handle gracefully)
run_test "error_invalid_directory" \
    "python '$ORCAFLEX_MODULE/run_to_sim_cli.py' --directory '/nonexistent/directory' --all --mock" 1

# Summary
echo -e "\n${GREEN}========================================${NC}"
echo -e "${GREEN}Test Suite Complete${NC}"
echo -e "${GREEN}========================================${NC}"
echo "Test output saved in: $TEST_OUTPUT_DIR"
echo ""

# Cleanup option
read -p "Clean up test output directory? (y/n) " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -rf "$TEST_OUTPUT_DIR"
    echo "Test output cleaned up"
else
    echo "Test output preserved at: $TEST_OUTPUT_DIR"
fi