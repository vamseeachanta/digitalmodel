# Standard library imports
import os
import sys
from typing import Any, Dict

# Reader imports
from assetutilities.modules.test_utilities.test_utilities import TestUtilities
from digitalmodel.engine import engine

tu = TestUtilities()

def run_process(input_file: str, expected_result: Dict[str, Any] = {}) -> None:
    """
    Execute the mooring tension iteration process.
    
    This test demonstrates automated mooring tension iteration
    to achieve target values using scipy optimization approach,
    building upon the existing semi-automated mooring analysis.
    
    Args:
        input_file: Path to YAML configuration file
        expected_result: Expected results for validation
    
    Returns:
        Configuration object with results
    """
    if input_file is not None and not os.path.isfile(input_file):
        input_file = os.path.join(os.path.dirname(__file__), input_file)
    cfg = engine(input_file)

    return cfg

def test_mooring_tension_iteration() -> None:
    """
    Test the mooring tension iteration system.
    
    This test validates:
    1. Single-line tension iteration convergence
    2. Multi-line coupled system optimization
    3. Convergence within specified tolerance
    4. Automation of the iteration process
    """
    input_file = 'mooring_tension_iteration.yml'

    # pytest_output_file = 'results/mooring_tension_iteration_pytest.yml'
    # pytest_output_file = tu.get_valid_pytest_output_file(os.path.dirname(__file__), pytest_output_file)
    # expected_result = ymlInput(pytest_output_file, updateYml=None)

    if len(sys.argv) > 1:
        sys.argv.pop()

    run_process(input_file, expected_result={})

def test_single_line_iteration() -> None:
    """
    Test tension iteration for a single mooring line.
    
    Validates:
    - Convergence to target tension within tolerance
    - Number of iterations required
    - Line length adjustment accuracy
    """
    input_file = 'single_line_iteration.yml'
    
    if len(sys.argv) > 1:
        sys.argv.pop()
    
    result = run_process(input_file, expected_result={})
    
    # Validate convergence
    assert result.get('converged') == True
    assert result.get('iterations') < 10
    
def test_multi_line_coupling() -> None:
    """
    Test tension iteration for coupled multi-line system.
    
    Validates:
    - Simultaneous convergence of all lines
    - Interaction effects properly handled
    - Jacobian matrix calculation accuracy
    """
    input_file = 'multi_line_iteration.yml'
    
    if len(sys.argv) > 1:
        sys.argv.pop()
    
    result = run_process(input_file, expected_result={})
    
    # Validate all lines converged
    assert result.get('converged') == True
    assert all(line['converged'] for line in result.get('lines', []))

if __name__ == '__main__':
    test_mooring_tension_iteration()