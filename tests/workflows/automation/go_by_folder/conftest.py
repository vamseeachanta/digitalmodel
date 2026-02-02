"""
Pytest fixtures for go-by folder tests
"""

import pytest
import tempfile
import shutil
from pathlib import Path
from typing import Generator, Dict, List
import json
import yaml


@pytest.fixture
def temp_dir() -> Generator[Path, None, None]:
    """Create a temporary directory for testing."""
    temp_path = Path(tempfile.mkdtemp())
    try:
        yield temp_path
    finally:
        if temp_path.exists():
            shutil.rmtree(temp_path)


@pytest.fixture
def sample_source_dir(temp_dir: Path) -> Path:
    """Create a sample source directory with various file types."""
    source_dir = temp_dir / "source"
    source_dir.mkdir(parents=True, exist_ok=True)
    
    # Create directory structure
    (source_dir / "src").mkdir()
    (source_dir / "data").mkdir()
    (source_dir / "config").mkdir()
    (source_dir / "output").mkdir()
    
    # Create sample files
    # Python files
    (source_dir / "src" / "main.py").write_text(
        "def main():\n    print('Hello World')\n\nif __name__ == '__main__':\n    main()\n"
    )
    (source_dir / "src" / "utils.py").write_text(
        "def helper():\n    return 42\n"
    )
    
    # Config files
    config_data = {"version": "1.0", "settings": {"param1": 10, "param2": "test"}}
    (source_dir / "config" / "settings.json").write_text(json.dumps(config_data, indent=2))
    (source_dir / "config" / "config.yaml").write_text(yaml.dump(config_data))
    
    # Data files
    (source_dir / "data" / "data_001.csv").write_text("id,value\n1,10\n2,20\n3,30\n")
    (source_dir / "data" / "data_002.csv").write_text("id,value\n4,40\n5,50\n6,60\n")
    (source_dir / "data" / "data_003.csv").write_text("id,value\n7,70\n8,80\n9,90\n")
    
    # Text file
    (source_dir / "README.md").write_text("# Sample Project\nThis is a test project.\n")
    
    # Large file (simulated)
    large_content = "Line {}\n" * 1000
    (source_dir / "output" / "large_output.txt").write_text(
        "\n".join(f"Line {i}" for i in range(1000))
    )
    
    # Binary file (small)
    (source_dir / "output" / "result.bin").write_bytes(b'\x00\x01\x02\x03' * 10)
    
    return source_dir


@pytest.fixture
def target_dir(temp_dir: Path) -> Path:
    """Create a target directory for go-by output."""
    target = temp_dir / "target"
    target.mkdir(parents=True, exist_ok=True)
    return target


@pytest.fixture
def config_dict() -> Dict:
    """Create a sample configuration dictionary."""
    return {
        'overwrite': True,
        'max_file_size': '10KB',
        'variation_coverage': 'medium',
        'verbose': False,
        'parallel': 2,
        'no_checkpoint': False,
        'yes_to_all': True,
        'no_interaction': True,
        'no_progress_bar': True,
        'monitor_memory': False
    }


@pytest.fixture
def parameter_sweep_files(temp_dir: Path) -> Path:
    """Create files with parameter sweep patterns."""
    sweep_dir = temp_dir / "parameter_sweep"
    sweep_dir.mkdir(parents=True, exist_ok=True)
    
    # Create files with numeric parameters
    for temp in [100, 200, 300]:
        for pressure in [1.0, 2.0, 3.0]:
            filename = f"sim_temp={temp}_pressure={pressure}.dat"
            (sweep_dir / filename).write_text(f"Simulation data T={temp} P={pressure}")
    
    # Create files with sequential numbering
    for i in range(1, 6):
        (sweep_dir / f"run_{i:03d}.log").write_text(f"Run {i} log data")
    
    return sweep_dir


@pytest.fixture
def mock_file_info() -> List[Dict]:
    """Create mock file information list."""
    return [
        {
            'path': Path('test1.txt'),
            'name': 'test1.txt',
            'extension': '.txt',
            'size': 1024,
            'modified': 1234567890,
            'is_binary': False
        },
        {
            'path': Path('test2.py'),
            'name': 'test2.py',
            'extension': '.py',
            'size': 2048,
            'modified': 1234567900,
            'is_binary': False
        },
        {
            'path': Path('data.bin'),
            'name': 'data.bin',
            'extension': '.bin',
            'size': 4096,
            'modified': 1234567910,
            'is_binary': True
        }
    ]


@pytest.fixture
def mock_patterns() -> Dict:
    """Create mock pattern detection results."""
    return {
        'numeric_sequences': ['file_{001..100}', 'run_{01..10}'],
        'naming_conventions': {
            'snake_case': ['my_file.py', 'test_data.csv'],
            'camelCase': ['myFile.js', 'testData.json']
        },
        'parameter_variations': {
            'temp': [100, 200, 300],
            'pressure': [1.0, 2.0, 3.0]
        },
        'file_groups': {
            'simulation': ['sim_001.dat', 'sim_002.dat'],
            'logs': ['run_001.log', 'run_002.log']
        }
    }