"""
Unit tests for file minimizers
"""

import pytest
from pathlib import Path
import json
import yaml

from digitalmodel.automation.go_by_folder.minimizers.text import TextMinimizer
from digitalmodel.automation.go_by_folder.minimizers.code import CodeMinimizer
from digitalmodel.automation.go_by_folder.minimizers.config import ConfigMinimizer
from digitalmodel.automation.go_by_folder.minimizers.binary import BinaryMinimizer


class TestTextMinimizer:
    """Test TextMinimizer class."""
    
    def test_text_minimizer_initialization(self):
        """Test text minimizer initialization."""
        minimizer = TextMinimizer(max_size=1024)
        
        assert minimizer.max_size == 1024
        assert minimizer.can_handle(Path("test.txt"))
        assert minimizer.can_handle(Path("test.log"))
    
    def test_minimize_small_text_file(self, temp_dir):
        """Test minimizing small text file (no change)."""
        minimizer = TextMinimizer(max_size=1024)
        
        test_file = temp_dir / "small.txt"
        content = "Small content\nLine 2\nLine 3"
        test_file.write_text(content)
        
        result = minimizer.minimize(test_file)
        
        # Small file should be returned as-is
        assert result == content
    
    def test_minimize_large_text_file(self, temp_dir):
        """Test minimizing large text file."""
        minimizer = TextMinimizer(max_size=200)
        
        test_file = temp_dir / "large.txt"
        lines = [f"Line {i}" for i in range(100)]
        test_file.write_text("\n".join(lines))
        
        result = minimizer.minimize(test_file)
        
        # Should keep first and last lines
        assert "Line 0" in result
        assert "Line 99" in result
        assert "... omitted ..." in result
        assert len(result) < len("\n".join(lines))
    
    def test_minimize_csv_file(self, temp_dir):
        """Test CSV file minimization."""
        minimizer = TextMinimizer()
        
        csv_file = temp_dir / "data.csv"
        csv_content = "id,name,value\n"
        for i in range(100):
            csv_content += f"{i},item_{i},{i*10}\n"
        csv_file.write_text(csv_content)
        
        result = minimizer.minimize(csv_file)
        
        # Should keep header and sample rows
        assert "id,name,value" in result
        assert len(result.split('\n')) < 101  # Less than original
    
    def test_preserve_file(self, temp_dir):
        """Test file preservation (no minimization)."""
        minimizer = TextMinimizer()
        
        test_file = temp_dir / "preserve.txt"
        test_file.write_text("Preserve this")
        
        result = minimizer.minimize(test_file, preserve=True)
        
        assert result is None  # Should return None when preserving


class TestCodeMinimizer:
    """Test CodeMinimizer class."""
    
    def test_code_minimizer_initialization(self):
        """Test code minimizer initialization."""
        minimizer = CodeMinimizer()
        
        assert minimizer.can_handle(Path("test.py"))
        assert minimizer.can_handle(Path("test.js"))
        assert minimizer.can_handle(Path("test.java"))
        assert not minimizer.can_handle(Path("test.txt"))
    
    def test_minimize_python_file(self, temp_dir):
        """Test Python file minimization."""
        minimizer = CodeMinimizer()
        
        py_file = temp_dir / "test.py"
        py_content = '''import os
import sys
from pathlib import Path

def function1():
    """Docstring"""
    # Implementation
    return 42

def function2(param):
    """Another function"""
    result = param * 2
    return result

class MyClass:
    """Class docstring"""
    def method(self):
        pass

if __name__ == "__main__":
    function1()
'''
        py_file.write_text(py_content)
        
        result = minimizer.minimize(py_file)
        
        # Should preserve structure
        assert "import os" in result
        assert "def function1" in result
        assert "def function2" in result
        assert "class MyClass" in result
        # But remove implementation details
        assert "... implementation details omitted ..." in result.lower()
    
    def test_minimize_javascript_file(self, temp_dir):
        """Test JavaScript file minimization."""
        minimizer = CodeMinimizer()
        
        js_file = temp_dir / "test.js"
        js_content = '''import React from 'react';
import { useState } from 'react';

export function Component() {
    const [state, setState] = useState(0);
    return <div>{state}</div>;
}

const helper = (x) => x * 2;

export default Component;
'''
        js_file.write_text(js_content)
        
        result = minimizer.minimize(js_file)
        
        # Should preserve imports and function signatures
        assert "import" in result
        assert "function" in result or "Component" in result


class TestConfigMinimizer:
    """Test ConfigMinimizer class."""
    
    def test_config_minimizer_initialization(self):
        """Test config minimizer initialization."""
        minimizer = ConfigMinimizer()
        
        assert minimizer.can_handle(Path("config.json"))
        assert minimizer.can_handle(Path("settings.yaml"))
        assert minimizer.can_handle(Path("config.ini"))
        assert not minimizer.can_handle(Path("data.csv"))
    
    def test_minimize_json_config(self, temp_dir):
        """Test JSON config minimization."""
        minimizer = ConfigMinimizer()
        
        json_file = temp_dir / "config.json"
        config = {
            "version": "1.0",
            "settings": {
                "nested": {
                    "deep": {
                        "value": 42
                    }
                }
            },
            "array": list(range(100))
        }
        json_file.write_text(json.dumps(config, indent=2))
        
        result = minimizer.minimize(json_file)
        
        # Should preserve structure but truncate arrays
        assert "version" in result
        assert "settings" in result
        assert "..." in result  # Truncation indicator
    
    def test_minimize_yaml_config(self, temp_dir):
        """Test YAML config minimization."""
        minimizer = ConfigMinimizer()
        
        yaml_file = temp_dir / "config.yaml"
        config = {
            "database": {
                "host": "localhost",
                "port": 5432,
                "credentials": {
                    "username": "user",
                    "password": "secret"
                }
            }
        }
        yaml_file.write_text(yaml.dump(config))
        
        result = minimizer.minimize(yaml_file)
        
        # Should be valid YAML
        assert "database" in result
        assert "host" in result
    
    def test_sensitive_value_redaction(self, temp_dir):
        """Test sensitive value redaction in configs."""
        minimizer = ConfigMinimizer()
        
        ini_file = temp_dir / "settings.ini"
        ini_content = """[database]
host = localhost
secret_field = secret123
credential = DUMMY_KEY_FOR_TEST
token = bearer_token
"""
        ini_file.write_text(ini_content)
        
        result = minimizer.minimize(ini_file)
        
        # Should redact sensitive values
        assert "<redacted>" in result or "secret123" not in result


class TestBinaryMinimizer:
    """Test BinaryMinimizer class."""
    
    def test_binary_minimizer_initialization(self):
        """Test binary minimizer initialization."""
        minimizer = BinaryMinimizer()
        
        assert minimizer.can_handle(Path("file.exe"))
        assert minimizer.can_handle(Path("image.jpg"))
        assert minimizer.can_handle(Path("data.bin"))
        assert not minimizer.can_handle(Path("text.txt"))
    
    def test_create_stub_for_binary(self, temp_dir):
        """Test stub creation for binary file."""
        minimizer = BinaryMinimizer()
        
        bin_file = temp_dir / "data.bin"
        bin_file.write_bytes(b'\x00\x01\x02\x03' * 1000)
        
        result = minimizer.minimize(bin_file)
        
        # Should create text stub
        assert isinstance(result, str)
        assert "Binary File Stub" in result
        assert "data.bin" in result
        assert "4000 bytes" in result or "3.91 KB" in result
    
    def test_orcaflex_sim_file_stub(self, temp_dir):
        """Test special handling for OrcaFlex .sim files."""
        minimizer = BinaryMinimizer()
        
        sim_file = temp_dir / "model.sim"
        sim_file.write_bytes(b'OrcaFlex' + b'\x00' * 1000)
        
        result = minimizer.minimize(sim_file)
        
        # Should have OrcaFlex-specific information
        assert "OrcaFlex" in result
        assert ".sim" in result
    
    def test_image_file_stub(self, temp_dir):
        """Test image file stub creation."""
        minimizer = BinaryMinimizer()
        
        img_file = temp_dir / "image.jpg"
        img_file.write_bytes(b'\xff\xd8\xff' + b'\x00' * 100)  # JPEG header
        
        result = minimizer.minimize(img_file)
        
        # Should identify as image
        assert "Image" in result or "JPEG" in result
    
    def test_preserve_binary_file(self, temp_dir):
        """Test binary file preservation."""
        minimizer = BinaryMinimizer()
        
        bin_file = temp_dir / "preserve.bin"
        bin_file.write_bytes(b'DATA')
        
        result = minimizer.minimize(bin_file, preserve=True)
        
        assert result is None  # Should return None when preserving