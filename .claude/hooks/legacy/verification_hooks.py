#!/usr/bin/env python
"""
Verification Hooks for Python Operations
MANDATORY: Use these hooks for ALL Python commands, tasks, and operations
"""

import os
import sys
import time
import subprocess
import csv
import json
from pathlib import Path
from typing import List, Dict, Optional, Tuple, Any
from datetime import datetime, timedelta


class Colors:
    """Terminal color codes"""
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    NC = '\033[0m'  # No Color


class VerificationHooks:
    """Core verification hooks for all repository operations"""
    
    def __init__(self, verbose: bool = True):
        self.verbose = verbose
        self.checks_passed = 0
        self.checks_failed = 0
        self.start_time = time.time()
        
    def log(self, level: str, message: str):
        """Log with color coding"""
        if not self.verbose:
            return
            
        colors = {
            'info': Colors.BLUE,
            'success': Colors.GREEN,
            'warning': Colors.YELLOW,
            'error': Colors.RED
        }
        
        symbols = {
            'info': '[CHECK]',
            'success': '[✓ PASS]',
            'warning': '[⚠ WARN]',
            'error': '[✗ FAIL]'
        }
        
        color = colors.get(level, Colors.NC)
        symbol = symbols.get(level, '[INFO]')
        print(f"{color}{symbol}{Colors.NC} {message}")
    
    def verify_return_code(self, cmd: str, description: str = "Command execution") -> bool:
        """Check command return code"""
        self.log('info', f"{description}: {cmd}")
        
        try:
            result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
            if result.returncode == 0:
                self.log('success', f"{description} (Return code: {result.returncode})")
                self.checks_passed += 1
                return True
            else:
                self.log('error', f"{description} (Return code: {result.returncode})")
                if result.stderr:
                    self.log('error', f"Error: {result.stderr[:200]}")
                self.checks_failed += 1
                return False
        except Exception as e:
            self.log('error', f"{description} - Exception: {str(e)}")
            self.checks_failed += 1
            return False
    
    def verify_file_exists(self, file_path: str, description: str = "File verification") -> bool:
        """Verify file exists"""
        self.log('info', f"{description}: {file_path}")
        
        if Path(file_path).exists() and Path(file_path).is_file():
            size = Path(file_path).stat().st_size
            size_str = self._format_size(size)
            self.log('success', f"{file_path} (Size: {size_str})")
            self.checks_passed += 1
            return True
        else:
            self.log('error', f"File missing: {file_path}")
            self.checks_failed += 1
            return False
    
    def verify_dir_exists(self, dir_path: str, description: str = "Directory verification") -> bool:
        """Verify directory exists"""
        self.log('info', f"{description}: {dir_path}")
        
        if Path(dir_path).exists() and Path(dir_path).is_dir():
            file_count = len(list(Path(dir_path).iterdir()))
            self.log('success', f"{dir_path} (Files: {file_count})")
            self.checks_passed += 1
            return True
        else:
            self.log('error', f"Directory missing: {dir_path}")
            self.checks_failed += 1
            return False
    
    def verify_file_created(self, file_path: str, minutes: int = 5, 
                           description: str = "File creation verification") -> bool:
        """Verify file was created/modified recently"""
        self.log('info', f"{description}: {file_path}")
        
        if not Path(file_path).exists():
            self.log('error', f"File not created: {file_path}")
            self.checks_failed += 1
            return False
        
        mod_time = datetime.fromtimestamp(Path(file_path).stat().st_mtime)
        time_diff = datetime.now() - mod_time
        
        if time_diff <= timedelta(minutes=minutes):
            self.log('success', f"{file_path} (Modified {time_diff.seconds//60} minutes ago)")
            self.checks_passed += 1
            return True
        else:
            self.log('warning', f"{file_path} (Not recently modified)")
            self.checks_failed += 1
            return False
    
    def verify_output_contains(self, cmd: str, expected: str, 
                              description: str = "Output verification") -> bool:
        """Verify process output contains expected text"""
        self.log('info', f"{description}")
        self.log('info', f"Command: {cmd}")
        self.log('info', f"Expecting: '{expected}'")
        
        try:
            result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
            output = result.stdout + result.stderr
            
            if expected in output:
                self.log('success', f"Found expected text in output")
                self.checks_passed += 1
                return True
            else:
                self.log('error', f"Expected text not found")
                self.log('warning', f"Output: {output[:200]}...")
                self.checks_failed += 1
                return False
        except Exception as e:
            self.log('error', f"Exception: {str(e)}")
            self.checks_failed += 1
            return False
    
    def verify_python_module(self, module: str, venv_path: str = ".venv") -> bool:
        """Verify Python module import"""
        self.log('info', f"Python module: {module}")
        
        try:
            # Try to import the module
            __import__(module)
            self.log('success', f"Module available: {module}")
            self.checks_passed += 1
            return True
        except ImportError:
            self.log('error', f"Module missing: {module}")
            self.checks_failed += 1
            return False
    
    def verify_csv_structure(self, file_path: str, expected_columns: List[str], 
                            min_rows: int = 1) -> Tuple[bool, Dict]:
        """Verify CSV file structure"""
        self.log('info', f"CSV structure: {file_path}")
        
        if not Path(file_path).exists():
            self.log('error', f"CSV file not found")
            self.checks_failed += 1
            return False, {}
        
        try:
            with open(file_path, 'r') as f:
                reader = csv.DictReader(f)
                headers = reader.fieldnames
                rows = list(reader)
                
            self.log('info', f"Headers: {headers}")
            self.log('info', f"Rows: {len(rows)}")
            
            # Check columns
            missing_cols = set(expected_columns) - set(headers)
            if missing_cols:
                self.log('error', f"Missing columns: {missing_cols}")
                self.checks_failed += 1
                return False, {'headers': headers, 'row_count': len(rows)}
            
            # Check row count
            if len(rows) < min_rows:
                self.log('error', f"Only {len(rows)} rows (minimum: {min_rows})")
                self.checks_failed += 1
                return False, {'headers': headers, 'row_count': len(rows)}
            
            self.log('success', f"CSV structure valid")
            self.checks_passed += 1
            return True, {'headers': headers, 'row_count': len(rows), 'data': rows}
            
        except Exception as e:
            self.log('error', f"Error reading CSV: {str(e)}")
            self.checks_failed += 1
            return False, {}
    
    def verify_json_structure(self, file_path: str, required_keys: List[str]) -> Tuple[bool, Dict]:
        """Verify JSON file structure"""
        self.log('info', f"JSON structure: {file_path}")
        
        if not Path(file_path).exists():
            self.log('error', f"JSON file not found")
            self.checks_failed += 1
            return False, {}
        
        try:
            with open(file_path, 'r') as f:
                data = json.load(f)
            
            missing_keys = set(required_keys) - set(data.keys())
            if missing_keys:
                self.log('error', f"Missing keys: {missing_keys}")
                self.checks_failed += 1
                return False, data
            
            self.log('success', f"JSON structure valid")
            self.checks_passed += 1
            return True, data
            
        except Exception as e:
            self.log('error', f"Error reading JSON: {str(e)}")
            self.checks_failed += 1
            return False, {}
    
    def verify_log_no_errors(self, log_file: str, ignore_patterns: List[str] = None) -> bool:
        """Verify log contains no errors"""
        self.log('info', f"Log file check: {log_file}")
        
        if not Path(log_file).exists():
            self.log('warning', f"Log file not found")
            return True  # No log means no errors
        
        error_keywords = ['error', 'exception', 'failed', 'fatal']
        ignore_patterns = ignore_patterns or []
        
        try:
            with open(log_file, 'r') as f:
                lines = f.readlines()
            
            errors = []
            for line in lines:
                line_lower = line.lower()
                if any(keyword in line_lower for keyword in error_keywords):
                    if not any(pattern in line for pattern in ignore_patterns):
                        errors.append(line.strip())
            
            if errors:
                self.log('error', f"Found {len(errors)} error messages")
                for error in errors[:5]:  # Show first 5 errors
                    self.log('error', f"  {error}")
                self.checks_failed += 1
                return False
            else:
                self.log('success', f"No errors in log")
                self.checks_passed += 1
                return True
                
        except Exception as e:
            self.log('error', f"Error reading log: {str(e)}")
            self.checks_failed += 1
            return False
    
    def pre_execution_check(self, working_dir: str, required_files: List[str]) -> bool:
        """Pre-execution verification"""
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        print(f"{Colors.YELLOW}PRE-EXECUTION VERIFICATION{Colors.NC}")
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        
        all_good = True
        
        # Check working directory
        all_good &= self.verify_dir_exists(working_dir, "Working directory")
        
        # Check required files
        for file in required_files:
            file_path = Path(working_dir) / file
            all_good &= self.verify_file_exists(str(file_path), f"Required file")
        
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        return all_good
    
    def post_execution_check(self, return_code: int, output_files: List[str]) -> bool:
        """Post-execution verification"""
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        print(f"{Colors.YELLOW}POST-EXECUTION VERIFICATION{Colors.NC}")
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        
        all_good = True
        
        # Check return code
        if return_code == 0:
            self.log('success', f"Command completed successfully (Return code: {return_code})")
            self.checks_passed += 1
        else:
            self.log('error', f"Command failed (Return code: {return_code})")
            self.checks_failed += 1
            all_good = False
        
        # Check output files
        for file in output_files:
            all_good &= self.verify_file_created(file, 10, "Output file")
        
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        return all_good
    
    def verification_summary(self) -> bool:
        """Generate verification summary"""
        duration = int(time.time() - self.start_time)
        
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        print(f"{Colors.YELLOW}VERIFICATION SUMMARY{Colors.NC}")
        print(f"{Colors.YELLOW}{'='*60}{Colors.NC}")
        print(f"{Colors.BLUE}Duration:{Colors.NC} {duration}s")
        print(f"{Colors.GREEN}Passed:{Colors.NC} {self.checks_passed}")
        print(f"{Colors.RED}Failed:{Colors.NC} {self.checks_failed}")
        
        if self.checks_failed == 0:
            print(f"{Colors.GREEN}╔{'═'*35}╗{Colors.NC}")
            print(f"{Colors.GREEN}║     ALL CHECKS PASSED ✓          ║{Colors.NC}")
            print(f"{Colors.GREEN}╚{'═'*35}╝{Colors.NC}")
            return True
        else:
            print(f"{Colors.RED}╔{'═'*35}╗{Colors.NC}")
            print(f"{Colors.RED}║     VERIFICATION FAILED ✗        ║{Colors.NC}")
            print(f"{Colors.RED}╚{'═'*35}╝{Colors.NC}")
            return False
    
    def _format_size(self, size: int) -> str:
        """Format file size in human readable format"""
        for unit in ['B', 'KB', 'MB', 'GB']:
            if size < 1024.0:
                return f"{size:.1f}{unit}"
            size /= 1024.0
        return f"{size:.1f}TB"


class OrcaFlexVerification(VerificationHooks):
    """OrcaFlex specific verification hooks"""
    
    def verify_orcaflex_license(self) -> bool:
        """Verify OrcaFlex license availability"""
        self.log('info', "OrcaFlex license availability")
        
        try:
            from digitalmodel.orcaflex.orcaflex_utilities import is_orcaflex_available
            if is_orcaflex_available():
                self.log('success', "OrcaFlex license available")
                self.checks_passed += 1
                return True
            else:
                self.log('warning', "OrcaFlex license unavailable (will use mock mode)")
                return False
        except ImportError:
            self.log('error', "Cannot import OrcaFlex utilities")
            self.checks_failed += 1
            return False
    
    def verify_orcaflex_output(self, output_dir: str, pattern: str = "*.sim") -> bool:
        """Verify OrcaFlex simulation outputs"""
        self.log('info', f"OrcaFlex outputs in {output_dir}")
        
        output_path = Path(output_dir)
        if not output_path.exists():
            self.log('error', f"Output directory not found")
            self.checks_failed += 1
            return False
        
        # Find recent simulation files
        recent_files = []
        cutoff_time = datetime.now() - timedelta(minutes=60)
        
        for file in output_path.glob(pattern):
            if datetime.fromtimestamp(file.stat().st_mtime) > cutoff_time:
                recent_files.append(file)
        
        if recent_files:
            self.log('success', f"Found {len(recent_files)} simulation files")
            for file in recent_files[:5]:  # Show first 5
                size = self._format_size(file.stat().st_size)
                self.log('info', f"  {file.name} ({size})")
            self.checks_passed += 1
            return True
        else:
            self.log('error', f"No simulation files found")
            self.checks_failed += 1
            return False


def main():
    """Example usage"""
    hooks = VerificationHooks()
    
    # Example pre-execution check
    hooks.pre_execution_check(
        working_dir=".",
        required_files=["config.yml", "data.csv"]
    )
    
    # Example command execution
    hooks.verify_return_code("echo 'Hello World'", "Test command")
    
    # Example post-execution check
    hooks.post_execution_check(
        return_code=0,
        output_files=["output.txt", "results.csv"]
    )
    
    # Summary
    hooks.verification_summary()


if __name__ == "__main__":
    main()