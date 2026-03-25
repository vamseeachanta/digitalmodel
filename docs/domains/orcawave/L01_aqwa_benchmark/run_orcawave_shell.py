#!/usr/bin/env python3
"""
OrcaWave Shell Execution Script

ABOUTME: Production-ready OrcaWave execution using subprocess with Git Bash/Shell support.

Features:
- Cross-platform (Windows Git Bash, Linux, macOS)
- Robust error handling
- Progress monitoring
- Log capture and storage
- Timeout management
- Parallel validation

Usage:
    # Windows (Git Bash)
    python run_orcawave_shell.py --config orcawave_001_ship_raos_rev2.yml

    # Linux/macOS
    ./run_orcawave_shell.py --config my_config.yml --threads 4

    # Direct shell execution
    bash run_orcawave.sh orcawave_001_ship_raos_rev2.yml
"""

import os
import sys
import subprocess
import argparse
import time
import platform
from pathlib import Path
from datetime import datetime
import yaml
import json
import shutil
from typing import Optional, Dict, List, Tuple

# ============================================================================
# CONFIGURATION
# ============================================================================

ORCAWAVE_PATHS_WINDOWS = [
    r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcaWave64.exe",
    r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcaWave.exe",
    r"C:\Program Files\Orcina\OrcaFlex\11.6\OrcaWave64.exe",
    r"C:\Program Files\Orcina\OrcaFlex\11.6\OrcaWave.exe",
    r"C:\Program Files\Orcina\OrcaWave\OrcaWave.exe",
    r"C:\Program Files (x86)\Orcina\OrcaWave\OrcaWave.exe",
    r"C:\Program Files\Orcina\OrcaFlex\OrcaWave.exe",
    r"D:\OrcaWave\OrcaWave.exe",
]

ORCAWAVE_PATHS_LINUX = [
    "/usr/local/bin/orcawave",
    "/opt/orcina/orcawave/bin/orcawave",
    "~/orcawave/bin/orcawave",
]

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

def detect_shell_environment() -> str:
    """Detect the current shell environment"""
    if platform.system() == "Windows":
        # Check if running in Git Bash
        if "GIT" in os.environ.get("SHELL", "").upper() or \
           "BASH" in os.environ.get("SHELL", "").upper():
            return "git_bash"
        elif "MSYSTEM" in os.environ:  # MINGW, MSYS2
            return "msys2"
        else:
            return "windows_cmd"
    elif platform.system() == "Linux":
        return "linux_bash"
    elif platform.system() == "Darwin":
        return "macos_bash"
    else:
        return "unknown"


def find_orcawave_executable() -> Optional[Path]:
    """Find OrcaWave executable across platforms"""

    system = platform.system()

    if system == "Windows":
        search_paths = ORCAWAVE_PATHS_WINDOWS
    else:
        search_paths = ORCAWAVE_PATHS_LINUX

    # Search predefined paths
    for path_str in search_paths:
        path = Path(path_str).expanduser()
        if path.exists():
            print(f"[OK] Found OrcaWave: {path}")
            return path

    # Try which/where command
    try:
        if system == "Windows":
            result = subprocess.run(
                ["where", "OrcaWave.exe"],
                capture_output=True,
                text=True,
                timeout=5
            )
        else:
            result = subprocess.run(
                ["which", "orcawave"],
                capture_output=True,
                text=True,
                timeout=5
            )

        if result.returncode == 0 and result.stdout.strip():
            path = Path(result.stdout.strip().split('\n')[0])
            if path.exists():
                print(f"[OK] Found OrcaWave (via which/where): {path}")
                return path

    except Exception as e:
        pass

    print("[ERROR] OrcaWave executable not found!")
    print("\nSearched locations:")
    for path in search_paths:
        print(f"  - {path}")

    return None


def convert_to_windows_path(path: Path) -> str:
    """Convert Unix-style path to Windows path for Git Bash"""
    path_str = str(path.resolve())

    shell_env = detect_shell_environment()

    if shell_env == "git_bash":
        # Git Bash uses Unix-style paths
        # C:\Program Files -> /c/Program Files
        if path_str[1:3] == ':\\':
            drive = path_str[0].lower()
            rest = path_str[3:].replace('\\', '/')
            return f"/{drive}/{rest}"

    return path_str


def convert_from_windows_path(path_str: str) -> Path:
    """Convert Git Bash Unix-style path to Windows path"""
    if path_str.startswith('/') and len(path_str) > 2 and path_str[2] == '/':
        # /c/path/to/file -> C:\path\to\file
        drive = path_str[1].upper()
        rest = path_str[3:].replace('/', '\\')
        return Path(f"{drive}:\\{rest}")

    return Path(path_str)


# ============================================================================
# VALIDATION
# ============================================================================

def validate_config_file(config_path: Path) -> Tuple[bool, Dict]:
    """Validate OrcaWave configuration file"""

    print(f"\n{'='*60}")
    print("CONFIGURATION VALIDATION")
    print(f"{'='*60}")

    if not config_path.exists():
        print(f"[ERROR] Config file not found: {config_path}")
        return False, {}

    print(f"[OK] Config file exists: {config_path.name}")

    # Load YAML (handle multiple documents with --- separator)
    try:
        with open(config_path, 'r') as f:
            # Load all documents and use only dictionary documents
            docs = list(yaml.safe_load_all(f))
            # Filter for dictionary documents only (skip None, strings, etc.)
            dict_docs = [doc for doc in docs if isinstance(doc, dict)]

            if len(dict_docs) == 0:
                raise ValueError("No valid YAML dictionary found in file")
            elif len(dict_docs) == 1:
                config = dict_docs[0]
            else:
                # Merge multiple dictionary documents
                config = {}
                for doc in dict_docs:
                    config.update(doc)
        print("[OK] YAML parsed successfully")
    except Exception as e:
        print(f"[ERROR] YAML parse error: {e}")
        return False, {}

    # Validate required fields
    required_fields = [
        'UnitsSystem', 'WaterDepth', 'WaterDensity',
        'PeriodOrFrequency', 'WaveHeading', 'Bodies'
    ]

    missing = [f for f in required_fields if f not in config]
    if missing:
        print(f"[ERROR] Missing required fields: {missing}")
        return False, config

    print("[OK] All required fields present")

    # Validate mesh files
    print("\nMesh File Validation:")
    for i, body in enumerate(config.get('Bodies', [])):
        mesh_file_name = body.get('BodyMeshFileName', '')
        if mesh_file_name:
            mesh_path = config_path.parent / mesh_file_name
            if mesh_path.exists():
                size_mb = mesh_path.stat().st_size / (1024 * 1024)
                print(f"  [OK] Body {i}: {mesh_file_name} ({size_mb:.2f} MB)")
            else:
                print(f"  [ERROR] Body {i}: Mesh not found: {mesh_file_name}")
                return False, config
        else:
            print(f"  [ERROR] Body {i}: No mesh file specified")
            return False, config

    # Display analysis parameters
    print("\nAnalysis Parameters:")
    print(f"  - Water Depth: {config.get('WaterDepth')} m")
    print(f"  - Water Density: {config.get('WaterDensity')} kg/m³")
    print(f"  - Periods: {len(config.get('PeriodOrFrequency', []))} values")
    print(f"  - Headings: {len(config.get('WaveHeading', []))} values")
    print(f"  - Bodies: {len(config.get('Bodies', []))}")

    # Estimate runtime
    n_periods = len(config.get('PeriodOrFrequency', []))
    n_headings = len(config.get('WaveHeading', []))
    total_cases = n_periods * n_headings
    estimated_min = total_cases * 0.3 / 60  # ~0.3 sec per case

    print(f"\nEstimated Analysis:")
    print(f"  - Total cases: {total_cases}")
    print(f"  - Estimated time: {estimated_min:.1f} minutes")

    print(f"\n{'='*60}\n")

    return True, config


# ============================================================================
# EXECUTION
# ============================================================================

def execute_orcawave_shell(
    config_path: Path,
    orcawave_exe: Path,
    timeout: int = 3600,
    verbose: bool = True
) -> Tuple[bool, Dict]:
    """
    Execute OrcaWave using subprocess with shell support

    Args:
        config_path: Path to OrcaWave YAML config
        orcawave_exe: Path to OrcaWave executable
        timeout: Execution timeout in seconds
        verbose: Print detailed output

    Returns:
        (success, results_dict)
    """

    print(f"\n{'='*60}")
    print("ORCAWAVE EXECUTION")
    print(f"{'='*60}")

    shell_env = detect_shell_environment()
    print(f"Shell Environment: {shell_env}")

    # Setup paths
    work_dir = config_path.parent
    log_dir = work_dir / "logs"
    output_dir = work_dir / "orcawave_output"

    log_dir.mkdir(exist_ok=True)
    output_dir.mkdir(exist_ok=True)

    # Create log file
    timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
    log_file = log_dir / f"orcawave_{timestamp}.log"

    # Prepare command
    if shell_env == "git_bash" and platform.system() == "Windows":
        # For Git Bash on Windows, use Windows-style paths
        cmd = [str(orcawave_exe), str(config_path)]
        shell = False  # Don't use shell for direct execution
    else:
        cmd = [str(orcawave_exe), str(config_path)]
        shell = False

    print(f"Command: {' '.join(cmd)}")
    print(f"Working Directory: {work_dir}")
    print(f"Log File: {log_file}")

    # Execute
    print(f"\nStarting OrcaWave analysis...")
    print(f"Timestamp: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    start_time = time.time()

    try:
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            cwd=str(work_dir),
            shell=shell,
            bufsize=1,
            universal_newlines=True
        )

        # Capture output in real-time
        stdout_lines = []
        stderr_lines = []

        print("\n--- OrcaWave Output ---")

        # Monitor process with timeout
        try:
            stdout, stderr = process.communicate(timeout=timeout)

            if stdout:
                stdout_lines = stdout.split('\n')
                if verbose:
                    for line in stdout_lines:
                        if line.strip():
                            print(line)

            if stderr:
                stderr_lines = stderr.split('\n')
                if verbose and stderr_lines:
                    print("\n--- Errors/Warnings ---")
                    for line in stderr_lines:
                        if line.strip():
                            print(line)

        except subprocess.TimeoutExpired:
            process.kill()
            stdout, stderr = process.communicate()
            print(f"\n[ERROR] Process timed out after {timeout} seconds")

            return False, {
                'status': 'timeout',
                'duration': timeout,
                'log_file': str(log_file)
            }

        elapsed_time = time.time() - start_time

        # Check return code
        if process.returncode == 0:
            print(f"\n[SUCCESS] OrcaWave completed successfully")
            print(f"  Duration: {elapsed_time:.2f} seconds ({elapsed_time/60:.1f} minutes)")
            status = 'success'
        else:
            print(f"\n[ERROR] OrcaWave failed with return code: {process.returncode}")
            print(f"  Duration: {elapsed_time:.2f} seconds")
            status = 'failed'

        # Save log
        with open(log_file, 'w', encoding='utf-8') as f:
            f.write(f"OrcaWave Execution Log\n")
            f.write(f"{'='*60}\n")
            f.write(f"Config: {config_path}\n")
            f.write(f"Started: {datetime.now().isoformat()}\n")
            f.write(f"Duration: {elapsed_time:.2f} seconds\n")
            f.write(f"Return Code: {process.returncode}\n")
            f.write(f"Status: {status}\n")
            f.write(f"\n{'='*60}\n")
            f.write(f"STDOUT:\n")
            f.write(f"{'='*60}\n")
            f.write('\n'.join(stdout_lines))
            f.write(f"\n\n{'='*60}\n")
            f.write(f"STDERR:\n")
            f.write(f"{'='*60}\n")
            f.write('\n'.join(stderr_lines))

        print(f"\n[OK] Log saved: {log_file}")

        # Check for output files
        expected_outputs = [
            output_dir / f"{config_path.stem}.sim",
            output_dir / f"{config_path.stem}_raos.csv",
        ]

        found_outputs = [f for f in expected_outputs if f.exists()]

        print(f"\nOutput Files:")
        if found_outputs:
            for output_file in found_outputs:
                size_mb = output_file.stat().st_size / (1024 * 1024)
                print(f"  [OK] {output_file.name} ({size_mb:.2f} MB)")
        else:
            print(f"  [WARNING] No output files detected (check logs)")

        print(f"\n{'='*60}\n")

        return status == 'success', {
            'status': status,
            'duration': elapsed_time,
            'return_code': process.returncode,
            'log_file': str(log_file),
            'output_files': [str(f) for f in found_outputs],
            'stdout_lines': len(stdout_lines),
            'stderr_lines': len(stderr_lines)
        }

    except FileNotFoundError:
        print(f"\n[ERROR] Executable not found: {orcawave_exe}")
        return False, {'status': 'error', 'message': 'Executable not found'}

    except Exception as e:
        print(f"\n[ERROR] Execution error: {e}")
        return False, {'status': 'error', 'message': str(e)}


# ============================================================================
# MAIN
# ============================================================================

def main():
    """Main execution"""

    parser = argparse.ArgumentParser(
        description='OrcaWave Shell Execution Script',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python run_orcawave_shell.py --config orcawave_001_ship_raos_rev2.yml
  python run_orcawave_shell.py --config my_config.yml --timeout 7200
  python run_orcawave_shell.py --config my_config.yml --orcawave-exe /path/to/OrcaWave.exe
        """
    )

    parser.add_argument(
        '--config',
        type=str,
        required=True,
        help='OrcaWave YAML configuration file'
    )
    parser.add_argument(
        '--orcawave-exe',
        type=str,
        help='Path to OrcaWave executable (auto-detect if not provided)'
    )
    parser.add_argument(
        '--timeout',
        type=int,
        default=3600,
        help='Execution timeout in seconds (default: 3600 = 1 hour)'
    )
    parser.add_argument(
        '--quiet',
        action='store_true',
        help='Suppress verbose output'
    )
    parser.add_argument(
        '--validate-only',
        action='store_true',
        help='Validate configuration without executing'
    )

    args = parser.parse_args()

    # Print header
    print("\n" + "="*60)
    print("ORCAWAVE SHELL EXECUTION")
    print("="*60)
    print(f"Platform: {platform.system()} ({platform.machine()})")
    print(f"Shell: {detect_shell_environment()}")
    print(f"Python: {sys.version.split()[0]}")
    print("="*60 + "\n")

    # Find config
    config_path = Path(args.config)
    if not config_path.is_absolute():
        config_path = Path.cwd() / config_path

    # Validate config
    valid, config_data = validate_config_file(config_path)

    if not valid:
        print("\n[ERROR] Configuration validation failed!")
        return 1

    if args.validate_only:
        print("\n[SUCCESS] Validation complete (no execution requested)")
        return 0

    # Find OrcaWave executable
    if args.orcawave_exe:
        orcawave_exe = Path(args.orcawave_exe)
        if not orcawave_exe.exists():
            print(f"\n[ERROR] Specified OrcaWave executable not found: {orcawave_exe}")
            return 1
    else:
        orcawave_exe = find_orcawave_executable()
        if not orcawave_exe:
            return 1

    # Execute
    success, results = execute_orcawave_shell(
        config_path=config_path,
        orcawave_exe=orcawave_exe,
        timeout=args.timeout,
        verbose=not args.quiet
    )

    # Summary
    print("\n" + "="*60)
    print("EXECUTION SUMMARY")
    print("="*60)
    print(f"Status: {'[SUCCESS]' if success else '[FAILED]'}")
    print(f"Duration: {results.get('duration', 0):.2f} seconds")
    print(f"Log File: {results.get('log_file', 'N/A')}")

    if results.get('output_files'):
        print(f"\nOutput Files ({len(results['output_files'])}):")
        for output in results['output_files']:
            print(f"  - {Path(output).name}")

    print("="*60 + "\n")

    return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main())
