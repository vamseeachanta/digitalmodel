#!/usr/bin/env python
"""
Run OrcaFlex batch processing in background with progress monitoring.
This script launches the batch runner in a background thread and provides real-time progress updates.
"""

import sys
import time
import threading
import subprocess
from pathlib import Path
from datetime import datetime
import argparse
import yaml

class BackgroundBatchRunner:
    """Run batch processing in background with monitoring."""
    
    def __init__(self, config_file, log_level='INFO'):
        self.config_file = Path(config_file)
        self.log_level = log_level
        self.process = None
        self.thread = None
        self.is_running = False
        self.start_time = None
        self.models_processed = 0
        self.total_models = 0
        self.log_file = None
        
        # Load config to get total model count
        self.load_config()
        
    def load_config(self):
        """Load configuration to get model count."""
        with open(self.config_file, 'r') as f:
            config = yaml.safe_load(f)
        self.total_models = len(config.get('models', []))
        base_dir = Path(config['batch_info']['base_directory'])
        output_dir = base_dir / config['batch_info']['output_directory'].replace('./', '')
        
        # Create unique log file name
        timestamp = datetime.now().strftime('%Y%m%d_%H%M%S')
        self.log_file = output_dir / f"background_batch_{timestamp}.log"
        
        # Ensure output directory exists
        output_dir.mkdir(parents=True, exist_ok=True)
        
    def run_batch_process(self):
        """Run the batch processing in a subprocess."""
        cmd = [
            sys.executable,
            'src/modules/orcaflex/mooring_tension_iteration/batch_processing/orcaflex_batch_runner.py',
            '--config', str(self.config_file),
            '--log-level', self.log_level
        ]
        
        print(f"Starting batch processing with command:")
        print(f"  {' '.join(cmd)}")
        print(f"Log file: {self.log_file}")
        print("-" * 80)
        
        with open(self.log_file, 'w') as log:
            self.process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT,
                universal_newlines=True,
                bufsize=1
            )
            
            # Process output line by line
            for line in self.process.stdout:
                # Write to log file
                log.write(line)
                log.flush()
                
                # Parse progress
                if "Processing:" in line or "[OK] SUCCESS" in line or "[FAILED]" in line:
                    self.models_processed += 1
                    self.print_progress(line.strip())
                elif "BATCH PROCESSING COMPLETE" in line:
                    self.is_running = False
                    print("\n" + "=" * 80)
                    print(line.strip())
                elif "Total Time:" in line or "Success Rate:" in line:
                    print(line.strip())
            
            self.process.wait()
            
    def print_progress(self, message=""):
        """Print progress update."""
        elapsed = time.time() - self.start_time
        progress_pct = (self.models_processed / self.total_models * 100) if self.total_models > 0 else 0
        
        # Estimate time remaining
        if self.models_processed > 0:
            avg_time_per_model = elapsed / self.models_processed
            remaining_models = self.total_models - self.models_processed
            est_remaining = avg_time_per_model * remaining_models
            est_str = f" | ETA: {int(est_remaining//60)}m {int(est_remaining%60)}s"
        else:
            est_str = ""
        
        # Print progress bar
        bar_length = 50
        filled = int(bar_length * self.models_processed / self.total_models)
        bar = '█' * filled + '░' * (bar_length - filled)
        
        print(f"\r[{bar}] {progress_pct:.1f}% ({self.models_processed}/{self.total_models}) | "
              f"Elapsed: {int(elapsed//60)}m {int(elapsed%60)}s{est_str}", end='')
        
        if message:
            print(f"\n{message}")
            
    def start(self):
        """Start batch processing in background."""
        self.is_running = True
        self.start_time = time.time()
        self.models_processed = 0
        
        print(f"{'='*80}")
        print(f"BACKGROUND BATCH PROCESSING STARTED")
        print(f"{'='*80}")
        print(f"Configuration: {self.config_file}")
        print(f"Total models: {self.total_models}")
        print(f"Max workers: 30")
        print(f"Start time: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"{'='*80}\n")
        
        # Start batch process in thread
        self.thread = threading.Thread(target=self.run_batch_process)
        self.thread.daemon = True
        self.thread.start()
        
    def wait(self):
        """Wait for batch processing to complete."""
        try:
            while self.is_running and self.thread.is_alive():
                time.sleep(1)
                
            self.thread.join()
            
            elapsed = time.time() - self.start_time
            print(f"\n{'='*80}")
            print(f"BATCH PROCESSING COMPLETED")
            print(f"Total time: {int(elapsed//60)}m {int(elapsed%60)}s")
            print(f"Log file: {self.log_file}")
            print(f"{'='*80}")
            
        except KeyboardInterrupt:
            print("\n\nInterrupted by user. Terminating batch process...")
            if self.process:
                self.process.terminate()
                self.process.wait()
            print("Batch processing terminated.")
            
    def run_async(self):
        """Run in background without waiting (returns immediately)."""
        self.start()
        # Wait a moment for process to start
        time.sleep(0.5)
        if self.process and self.process.poll() is None:
            pid = self.process.pid
            print(f"\nBatch processing running in background (PID: {pid})")
            print(f"Monitor progress in log file: {self.log_file}")
            print(f"Or run: tail -f {self.log_file}")
            return pid
        else:
            print("\nFailed to start batch process in background")
            return None

def main():
    parser = argparse.ArgumentParser(
        description='Run OrcaFlex batch processing in background'
    )
    
    parser.add_argument(
        '--config',
        default='batch_all_fsts_vessel_statics_6dof.yml',
        help='Batch configuration file'
    )
    
    parser.add_argument(
        '--log-level',
        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR'],
        default='INFO',
        help='Logging level'
    )
    
    parser.add_argument(
        '--async-mode',
        action='store_true',
        help='Run asynchronously (return immediately without waiting)'
    )
    
    args = parser.parse_args()
    
    # Check if config file exists
    if not Path(args.config).exists():
        print(f"Error: Configuration file not found: {args.config}")
        sys.exit(1)
    
    # Create and start runner
    runner = BackgroundBatchRunner(args.config, args.log_level)
    
    if args.async_mode:
        # Run async and return immediately
        pid = runner.run_async()
        print(f"\nTo check if still running: tasklist /FI \"PID eq {pid}\"")
        print(f"To stop: taskkill /PID {pid}")
    else:
        # Run and wait for completion
        runner.start()
        runner.wait()

if __name__ == "__main__":
    main()