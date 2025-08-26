"""
Master orchestration script for OrcaWave diffraction analysis input preparation.
"""

import argparse
import logging
from pathlib import Path
import subprocess
import sys
import yaml
from typing import Dict, Any, Optional

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class OrcaWaveWorkflowOrchestrator:
    """Orchestrate the complete OrcaWave input preparation workflow."""
    
    def __init__(self, config_path: Optional[Path] = None):
        """Initialize orchestrator with optional configuration."""
        self.config = {}
        if config_path:
            self.load_config(config_path)
        
        self.scripts_dir = Path(__file__).parent
        self.results = {}
        
    def load_config(self, config_path: Path):
        """Load workflow configuration from YAML file."""
        try:
            with open(config_path, 'r') as f:
                self.config = yaml.safe_load(f)
            logger.info(f"Loaded configuration: {config_path}")
        except Exception as e:
            logger.error(f"Error loading config: {e}")
            
    def extract_excel_data(self, excel_file: Path, sheet: Optional[str] = None) -> bool:
        """Extract vessel data from Excel file."""
        logger.info("=" * 60)
        logger.info("STEP 1: Extract Excel Data")
        logger.info("=" * 60)
        
        try:
            script = self.scripts_dir / 'extract_excel_data.py'
            cmd = [sys.executable, str(script), str(excel_file)]
            
            if sheet:
                cmd.extend(['--sheet', sheet])
                
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode == 0:
                logger.info("✅ Excel data extraction successful")
                self.results['excel_extraction'] = True
                print(result.stdout)
                return True
            else:
                logger.error(f"❌ Excel extraction failed: {result.stderr}")
                self.results['excel_extraction'] = False
                return False
                
        except Exception as e:
            logger.error(f"Excel extraction error: {e}")
            self.results['excel_extraction'] = False
            return False
            
    def convert_geometry(self, msh_file: Path, output_gdf: Path, scale: float = 1.0) -> bool:
        """Convert GMsh .msh file to OrcaWave GDF format."""
        logger.info("=" * 60)
        logger.info("STEP 2: Convert Geometry (MSH to GDF)")
        logger.info("=" * 60)
        
        try:
            script = self.scripts_dir / 'convert_msh_to_gdf.py'
            cmd = [
                sys.executable, str(script),
                str(msh_file),
                '--output', str(output_gdf),
                '--scale', str(scale),
                '--validate'
            ]
            
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode == 0:
                logger.info(f"✅ Geometry conversion successful: {output_gdf}")
                self.results['geometry_conversion'] = True
                print(result.stdout)
                return True
            else:
                logger.error(f"❌ Geometry conversion failed: {result.stderr}")
                self.results['geometry_conversion'] = False
                return False
                
        except Exception as e:
            logger.error(f"Geometry conversion error: {e}")
            self.results['geometry_conversion'] = False
            return False
            
    def generate_orcawave_input(
        self,
        template: Path,
        output: Path,
        gdf: Optional[Path] = None,
        excel: Optional[Path] = None,
        water_depth: Optional[float] = None
    ) -> bool:
        """Generate OrcaWave input YAML file."""
        logger.info("=" * 60)
        logger.info("STEP 3: Generate OrcaWave Input")
        logger.info("=" * 60)
        
        try:
            script = self.scripts_dir / 'generate_orcawave_input.py'
            cmd = [
                sys.executable, str(script),
                str(template),
                '--output', str(output),
                '--validate'
            ]
            
            if gdf:
                cmd.extend(['--gdf', str(gdf)])
            if excel:
                cmd.extend(['--excel', str(excel)])
            if water_depth:
                cmd.extend(['--water-depth', str(water_depth)])
                
            result = subprocess.run(cmd, capture_output=True, text=True)
            
            if result.returncode == 0:
                logger.info(f"✅ OrcaWave input generation successful: {output}")
                self.results['input_generation'] = True
                print(result.stdout)
                return True
            else:
                logger.error(f"❌ Input generation failed: {result.stderr}")
                self.results['input_generation'] = False
                return False
                
        except Exception as e:
            logger.error(f"Input generation error: {e}")
            self.results['input_generation'] = False
            return False
            
    def test_all_components(self) -> bool:
        """Run parallel tests on all components."""
        logger.info("=" * 60)
        logger.info("VALIDATION: Testing All Components")
        logger.info("=" * 60)
        
        try:
            script = self.scripts_dir / 'test_parallel.py'
            result = subprocess.run(
                [sys.executable, str(script)],
                capture_output=True,
                text=True
            )
            
            if result.returncode == 0:
                logger.info("✅ All components validated successfully")
                self.results['validation'] = True
                return True
            else:
                logger.error("❌ Component validation failed")
                print(result.stdout)
                print(result.stderr)
                self.results['validation'] = False
                return False
                
        except Exception as e:
            logger.error(f"Validation error: {e}")
            self.results['validation'] = False
            return False
            
    def run_complete_workflow(
        self,
        excel_file: Path,
        msh_file: Path,
        template_file: Path,
        output_dir: Path
    ) -> bool:
        """Run the complete workflow end-to-end."""
        logger.info("\n" + "=" * 60)
        logger.info("ORCAWAVE INPUT PREPARATION WORKFLOW")
        logger.info("=" * 60)
        
        # Create output directory
        output_dir = Path(output_dir)
        output_dir.mkdir(parents=True, exist_ok=True)
        
        # Define output files
        gdf_file = output_dir / "geometry.gdf"
        yaml_file = output_dir / "orcawave_input.yml"
        
        # Step 1: Extract Excel data
        if not self.extract_excel_data(excel_file):
            logger.error("Workflow failed at Excel extraction")
            return False
            
        # Step 2: Convert geometry
        if not self.convert_geometry(msh_file, gdf_file):
            logger.error("Workflow failed at geometry conversion")
            return False
            
        # Step 3: Generate OrcaWave input
        if not self.generate_orcawave_input(
            template_file,
            yaml_file,
            gdf=gdf_file,
            excel=excel_file,
            water_depth=self.config.get('water_depth', 30)
        ):
            logger.error("Workflow failed at input generation")
            return False
            
        # Summary
        logger.info("\n" + "=" * 60)
        logger.info("WORKFLOW COMPLETED SUCCESSFULLY")
        logger.info("=" * 60)
        logger.info(f"✅ Excel data extracted from: {excel_file}")
        logger.info(f"✅ Geometry converted to: {gdf_file}")
        logger.info(f"✅ OrcaWave input generated: {yaml_file}")
        logger.info("\nNext steps:")
        logger.info("1. Review generated files in: " + str(output_dir))
        logger.info("2. Run OrcaWave analysis:")
        logger.info(f"   run_orcawave.bat {yaml_file} /GUI")
        logger.info("3. Post-process results:")
        logger.info(f"   python postprocess_results.py {output_dir}/results")
        
        return True
        
    def generate_report(self) -> str:
        """Generate workflow execution report."""
        report = []
        report.append("\n" + "=" * 60)
        report.append("WORKFLOW EXECUTION REPORT")
        report.append("=" * 60)
        
        for step, success in self.results.items():
            status = "✅ SUCCESS" if success else "❌ FAILED"
            report.append(f"{step}: {status}")
            
        report.append("=" * 60)
        
        return "\n".join(report)


def main():
    """Main execution function."""
    parser = argparse.ArgumentParser(
        description='Orchestrate OrcaWave input preparation workflow'
    )
    
    # Workflow mode
    subparsers = parser.add_subparsers(dest='mode', help='Workflow mode')
    
    # Complete workflow
    complete = subparsers.add_parser('complete', help='Run complete workflow')
    complete.add_argument('--excel', required=True, help='Excel file with vessel data')
    complete.add_argument('--msh', required=True, help='GMsh .msh geometry file')
    complete.add_argument('--template', required=True, help='OrcaWave template YAML')
    complete.add_argument('--output-dir', required=True, help='Output directory')
    complete.add_argument('--config', help='Configuration YAML file')
    
    # Test mode
    test = subparsers.add_parser('test', help='Test all components')
    
    # Individual steps
    excel = subparsers.add_parser('excel', help='Extract Excel data only')
    excel.add_argument('file', help='Excel file path')
    excel.add_argument('--sheet', help='Sheet name')
    
    geometry = subparsers.add_parser('geometry', help='Convert geometry only')
    geometry.add_argument('msh', help='MSH file path')
    geometry.add_argument('--output', required=True, help='Output GDF file')
    geometry.add_argument('--scale', type=float, default=1.0, help='Scale factor')
    
    generate = subparsers.add_parser('generate', help='Generate OrcaWave input only')
    generate.add_argument('template', help='Template YAML file')
    generate.add_argument('--output', required=True, help='Output YAML file')
    generate.add_argument('--gdf', help='GDF file path')
    generate.add_argument('--excel', help='Excel file path')
    generate.add_argument('--water-depth', type=float, help='Water depth')
    
    args = parser.parse_args()
    
    if not args.mode:
        parser.print_help()
        sys.exit(1)
        
    # Create orchestrator
    config_path = Path(args.config) if hasattr(args, 'config') and args.config else None
    orchestrator = OrcaWaveWorkflowOrchestrator(config_path)
    
    # Execute based on mode
    if args.mode == 'complete':
        success = orchestrator.run_complete_workflow(
            Path(args.excel),
            Path(args.msh),
            Path(args.template),
            Path(args.output_dir)
        )
        print(orchestrator.generate_report())
        sys.exit(0 if success else 1)
        
    elif args.mode == 'test':
        success = orchestrator.test_all_components()
        sys.exit(0 if success else 1)
        
    elif args.mode == 'excel':
        success = orchestrator.extract_excel_data(Path(args.file), args.sheet)
        sys.exit(0 if success else 1)
        
    elif args.mode == 'geometry':
        success = orchestrator.convert_geometry(
            Path(args.msh),
            Path(args.output),
            args.scale
        )
        sys.exit(0 if success else 1)
        
    elif args.mode == 'generate':
        success = orchestrator.generate_orcawave_input(
            Path(args.template),
            Path(args.output),
            Path(args.gdf) if args.gdf else None,
            Path(args.excel) if args.excel else None,
            args.water_depth
        )
        sys.exit(0 if success else 1)
        

if __name__ == "__main__":
    main()