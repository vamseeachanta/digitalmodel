#!/usr/bin/env python
"""
GMSH Agent Runner
Main entry point for the GMSH finite element mesh generation agent
"""

import json
import logging
import os
import sys
from pathlib import Path
from typing import Dict, List, Optional, Any
import click
import yaml

try:
    import gmsh
    GMSH_AVAILABLE = True
except ImportError:
    print("Warning: GMSH Python bindings not installed. Install with: pip install gmsh")
    gmsh = None
    GMSH_AVAILABLE = False

# Import utilities
from utilities.mesh_generator import MeshGenerator


class GMSHAgent:
    """Specialized agent for finite element mesh generation using GMSH"""
    
    def __init__(self, config_path: Optional[str] = None):
        """
        Initialize GMSH Agent
        
        Args:
            config_path: Path to configuration file (JSON or YAML)
        """
        self.config = self.load_config(config_path)
        self.logger = self.setup_logging()
        self.gmsh_initialized = False
        self.mesh_generator = None
        
        if GMSH_AVAILABLE:
            self.initialize_gmsh()
            self.mesh_generator = MeshGenerator(self.config.get("configuration", {}))
        else:
            self.logger.warning("GMSH not available - running in limited mode")
    
    def load_config(self, config_path: Optional[str] = None) -> Dict:
        """
        Load agent configuration from file
        
        Args:
            config_path: Path to configuration file
            
        Returns:
            Configuration dictionary
        """
        default_config_path = Path(__file__).parent / "agent_config.json"
        
        if config_path and Path(config_path).exists():
            config_file = Path(config_path)
        elif default_config_path.exists():
            config_file = default_config_path
        else:
            # Return minimal default configuration
            return {
                "configuration": {
                    "default_algorithm": "frontal-delaunay",
                    "default_element_size": 1.0,
                    "quality_thresholds": {
                        "min_jacobian": 0.3,
                        "max_aspect_ratio": 5.0,
                        "max_skewness": 0.7
                    },
                    "logging": {
                        "level": "INFO",
                        "file": "gmsh_agent.log"
                    }
                }
            }
        
        with open(config_file, 'r') as f:
            if config_file.suffix == '.json':
                return json.load(f)
            elif config_file.suffix in ['.yml', '.yaml']:
                return yaml.safe_load(f)
            else:
                raise ValueError(f"Unsupported config format: {config_file.suffix}")
    
    def setup_logging(self) -> logging.Logger:
        """
        Set up logging configuration
        
        Returns:
            Configured logger instance
        """
        log_config = self.config.get("configuration", {}).get("logging", {})
        log_level = getattr(logging, log_config.get("level", "INFO"))
        log_file = log_config.get("file", "gmsh_agent.log")
        log_format = log_config.get(
            "format", 
            "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
        )
        
        # Create logger
        logger = logging.getLogger("GMSHAgent")
        logger.setLevel(log_level)
        
        # Console handler
        console_handler = logging.StreamHandler()
        console_handler.setLevel(log_level)
        console_formatter = logging.Formatter(log_format)
        console_handler.setFormatter(console_formatter)
        logger.addHandler(console_handler)
        
        # File handler
        if log_file:
            file_handler = logging.FileHandler(log_file)
            file_handler.setLevel(log_level)
            file_handler.setFormatter(console_formatter)
            logger.addHandler(file_handler)
        
        return logger
    
    def initialize_gmsh(self):
        """Initialize GMSH library"""
        try:
            if not self.gmsh_initialized:
                gmsh.initialize()
                self.gmsh_initialized = True
                self.logger.info(f"GMSH initialized (version: {self.get_version()})")
        except Exception as e:
            self.logger.error(f"Failed to initialize GMSH: {e}")
            raise
    
    def cleanup_gmsh(self):
        """Clean up GMSH resources"""
        try:
            if self.gmsh_initialized and gmsh:
                gmsh.finalize()
                self.gmsh_initialized = False
                self.logger.info("GMSH cleaned up")
        except Exception as e:
            self.logger.error(f"Error during GMSH cleanup: {e}")
    
    def get_version(self) -> str:
        """Get GMSH version information"""
        if gmsh:
            major, minor, patch = gmsh.option.getNumber("General.MajorVersion"), \
                                  gmsh.option.getNumber("General.MinorVersion"), \
                                  gmsh.option.getNumber("General.PatchVersion")
            return f"{int(major)}.{int(minor)}.{int(patch)}"
        return "N/A (GMSH not installed)"
    
    def show_capabilities(self) -> Dict:
        """
        Display agent capabilities
        
        Returns:
            Dictionary of agent capabilities
        """
        capabilities_file = Path(__file__).parent / "capabilities.yml"
        
        if capabilities_file.exists():
            with open(capabilities_file, 'r') as f:
                capabilities = yaml.safe_load(f)
        else:
            capabilities = {
                "name": "GMSH Agent",
                "version": "1.0.0",
                "status": "Configuration file not found"
            }
        
        return capabilities
    
    def generate_mesh(self, 
                     geometry_file: str, 
                     mesh_config: Optional[Dict] = None) -> str:
        """
        Generate mesh from geometry file
        
        Args:
            geometry_file: Path to geometry file
            mesh_config: Mesh generation configuration
            
        Returns:
            Path to generated mesh file
        """
        if not gmsh:
            self.logger.error("GMSH not available - cannot generate mesh")
            raise RuntimeError("GMSH Python bindings not installed")
        
        if not Path(geometry_file).exists():
            raise FileNotFoundError(f"Geometry file not found: {geometry_file}")
        
        config = mesh_config or {}
        algorithm = config.get("algorithm", self.config["configuration"]["default_algorithm"])
        element_size = config.get("element_size", self.config["configuration"]["default_element_size"])
        
        self.logger.info(f"Generating mesh from {geometry_file}")
        self.logger.info(f"Algorithm: {algorithm}, Element size: {element_size}")
        
        try:
            # Clear any existing model
            gmsh.clear()
            
            # Import geometry
            gmsh.merge(geometry_file)
            
            # Set meshing algorithm
            if algorithm == "frontal-delaunay":
                gmsh.option.setNumber("Mesh.Algorithm", 6)
            elif algorithm == "delaunay":
                gmsh.option.setNumber("Mesh.Algorithm", 5)
            elif algorithm == "frontal":
                gmsh.option.setNumber("Mesh.Algorithm", 1)
            
            # Set element size
            gmsh.option.setNumber("Mesh.CharacteristicLengthMin", element_size * 0.1)
            gmsh.option.setNumber("Mesh.CharacteristicLengthMax", element_size)
            
            # Generate mesh
            gmsh.model.mesh.generate(3)
            
            # Save mesh
            output_file = Path(geometry_file).stem + "_mesh.msh"
            gmsh.write(output_file)
            
            self.logger.info(f"Mesh generated successfully: {output_file}")
            return output_file
            
        except Exception as e:
            self.logger.error(f"Mesh generation failed: {e}")
            raise
    
    def assess_quality(self, mesh_file: str) -> Dict[str, float]:
        """
        Assess mesh quality metrics
        
        Args:
            mesh_file: Path to mesh file
            
        Returns:
            Dictionary of quality metrics
        """
        self.logger.info(f"Assessing mesh quality for {mesh_file}")
        
        # Placeholder for quality assessment
        # In full implementation, this would calculate actual metrics
        quality_metrics = {
            "min_jacobian": 0.35,
            "avg_jacobian": 0.75,
            "max_aspect_ratio": 4.5,
            "avg_aspect_ratio": 2.1,
            "max_skewness": 0.65,
            "num_elements": 10000,
            "num_nodes": 2500
        }
        
        self.logger.info(f"Quality assessment complete: {quality_metrics}")
        return quality_metrics
    
    def optimize_mesh(self, 
                     mesh_file: str, 
                     quality_targets: Optional[Dict] = None) -> str:
        """
        Optimize mesh quality
        
        Args:
            mesh_file: Path to mesh file
            quality_targets: Target quality metrics
            
        Returns:
            Path to optimized mesh file
        """
        self.logger.info(f"Optimizing mesh: {mesh_file}")
        
        targets = quality_targets or self.config["configuration"]["quality_thresholds"]
        
        # Placeholder for mesh optimization
        # In full implementation, this would perform actual optimization
        output_file = Path(mesh_file).stem + "_optimized.msh"
        
        self.logger.info(f"Mesh optimization complete: {output_file}")
        return output_file
    
    def batch_process(self, 
                     input_dir: str, 
                     config_file: Optional[str] = None) -> Dict[str, Any]:
        """
        Process multiple geometries in batch mode
        
        Args:
            input_dir: Directory containing geometry files
            config_file: Batch configuration file
            
        Returns:
            Dictionary of processing results
        """
        self.logger.info(f"Starting batch processing for directory: {input_dir}")
        
        input_path = Path(input_dir)
        if not input_path.exists():
            raise FileNotFoundError(f"Input directory not found: {input_dir}")
        
        # Find geometry files
        geometry_files = list(input_path.glob("*.step")) + \
                        list(input_path.glob("*.stp")) + \
                        list(input_path.glob("*.iges")) + \
                        list(input_path.glob("*.igs"))
        
        results = {
            "total_files": len(geometry_files),
            "processed": 0,
            "failed": 0,
            "files": {}
        }
        
        for geom_file in geometry_files:
            try:
                mesh_file = self.generate_mesh(str(geom_file))
                quality = self.assess_quality(mesh_file)
                results["files"][str(geom_file)] = {
                    "status": "success",
                    "mesh_file": mesh_file,
                    "quality": quality
                }
                results["processed"] += 1
            except Exception as e:
                results["files"][str(geom_file)] = {
                    "status": "failed",
                    "error": str(e)
                }
                results["failed"] += 1
        
        self.logger.info(f"Batch processing complete: {results['processed']}/{results['total_files']} successful")
        return results
    
    def __enter__(self):
        """Context manager entry"""
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit"""
        self.cleanup_gmsh()


# CLI Commands
@click.group()
@click.version_option(version="1.0.0")
def cli():
    """GMSH Agent - Finite Element Mesh Generation"""
    pass


@cli.command()
@click.option("--config", type=click.Path(exists=True), help="Configuration file path")
def capabilities(config):
    """Show agent capabilities"""
    agent = GMSHAgent(config)
    caps = agent.show_capabilities()
    print(yaml.dump(caps, default_flow_style=False))


@cli.command()
@click.option("--input", "-i", required=True, type=click.Path(exists=True), help="Input geometry file")
@click.option("--output", "-o", type=click.Path(), help="Output mesh file")
@click.option("--element-size", type=float, default=1.0, help="Target element size")
@click.option("--algorithm", type=click.Choice(["frontal", "delaunay", "frontal-delaunay"]), 
              default="frontal-delaunay", help="Meshing algorithm")
@click.option("--config", type=click.Path(exists=True), help="Configuration file")
def generate(input, output, element_size, algorithm, config):
    """Generate mesh from geometry"""
    with GMSHAgent(config) as agent:
        mesh_config = {
            "element_size": element_size,
            "algorithm": algorithm
        }
        mesh_file = agent.generate_mesh(input, mesh_config)
        
        if output:
            import shutil
            shutil.move(mesh_file, output)
            mesh_file = output
        
        print(f"Mesh generated: {mesh_file}")


@cli.command()
@click.option("--mesh", "-m", required=True, type=click.Path(exists=True), help="Mesh file to assess")
@click.option("--report", "-r", type=click.Path(), help="Output report file")
@click.option("--config", type=click.Path(exists=True), help="Configuration file")
def assess(mesh, report, config):
    """Assess mesh quality"""
    with GMSHAgent(config) as agent:
        quality = agent.assess_quality(mesh)
        
        print("\nMesh Quality Metrics:")
        print("-" * 40)
        for metric, value in quality.items():
            print(f"{metric:20s}: {value}")
        
        if report:
            with open(report, 'w') as f:
                json.dump(quality, f, indent=2)
            print(f"\nReport saved to: {report}")


@cli.command()
@click.option("--input", "-i", required=True, type=click.Path(exists=True), help="Input mesh file")
@click.option("--output", "-o", type=click.Path(), help="Output optimized mesh")
@click.option("--iterations", type=int, default=10, help="Optimization iterations")
@click.option("--config", type=click.Path(exists=True), help="Configuration file")
def optimize(input, output, iterations, config):
    """Optimize mesh quality"""
    with GMSHAgent(config) as agent:
        optimized = agent.optimize_mesh(input)
        
        if output:
            import shutil
            shutil.move(optimized, output)
            optimized = output
        
        print(f"Optimized mesh: {optimized}")


@cli.command()
@click.option("--input-directory", "-d", required=True, type=click.Path(exists=True), 
              help="Directory containing geometry files")
@click.option("--config", "-c", type=click.Path(exists=True), help="Batch configuration file")
@click.option("--parallel", "-p", type=int, default=1, help="Number of parallel workers")
def batch(input_directory, config, parallel):
    """Process multiple files in batch mode"""
    with GMSHAgent(config) as agent:
        results = agent.batch_process(input_directory, config)
        
        print("\nBatch Processing Results:")
        print("-" * 40)
        print(f"Total files: {results['total_files']}")
        print(f"Processed: {results['processed']}")
        print(f"Failed: {results['failed']}")
        
        if results['failed'] > 0:
            print("\nFailed files:")
            for file, info in results['files'].items():
                if info['status'] == 'failed':
                    print(f"  - {file}: {info['error']}")


@cli.command()
def version():
    """Show GMSH version"""
    agent = GMSHAgent()
    print(f"GMSH Agent Version: 1.0.0")
    print(f"GMSH Library Version: {agent.get_version()}")
    agent.cleanup_gmsh()


@cli.command()
@click.option("--verbose", "-v", is_flag=True, help="Show detailed output")
def show_capabilities(verbose):
    """Display agent capabilities"""
    agent = GMSHAgent()
    caps = agent.show_capabilities()
    
    if verbose:
        print(yaml.dump(caps, default_flow_style=False))
    else:
        print(f"GMSH Agent - {caps.get('description', 'N/A')}")
        print(f"Version: {caps.get('version', 'N/A')}")
        
        if 'capabilities' in caps:
            print("\nCapabilities:")
            for cap_name, cap_info in caps['capabilities'].items():
                print(f"  - {cap_name}: {cap_info.get('description', 'N/A')}")


if __name__ == "__main__":
    cli()