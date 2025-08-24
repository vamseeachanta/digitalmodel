#!/usr/bin/env python
"""
FreeCAD Agent Runner - Main entry point for the FreeCAD Agent
"""

import sys
import argparse
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent))

from src.core.agent import FreeCADAgent
from src.core.logging_config import logger


def main():
    """Main entry point for FreeCAD Agent"""
    parser = argparse.ArgumentParser(
        description="FreeCAD Agent - AI-powered CAD automation",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Show agent capabilities
  python run_freecad_agent.py --show-capabilities
  
  # Create a simple box
  python run_freecad_agent.py --prompt "Create a box 100x50x25"
  
  # Batch process files
  python run_freecad_agent.py --batch --pattern "*.FCStd" --input-directory ./models
  
  # Export single file to STEP
  python run_freecad_agent.py --file model.FCStd --export model.step
        """
    )
    
    # General options
    parser.add_argument(
        '--config',
        type=str,
        help='Path to configuration file'
    )
    parser.add_argument(
        '--show-capabilities',
        action='store_true',
        help='Display agent capabilities'
    )
    parser.add_argument(
        '--metrics',
        action='store_true',
        help='Show performance metrics'
    )
    parser.add_argument(
        '-v', '--verbose',
        action='store_true',
        help='Enable verbose output'
    )
    
    # Natural language operation
    parser.add_argument(
        '--prompt', '-p',
        type=str,
        help='Natural language command to execute'
    )
    
    # File operations
    parser.add_argument(
        '--file', '-f',
        type=str,
        help='Input FreeCAD file'
    )
    parser.add_argument(
        '--export', '-e',
        type=str,
        help='Export to specified file'
    )
    parser.add_argument(
        '--format',
        type=str,
        choices=['STEP', 'IGES', 'STL', 'DXF', 'PDF'],
        help='Export format'
    )
    
    # Batch operations
    parser.add_argument(
        '--batch', '-b',
        action='store_true',
        help='Enable batch processing mode'
    )
    parser.add_argument(
        '--pattern',
        type=str,
        default='*.FCStd',
        help='File pattern for batch processing (default: *.FCStd)'
    )
    parser.add_argument(
        '--input-directory', '--directory', '-d',
        type=str,
        dest='directory',
        default='.',
        help='Input directory for batch processing'
    )
    parser.add_argument(
        '--output-directory', '--output', '-o',
        type=str,
        dest='output',
        help='Output directory for results'
    )
    parser.add_argument(
        '--parallel',
        type=int,
        default=4,
        help='Number of parallel workers for batch processing'
    )
    parser.add_argument(
        '--operation',
        type=str,
        default='export_step',
        help='Batch operation to perform'
    )
    
    # Object creation
    parser.add_argument(
        '--create',
        type=str,
        choices=['box', 'cylinder', 'sphere'],
        help='Create a basic shape'
    )
    parser.add_argument(
        '--dimensions',
        type=float,
        nargs='+',
        help='Dimensions for shape creation'
    )
    parser.add_argument(
        '--name',
        type=str,
        help='Name for created object'
    )
    
    args = parser.parse_args()
    
    # Initialize agent
    agent = FreeCADAgent(args.config)
    
    # Handle commands
    if args.show_capabilities:
        agent.show_capabilities()
        return 0
    
    if args.metrics:
        metrics = agent.get_metrics()
        print("\nPerformance Metrics:")
        print("-" * 40)
        for key, value in metrics.items():
            print(f"  {key}: {value}")
        return 0
    
    # Natural language prompt
    if args.prompt:
        result = agent.execute_prompt(args.prompt)
        print(f"\nResult: {result['message']}")
        if not result['success'] and 'hint' in result:
            print(f"Hint: {result['hint']}")
        return 0 if result['success'] else 1
    
    # File operations
    if args.file:
        # Open file
        result = agent.open_document(args.file)
        if not result['success']:
            print(f"Error: {result.get('error', 'Failed to open file')}")
            return 1
        
        # Export if requested
        if args.export:
            result = agent.export_document(args.export, args.format)
            print(f"Export: {result['message']}")
            return 0 if result['success'] else 1
        
        return 0
    
    # Batch processing
    if args.batch:
        print(f"\nBatch Processing:")
        print(f"  Pattern: {args.pattern}")
        print(f"  Directory: {args.directory}")
        print(f"  Operation: {args.operation}")
        print(f"  Workers: {args.parallel}")
        print()
        
        result = agent.batch_process(
            pattern=args.pattern,
            input_directory=args.directory,
            operation=args.operation,
            parallel_workers=args.parallel
        )
        
        if result['success']:
            print(f"\nProcessing Complete:")
            print(f"  Total files: {result['total_files']}")
            print(f"  Successful: {result['successful']}")
            print(f"  Failed: {result['failed']}")
            print(f"  Duration: {result['duration']:.2f}s")
        else:
            print(f"Error: {result.get('message', 'Batch processing failed')}")
        
        return 0 if result['success'] else 1
    
    # Shape creation
    if args.create:
        # Create new document first
        agent.new_document("CreatedShapes")
        
        if args.create == 'box':
            if args.dimensions and len(args.dimensions) >= 3:
                result = agent.create_box(
                    args.dimensions[0],
                    args.dimensions[1],
                    args.dimensions[2],
                    name=args.name
                )
            else:
                result = agent.create_box(100, 100, 100, name=args.name)
        
        elif args.create == 'cylinder':
            if args.dimensions and len(args.dimensions) >= 2:
                result = agent.create_cylinder(
                    args.dimensions[0],
                    args.dimensions[1],
                    name=args.name
                )
            else:
                result = agent.create_cylinder(50, 100, name=args.name)
        
        elif args.create == 'sphere':
            if args.dimensions and len(args.dimensions) >= 1:
                result = agent.create_sphere(args.dimensions[0], name=args.name)
            else:
                result = agent.create_sphere(50, name=args.name)
        
        print(f"\nResult: {result['message']}")
        
        # Save if output specified
        if args.output:
            save_path = Path(args.output) / "created_shapes.FCStd"
            agent.save_document(str(save_path))
            print(f"Saved to: {save_path}")
        
        return 0 if result['success'] else 1
    
    # No command specified
    print("No operation specified. Use --help for usage information.")
    return 1


if __name__ == "__main__":
    sys.exit(main())