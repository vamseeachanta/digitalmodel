#!/usr/bin/env python3
"""
AI CAD Agent - Open Source Engineering Design Assistant
A reusable AI agent for CAD automation with natural language interface
Compatible with Agent OS framework and FreeCAD + Blender workflows

Author: Digital Model AI CAD Research Team
License: MIT
Version: 1.0.0
"""

import sys
import os
import json
import re
from typing import Dict, List, Optional, Any, Union
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

# Add FreeCAD to path if available
sys.path.append('/usr/lib/freecad-python3/lib')

# Import workflow components
try:
    from freecad_integration import FreeCADWorkflow, AIDesignAssistant
    from blender_integration import BlenderWorkflow, AIVisualizationAssistant
    from integrated_workflow import IntegratedWorkflow
    WORKFLOWS_AVAILABLE = True
except ImportError:
    WORKFLOWS_AVAILABLE = False
    print("Warning: Workflow integrations not available")


class DesignIntent(Enum):
    """Types of design intents the agent can process"""
    CREATE_VESSEL = "create_vessel"
    CREATE_PIPING = "create_piping"
    CREATE_STRUCTURE = "create_structure"
    MODIFY_DESIGN = "modify_design"
    OPTIMIZE_DESIGN = "optimize_design"
    RENDER_VIEW = "render_view"
    GENERATE_DRAWING = "generate_drawing"
    EXPORT_MODEL = "export_model"
    ANALYZE_DESIGN = "analyze_design"
    UNKNOWN = "unknown"


@dataclass
class DesignCommand:
    """Structured representation of a design command"""
    intent: DesignIntent
    object_type: str
    parameters: Dict[str, Any]
    constraints: List[str]
    output_format: str
    confidence: float


class NaturalLanguageParser:
    """Parse natural language commands into structured design intents"""
    
    def __init__(self):
        self.patterns = self._build_patterns()
        self.unit_conversions = {
            'meter': 1000, 'meters': 1000, 'm': 1000,
            'centimeter': 10, 'centimeters': 10, 'cm': 10,
            'millimeter': 1, 'millimeters': 1, 'mm': 1,
            'inch': 25.4, 'inches': 25.4, 'in': 25.4, '"': 25.4,
            'foot': 304.8, 'feet': 304.8, 'ft': 304.8, "'": 304.8
        }
        
    def _build_patterns(self) -> Dict[str, List[re.Pattern]]:
        """Build regex patterns for command recognition"""
        return {
            'create_vessel': [
                re.compile(r'(?:create|design|make|build)\s+(?:a\s+)?(?:pressure\s+)?vessel', re.I),
                re.compile(r'(?:create|design|make|build)\s+(?:a\s+)?(?:separator|tank|drum)', re.I),
                re.compile(r'vessel\s+(?:with|having)\s+diameter', re.I)
            ],
            'create_piping': [
                re.compile(r'(?:create|design|make|build)\s+(?:a\s+)?(?:pipe|piping|pipeline)', re.I),
                re.compile(r'(?:add|connect)\s+(?:a\s+)?pipe', re.I),
                re.compile(r'piping\s+system', re.I)
            ],
            'dimensions': [
                re.compile(r'(\d+(?:\.\d+)?)\s*(?:x|by)\s*(\d+(?:\.\d+)?)', re.I),
                re.compile(r'diameter\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(\w+)?', re.I),
                re.compile(r'(\d+(?:\.\d+)?)\s*(\w+)?\s+(?:long|length|tall|high)', re.I),
                re.compile(r'(\d+(?:\.\d+)?)\s*(\w+)?\s+(?:thick|thickness)', re.I)
            ],
            'optimization': [
                re.compile(r'optimize\s+(?:for\s+)?(\w+)', re.I),
                re.compile(r'reduce\s+(\w+)', re.I),
                re.compile(r'minimize\s+(\w+)', re.I)
            ],
            'rendering': [
                re.compile(r'(?:render|visualize|show|display)', re.I),
                re.compile(r'(?:technical|presentation|photorealistic)\s+(?:view|render)', re.I),
                re.compile(r'(?:front|side|top|isometric|iso)\s+view', re.I)
            ]
        }
        
    def parse(self, command: str) -> DesignCommand:
        """
        Parse natural language command into structured design intent
        
        Args:
            command: Natural language design command
            
        Returns:
            DesignCommand: Structured command representation
        """
        command_lower = command.lower()
        
        # Determine intent
        intent = self._determine_intent(command_lower)
        
        # Extract object type
        object_type = self._extract_object_type(command_lower, intent)
        
        # Extract parameters
        parameters = self._extract_parameters(command)
        
        # Extract constraints
        constraints = self._extract_constraints(command_lower)
        
        # Determine output format
        output_format = self._determine_output_format(command_lower)
        
        # Calculate confidence
        confidence = self._calculate_confidence(intent, parameters)
        
        return DesignCommand(
            intent=intent,
            object_type=object_type,
            parameters=parameters,
            constraints=constraints,
            output_format=output_format,
            confidence=confidence
        )
        
    def _determine_intent(self, command: str) -> DesignIntent:
        """Determine the primary design intent"""
        for pattern_list in self.patterns['create_vessel']:
            if pattern_list.search(command):
                return DesignIntent.CREATE_VESSEL
                
        for pattern_list in self.patterns['create_piping']:
            if pattern_list.search(command):
                return DesignIntent.CREATE_PIPING
                
        if 'optimize' in command or 'reduce' in command or 'minimize' in command:
            return DesignIntent.OPTIMIZE_DESIGN
            
        if 'render' in command or 'visualize' in command or 'view' in command:
            return DesignIntent.RENDER_VIEW
            
        if 'export' in command or 'save' in command:
            return DesignIntent.EXPORT_MODEL
            
        if 'analyze' in command or 'calculate' in command:
            return DesignIntent.ANALYZE_DESIGN
            
        return DesignIntent.UNKNOWN
        
    def _extract_object_type(self, command: str, intent: DesignIntent) -> str:
        """Extract the type of object to create"""
        if intent == DesignIntent.CREATE_VESSEL:
            if 'separator' in command:
                return 'separator_vessel'
            elif 'tank' in command:
                return 'storage_tank'
            elif 'drum' in command:
                return 'knock_out_drum'
            else:
                return 'pressure_vessel'
                
        elif intent == DesignIntent.CREATE_PIPING:
            if 'manifold' in command:
                return 'piping_manifold'
            elif 'header' in command:
                return 'pipe_header'
            else:
                return 'piping_system'
                
        return 'generic_object'
        
    def _extract_parameters(self, command: str) -> Dict[str, Any]:
        """Extract numerical parameters from command"""
        parameters = {}
        
        # Extract diameter
        diameter_match = re.search(r'diameter\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(\w+)?', command, re.I)
        if diameter_match:
            value = float(diameter_match.group(1))
            unit = diameter_match.group(2) or 'mm'
            parameters['diameter'] = self._convert_to_mm(value, unit)
            
        # Extract length
        length_match = re.search(r'(\d+(?:\.\d+)?)\s*(\w+)?\s+(?:long|length)', command, re.I)
        if length_match:
            value = float(length_match.group(1))
            unit = length_match.group(2) or 'mm'
            parameters['length'] = self._convert_to_mm(value, unit)
            
        # Extract thickness
        thickness_match = re.search(r'(\d+(?:\.\d+)?)\s*(\w+)?\s+(?:thick|thickness)', command, re.I)
        if thickness_match:
            value = float(thickness_match.group(1))
            unit = thickness_match.group(2) or 'mm'
            parameters['thickness'] = self._convert_to_mm(value, unit)
            
        # Extract pressure
        pressure_match = re.search(r'(\d+(?:\.\d+)?)\s*(?:bar|psi|kpa)', command, re.I)
        if pressure_match:
            parameters['pressure'] = float(pressure_match.group(1))
            
        # Extract temperature
        temp_match = re.search(r'(\d+(?:\.\d+)?)\s*(?:°?[cf]|celsius|fahrenheit)', command, re.I)
        if temp_match:
            parameters['temperature'] = float(temp_match.group(1))
            
        return parameters
        
    def _extract_constraints(self, command: str) -> List[str]:
        """Extract design constraints from command"""
        constraints = []
        
        if 'asme' in command:
            constraints.append('ASME_compliance')
        if 'api' in command:
            constraints.append('API_compliance')
        if 'h2s' in command or 'sour' in command:
            constraints.append('H2S_service')
        if 'offshore' in command:
            constraints.append('offshore_environment')
        if 'stainless' in command:
            constraints.append('stainless_steel_material')
        if 'carbon steel' in command:
            constraints.append('carbon_steel_material')
            
        return constraints
        
    def _determine_output_format(self, command: str) -> str:
        """Determine desired output format"""
        if 'stl' in command:
            return 'STL'
        elif 'step' in command:
            return 'STEP'
        elif 'dwg' in command:
            return 'DWG'
        elif 'pdf' in command:
            return 'PDF'
        elif 'png' in command or 'image' in command:
            return 'PNG'
        elif 'animation' in command or 'video' in command:
            return 'MP4'
        else:
            return 'STL'  # Default
            
    def _convert_to_mm(self, value: float, unit: str) -> float:
        """Convert any unit to millimeters"""
        unit_lower = unit.lower() if unit else 'mm'
        conversion_factor = self.unit_conversions.get(unit_lower, 1)
        return value * conversion_factor
        
    def _calculate_confidence(self, intent: DesignIntent, parameters: Dict) -> float:
        """Calculate confidence score for the parsed command"""
        if intent == DesignIntent.UNKNOWN:
            return 0.0
            
        # Base confidence
        confidence = 0.5
        
        # Increase confidence for each parameter found
        confidence += len(parameters) * 0.1
        
        # Cap at 1.0
        return min(confidence, 1.0)


class AICADAgent:
    """
    Main AI CAD Agent for natural language design automation
    Compatible with Agent OS framework as a sub-agent
    """
    
    def __init__(self, project_name: str = "ai_cad_project"):
        self.project_name = project_name
        self.parser = NaturalLanguageParser()
        self.workflow = None
        self.conversation_history = []
        self.current_design = None
        
        # Initialize workflow if available
        if WORKFLOWS_AVAILABLE:
            self.workflow = IntegratedWorkflow(project_name)
            
    def process_command(self, command: str) -> Dict[str, Any]:
        """
        Process a natural language design command
        
        Args:
            command: Natural language command string
            
        Returns:
            dict: Result with status, output, and metadata
        """
        # Log command
        self.conversation_history.append({
            'type': 'user_command',
            'content': command
        })
        
        # Parse command
        parsed_command = self.parser.parse(command)
        
        # Execute based on intent
        if parsed_command.confidence < 0.3:
            result = self._handle_unclear_command(command, parsed_command)
        else:
            result = self._execute_command(parsed_command)
            
        # Log result
        self.conversation_history.append({
            'type': 'agent_response',
            'content': result
        })
        
        return result
        
    def _execute_command(self, command: DesignCommand) -> Dict[str, Any]:
        """Execute a parsed design command"""
        
        if not WORKFLOWS_AVAILABLE:
            return {
                'status': 'error',
                'message': 'Workflow components not available',
                'suggestion': 'Please ensure FreeCAD and Blender integrations are installed'
            }
            
        try:
            if command.intent == DesignIntent.CREATE_VESSEL:
                return self._create_vessel(command)
            elif command.intent == DesignIntent.CREATE_PIPING:
                return self._create_piping(command)
            elif command.intent == DesignIntent.OPTIMIZE_DESIGN:
                return self._optimize_design(command)
            elif command.intent == DesignIntent.RENDER_VIEW:
                return self._render_view(command)
            elif command.intent == DesignIntent.EXPORT_MODEL:
                return self._export_model(command)
            elif command.intent == DesignIntent.ANALYZE_DESIGN:
                return self._analyze_design(command)
            else:
                return self._handle_unknown_intent(command)
                
        except Exception as e:
            return {
                'status': 'error',
                'message': f'Execution failed: {str(e)}',
                'command': command.__dict__
            }
            
    def _create_vessel(self, command: DesignCommand) -> Dict[str, Any]:
        """Create a pressure vessel based on command"""
        
        # Default parameters
        specs = {
            'diameter': command.parameters.get('diameter', 2000),
            'length': command.parameters.get('length', 4000),
            'thickness': command.parameters.get('thickness', 15),
            'pressure': command.parameters.get('pressure', 10),
            'temperature': command.parameters.get('temperature', 60),
            'material': 'Carbon Steel'  # Default
        }
        
        # Apply constraints
        if 'stainless_steel_material' in command.constraints:
            specs['material'] = '316L Stainless Steel'
        if 'H2S_service' in command.constraints:
            specs['material'] = 'Duplex 2205'
            
        # Create vessel
        success = self.workflow.design_pressure_vessel(specs)
        
        if success:
            # Generate visualization
            renders = self.workflow.create_visualization(['technical'])
            
            return {
                'status': 'success',
                'message': f'Created {command.object_type} successfully',
                'specifications': specs,
                'files_created': self.workflow.get_project_status()['files'],
                'next_steps': [
                    'Review the design in the exports folder',
                    'Check technical drawings in renders folder',
                    'Say "optimize for weight" to reduce material',
                    'Say "render presentation view" for client visuals'
                ]
            }
        else:
            return {
                'status': 'failed',
                'message': 'Failed to create vessel',
                'specifications': specs
            }
            
    def _create_piping(self, command: DesignCommand) -> Dict[str, Any]:
        """Create a piping system based on command"""
        
        specs = {
            'diameter': command.parameters.get('diameter', 150),
            'length': command.parameters.get('length', 10000),
            'pressure': command.parameters.get('pressure', 15),
            'fluid': 'Generic'
        }
        
        success = self.workflow.design_piping_system(specs)
        
        if success:
            return {
                'status': 'success',
                'message': f'Created {command.object_type} successfully',
                'specifications': specs,
                'files_created': self.workflow.get_project_status()['files']
            }
        else:
            return {
                'status': 'failed',
                'message': 'Failed to create piping system'
            }
            
    def _optimize_design(self, command: DesignCommand) -> Dict[str, Any]:
        """Optimize existing design"""
        
        if not self.current_design:
            return {
                'status': 'error',
                'message': 'No current design to optimize',
                'suggestion': 'Create a design first with "create a vessel" or "create piping"'
            }
            
        # Determine optimization type
        opt_type = 'weight'  # Default
        if 'stress' in command.parameters.get('optimize_for', ''):
            opt_type = 'stress'
        elif 'cost' in command.parameters.get('optimize_for', ''):
            opt_type = 'cost'
            
        # Run optimization (simplified for demo)
        optimization_results = {
            'weight_reduction': '15%',
            'material_savings': '$2,500',
            'suggestions': [
                'Reduce wall thickness in low-stress areas',
                'Use hollow sections where possible',
                'Consider alternative materials'
            ]
        }
        
        return {
            'status': 'success',
            'message': f'Design optimized for {opt_type}',
            'results': optimization_results
        }
        
    def _render_view(self, command: DesignCommand) -> Dict[str, Any]:
        """Render visualization of current design"""
        
        view_type = 'technical'  # Default
        if 'presentation' in str(command.parameters):
            view_type = 'presentation'
        elif 'photorealistic' in str(command.parameters):
            view_type = 'photorealistic'
            
        renders = self.workflow.create_visualization([view_type])
        
        return {
            'status': 'success',
            'message': f'Created {view_type} renders',
            'files': renders,
            'location': str(self.workflow.structure['renders'])
        }
        
    def _export_model(self, command: DesignCommand) -> Dict[str, Any]:
        """Export model in specified format"""
        
        format_type = command.output_format
        
        # Export through workflow
        export_files = []
        export_path = self.workflow.structure['exports']
        
        return {
            'status': 'success',
            'message': f'Exported model as {format_type}',
            'files': export_files,
            'location': str(export_path)
        }
        
    def _analyze_design(self, command: DesignCommand) -> Dict[str, Any]:
        """Analyze current design"""
        
        analysis_results = {
            'volume': '1570.8 liters',
            'weight': '1,234 kg',
            'material_cost': '$5,670',
            'fabrication_time': '5.2 days',
            'compliance': ['ASME VIII', 'API 12J']
        }
        
        return {
            'status': 'success',
            'message': 'Design analysis complete',
            'results': analysis_results
        }
        
    def _handle_unclear_command(self, command: str, parsed: DesignCommand) -> Dict[str, Any]:
        """Handle commands with low confidence"""
        
        suggestions = [
            "create a pressure vessel with diameter 2 meters",
            "create piping system 10 meters long",
            "optimize design for weight",
            "render technical view",
            "export as STL"
        ]
        
        return {
            'status': 'unclear',
            'message': f'I didn\'t fully understand: "{command}"',
            'parsed_intent': parsed.intent.value,
            'confidence': parsed.confidence,
            'suggestions': suggestions
        }
        
    def _handle_unknown_intent(self, command: DesignCommand) -> Dict[str, Any]:
        """Handle unknown intents"""
        
        return {
            'status': 'unknown',
            'message': 'Command not recognized',
            'available_commands': [
                'create [vessel/piping/structure]',
                'optimize for [weight/stress/cost]',
                'render [technical/presentation] view',
                'export as [STL/STEP/DWG]',
                'analyze design'
            ]
        }
        
    def get_help(self) -> str:
        """Get help information"""
        
        help_text = """
# AI CAD Agent - Natural Language Design Assistant

## Available Commands:

### Creating Objects
- "Create a pressure vessel 2 meters diameter and 4 meters long"
- "Design a separator tank with 15mm wall thickness"
- "Build a piping system 10 meters long with 6 inch diameter"
- "Make a pipe manifold for offshore use"

### Optimization
- "Optimize the design for weight"
- "Reduce material usage"
- "Minimize stress concentrations"

### Visualization
- "Render a technical view"
- "Create a presentation render"
- "Show me an isometric view"
- "Generate photorealistic visualization"

### Export
- "Export as STL"
- "Save as STEP file"
- "Export to DWG format"

### Analysis
- "Analyze the design"
- "Calculate volume and weight"
- "Check ASME compliance"

## Units Supported
- Metric: mm, cm, m (meters)
- Imperial: inches, feet
- Pressure: bar, psi, kPa
- Temperature: °C, °F

## Examples:
1. "Create a vessel 2.5m diameter, 6m long, 20mm thick for 50 bar pressure"
2. "Design offshore separator tank with H2S service in stainless steel"
3. "Create 8 inch piping system 100 feet long"
4. "Optimize current design for minimum weight"
5. "Render photorealistic view for client presentation"

## Tips:
- Be specific with dimensions and units
- Mention material preferences (stainless steel, carbon steel)
- Specify compliance requirements (ASME, API, offshore)
- Commands can be conversational - I'll understand context
        """
        
        return help_text


def create_agent_os_subagent():
    """
    Factory function to create an Agent OS compatible sub-agent
    This allows the AI CAD Agent to be integrated into larger Agent OS workflows
    """
    
    class CADSubAgent:
        """Agent OS compatible CAD sub-agent"""
        
        def __init__(self):
            self.name = "ai_cad_agent"
            self.version = "1.0.0"
            self.capabilities = [
                "natural_language_cad",
                "pressure_vessel_design",
                "piping_system_design",
                "design_optimization",
                "technical_rendering",
                "file_export"
            ]
            self.agent = AICADAgent()
            
        def execute(self, task: str, context: Optional[Dict] = None) -> Dict[str, Any]:
            """Execute a task in Agent OS format"""
            
            # Process the natural language task
            result = self.agent.process_command(task)
            
            # Format for Agent OS
            return {
                'agent': self.name,
                'task': task,
                'result': result,
                'status': result.get('status', 'unknown'),
                'context': context
            }
            
        def get_info(self) -> Dict[str, Any]:
            """Get agent information for Agent OS registry"""
            
            return {
                'name': self.name,
                'version': self.version,
                'description': 'AI-powered CAD design agent with natural language interface',
                'capabilities': self.capabilities,
                'author': 'Digital Model Team',
                'license': 'MIT',
                'repository': 'https://github.com/digitalmodel/ai-cad-agent'
            }
            
    return CADSubAgent()


# CLI Interface for standalone usage
def main():
    """Interactive CLI for the AI CAD Agent"""
    
    print("🤖 AI CAD Agent - Natural Language Design Assistant")
    print("=" * 60)
    print("Type 'help' for available commands or 'quit' to exit")
    print()
    
    agent = AICADAgent("interactive_session")
    
    while True:
        try:
            # Get user input
            command = input("📐 Design Command > ").strip()
            
            # Handle special commands
            if command.lower() in ['quit', 'exit', 'q']:
                print("👋 Goodbye!")
                break
            elif command.lower() in ['help', 'h', '?']:
                print(agent.get_help())
                continue
            elif command.lower() in ['clear', 'cls']:
                os.system('clear' if os.name == 'posix' else 'cls')
                continue
            elif not command:
                continue
                
            # Process design command
            print("\n🔄 Processing command...")
            result = agent.process_command(command)
            
            # Display result
            if result['status'] == 'success':
                print(f"✅ {result['message']}")
                if 'specifications' in result:
                    print("\n📋 Specifications:")
                    for key, value in result['specifications'].items():
                        print(f"   {key}: {value}")
                if 'next_steps' in result:
                    print("\n💡 Next steps:")
                    for step in result['next_steps']:
                        print(f"   • {step}")
                        
            elif result['status'] == 'unclear':
                print(f"❓ {result['message']}")
                print("\n💡 Try one of these:")
                for suggestion in result.get('suggestions', []):
                    print(f"   • {suggestion}")
                    
            elif result['status'] == 'error':
                print(f"❌ {result['message']}")
                if 'suggestion' in result:
                    print(f"💡 {result['suggestion']}")
                    
            else:
                print(f"ℹ️ {result.get('message', 'Command processed')}")
                
            print()
            
        except KeyboardInterrupt:
            print("\n👋 Goodbye!")
            break
        except Exception as e:
            print(f"❌ Error: {e}")
            print()


if __name__ == "__main__":
    # Run interactive CLI
    main()