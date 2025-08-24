"""
AI Integration Layer for FreeCAD Agent
Provides LLM integration for enhanced natural language understanding
"""

import json
import os
from typing import Dict, List, Any, Optional, Tuple
from datetime import datetime
from loguru import logger

from .parser import ParsedCommand, CommandIntent, ObjectType


class AIIntegration:
    """
    Integrates AI/LLM capabilities for enhanced prompt understanding
    """
    
    def __init__(self, model: str = "local", api_key: Optional[str] = None):
        """
        Initialize AI integration
        
        Args:
            model: Model to use (local, gpt-4, claude, etc.)
            api_key: API key if using external service
        """
        self.model = model
        self.api_key = api_key or os.getenv("AI_API_KEY")
        self.conversation_history = []
        self.learning_data = []
        self.suggestions_cache = {}
        
    def enhance_command(self, command: ParsedCommand) -> ParsedCommand:
        """
        Enhance a parsed command using AI
        
        Args:
            command: Initial parsed command
            
        Returns:
            Enhanced command with improved understanding
        """
        # If confidence is low, try to improve understanding
        if command.confidence < 0.7:
            enhanced = self._enhance_with_ai(command)
            if enhanced:
                return enhanced
        
        # Add intelligent defaults based on context
        command = self._add_intelligent_defaults(command)
        
        # Generate design suggestions if applicable
        if command.intent == CommandIntent.CREATE_OBJECT:
            command.parameters['suggestions'] = self._generate_design_suggestions(command)
        
        return command
    
    def _enhance_with_ai(self, command: ParsedCommand) -> Optional[ParsedCommand]:
        """Use AI to better understand ambiguous commands"""
        
        # For local mode, use rule-based enhancement
        if self.model == "local":
            return self._local_enhancement(command)
        
        # For external AI, would call API here
        # This is a placeholder for actual AI API integration
        logger.info(f"AI enhancement would be applied here for model: {self.model}")
        return command
    
    def _local_enhancement(self, command: ParsedCommand) -> ParsedCommand:
        """Local rule-based command enhancement"""
        
        # Enhance based on common patterns
        prompt_lower = command.raw_prompt.lower()
        
        # Marine engineering context detection
        if any(word in prompt_lower for word in ['ship', 'vessel', 'marine', 'offshore']):
            if command.object_type == ObjectType.UNKNOWN:
                if 'hull' in prompt_lower:
                    command.object_type = ObjectType.HULL
                elif 'beam' in prompt_lower or 'girder' in prompt_lower:
                    command.object_type = ObjectType.BEAM
                elif 'plate' in prompt_lower or 'panel' in prompt_lower:
                    command.object_type = ObjectType.PLATE
            
            # Add marine-specific parameters
            if command.object_type == ObjectType.HULL:
                if 'length' not in command.parameters:
                    command.parameters['length'] = 150000  # 150m default
                if 'beam' in prompt_lower and 'width' not in command.parameters:
                    # Extract beam value
                    import re
                    beam_match = re.search(r'beam\s+(?:of\s+)?(\d+)', prompt_lower)
                    if beam_match:
                        command.parameters['width'] = float(beam_match.group(1)) * 1000
        
        # Engineering unit inference
        if any(word in prompt_lower for word in ['meter', 'metres', 'm ']):
            # Convert to mm if values seem to be in meters
            for key in ['length', 'width', 'height', 'radius']:
                if key in command.parameters and command.parameters[key] < 1000:
                    command.parameters[key] *= 1000  # Convert to mm
        
        # Improve confidence based on enhancements
        if command.object_type != ObjectType.UNKNOWN:
            command.confidence = min(command.confidence + 0.2, 1.0)
        
        return command
    
    def _add_intelligent_defaults(self, command: ParsedCommand) -> ParsedCommand:
        """Add intelligent default values based on object type and context"""
        
        defaults = self._get_intelligent_defaults(command.object_type)
        
        for param, value in defaults.items():
            if param not in command.parameters:
                command.parameters[param] = value
        
        # Context-aware adjustments
        if self.conversation_history:
            last_command = self.conversation_history[-1]
            # If creating similar object, use similar dimensions
            if last_command.object_type == command.object_type:
                for param in ['length', 'width', 'height', 'radius']:
                    if param in last_command.parameters and param not in command.parameters:
                        command.parameters[param] = last_command.parameters[param]
        
        return command
    
    def _get_intelligent_defaults(self, obj_type: Optional[ObjectType]) -> Dict[str, Any]:
        """Get intelligent default values for object type"""
        
        if not obj_type:
            return {}
        
        defaults = {
            ObjectType.BOX: {'length': 100, 'width': 100, 'height': 100},
            ObjectType.CYLINDER: {'radius': 50, 'height': 100},
            ObjectType.SPHERE: {'radius': 50},
            ObjectType.HULL: {'length': 150000, 'width': 25000, 'height': 15000},
            ObjectType.BEAM: {'length': 5000, 'width': 200, 'height': 400, 'thickness': 20},
            ObjectType.PLATE: {'length': 2000, 'width': 1000, 'thickness': 10},
            ObjectType.CONE: {'radius1': 50, 'radius2': 25, 'height': 100},
            ObjectType.TORUS: {'major_radius': 50, 'minor_radius': 10}
        }
        
        return defaults.get(obj_type, {})
    
    def _generate_design_suggestions(self, command: ParsedCommand) -> List[str]:
        """Generate design suggestions based on the command"""
        
        suggestions = []
        obj_type = command.object_type
        params = command.parameters
        
        if obj_type == ObjectType.HULL:
            suggestions.append("Consider adding bulbous bow for improved hydrodynamics")
            suggestions.append("Typical L/B ratio for cargo vessels is 6-7")
            suggestions.append("Add bilge keels for roll reduction")
        
        elif obj_type == ObjectType.BEAM:
            # Check for structural efficiency
            if 'height' in params and 'width' in params:
                h_w_ratio = params['height'] / params['width']
                if h_w_ratio < 1.5:
                    suggestions.append("Consider increasing height/width ratio for better bending resistance")
                if h_w_ratio > 3:
                    suggestions.append("High height/width ratio may cause lateral-torsional buckling")
        
        elif obj_type == ObjectType.BOX:
            # Check for standard sizes
            if all(k in params for k in ['length', 'width', 'height']):
                volume = params['length'] * params['width'] * params['height']
                if volume < 1000:  # Less than 1 liter
                    suggestions.append("Very small volume - confirm dimensions are in mm")
        
        # Material suggestions
        if 'material' not in params:
            if obj_type in [ObjectType.BEAM, ObjectType.PLATE]:
                suggestions.append("Consider specifying material (steel, aluminum, etc.)")
        
        return suggestions
    
    def interpret_error(self, error: Exception, context: Dict[str, Any]) -> str:
        """
        Interpret error messages and provide user guidance
        
        Args:
            error: The exception that occurred
            context: Context about the operation
            
        Returns:
            User-friendly error message with guidance
        """
        error_str = str(error)
        guidance = []
        
        # Common FreeCAD errors
        if "No active document" in error_str:
            guidance.append("No document is open. Create a new document first.")
            guidance.append("Try: 'Create new document' or 'Open file.FCStd'")
        
        elif "not found" in error_str.lower():
            obj_name = context.get('object_name', 'Object')
            guidance.append(f"Object '{obj_name}' doesn't exist in the current document.")
            guidance.append("Check object name or create the object first.")
        
        elif "ImportError" in error_str or "ModuleNotFoundError" in error_str:
            guidance.append("FreeCAD module not available.")
            guidance.append("Ensure FreeCAD is installed and Python bindings are configured.")
        
        elif "invalid" in error_str.lower():
            guidance.append("Invalid parameters provided.")
            guidance.append("Check that dimensions are positive numbers.")
            guidance.append("Ensure angles are in degrees (0-360).")
        
        else:
            # Generic guidance
            guidance.append("An error occurred during execution.")
            guidance.append("Check the command syntax and parameters.")
        
        # Add suggestion based on context
        if context.get('intent') == CommandIntent.CREATE_OBJECT:
            guidance.append(f"Example: Create a {context.get('object_type', 'box')} 100x50x25")
        
        return "\n".join(guidance)
    
    def learn_from_feedback(self, command: ParsedCommand, 
                           success: bool, feedback: Optional[str] = None) -> None:
        """
        Learn from command execution feedback
        
        Args:
            command: The executed command
            success: Whether execution was successful
            feedback: Optional user feedback
        """
        learning_entry = {
            'timestamp': datetime.now().isoformat(),
            'prompt': command.raw_prompt,
            'intent': command.intent.value,
            'object_type': command.object_type.value if command.object_type else None,
            'parameters': command.parameters,
            'success': success,
            'confidence': command.confidence,
            'feedback': feedback
        }
        
        self.learning_data.append(learning_entry)
        
        # Update conversation history
        self.conversation_history.append(command)
        if len(self.conversation_history) > 10:
            self.conversation_history.pop(0)
        
        # Analyze patterns for improvement
        if len(self.learning_data) >= 10:
            self._analyze_learning_patterns()
    
    def _analyze_learning_patterns(self) -> None:
        """Analyze learning data to identify patterns"""
        
        # Calculate success rate by intent
        intent_stats = {}
        for entry in self.learning_data[-100:]:  # Last 100 entries
            intent = entry['intent']
            if intent not in intent_stats:
                intent_stats[intent] = {'success': 0, 'total': 0}
            intent_stats[intent]['total'] += 1
            if entry['success']:
                intent_stats[intent]['success'] += 1
        
        # Log insights
        for intent, stats in intent_stats.items():
            success_rate = stats['success'] / stats['total'] if stats['total'] > 0 else 0
            if success_rate < 0.7:
                logger.warning(f"Low success rate for {intent}: {success_rate:.2%}")
    
    def suggest_next_action(self, current_command: ParsedCommand) -> List[str]:
        """
        Suggest next actions based on current command
        
        Args:
            current_command: The current command
            
        Returns:
            List of suggested next actions
        """
        suggestions = []
        
        if current_command.intent == CommandIntent.CREATE_OBJECT:
            obj_name = current_command.parameters.get('name', 'object')
            suggestions.extend([
                f"Add fillet to {obj_name} edges",
                f"Create a copy of {obj_name}",
                f"Export {obj_name} to STEP format",
                f"Measure {obj_name} dimensions"
            ])
            
            if current_command.object_type == ObjectType.BOX:
                suggestions.append(f"Create cylinder to cut hole in {obj_name}")
            elif current_command.object_type == ObjectType.HULL:
                suggestions.append("Add deck structures")
                suggestions.append("Create bulkheads")
        
        elif current_command.intent == CommandIntent.CREATE_SKETCH:
            suggestions.extend([
                "Extrude sketch to create solid",
                "Add constraints to sketch",
                "Create revolution from sketch"
            ])
        
        elif current_command.intent == CommandIntent.MODIFY_OBJECT:
            suggestions.extend([
                "Save document",
                "Export to STEP",
                "Create assembly"
            ])
        
        return suggestions[:5]  # Limit suggestions
    
    def get_learning_summary(self) -> Dict[str, Any]:
        """Get summary of learning data"""
        
        if not self.learning_data:
            return {'total_commands': 0, 'success_rate': 0}
        
        total = len(self.learning_data)
        successful = sum(1 for entry in self.learning_data if entry['success'])
        
        # Most common intents
        intent_counts = {}
        for entry in self.learning_data:
            intent = entry['intent']
            intent_counts[intent] = intent_counts.get(intent, 0) + 1
        
        return {
            'total_commands': total,
            'successful_commands': successful,
            'success_rate': successful / total if total > 0 else 0,
            'most_common_intents': sorted(intent_counts.items(), 
                                         key=lambda x: x[1], reverse=True)[:5],
            'average_confidence': sum(e['confidence'] for e in self.learning_data) / total
        }
    
    def export_learning_data(self, filename: str) -> bool:
        """Export learning data to file"""
        try:
            with open(filename, 'w') as f:
                json.dump(self.learning_data, f, indent=2)
            logger.info(f"Learning data exported to: {filename}")
            return True
        except Exception as e:
            logger.error(f"Failed to export learning data: {e}")
            return False