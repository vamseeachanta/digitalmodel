"""
Natural Language Parser for FreeCAD Agent
Translates natural language prompts into CAD commands
"""

import re
from typing import Dict, List, Any, Optional, Tuple
from enum import Enum
from dataclasses import dataclass
from loguru import logger


class CommandIntent(Enum):
    """Enumeration of possible command intents"""
    CREATE_OBJECT = "create_object"
    MODIFY_OBJECT = "modify_object"
    DELETE_OBJECT = "delete_object"
    EXPORT_FILE = "export_file"
    IMPORT_FILE = "import_file"
    SAVE_DOCUMENT = "save_document"
    OPEN_DOCUMENT = "open_document"
    CREATE_SKETCH = "create_sketch"
    ADD_CONSTRAINT = "add_constraint"
    ASSEMBLY_OPERATION = "assembly_operation"
    MEASURE = "measure"
    ANALYSIS = "analysis"
    RENDER = "render"
    UNKNOWN = "unknown"


class ObjectType(Enum):
    """Types of objects that can be created"""
    BOX = "box"
    CYLINDER = "cylinder"
    SPHERE = "sphere"
    CONE = "cone"
    TORUS = "torus"
    WEDGE = "wedge"
    PYRAMID = "pyramid"
    PRISM = "prism"
    TUBE = "tube"
    SKETCH = "sketch"
    EXTRUSION = "extrusion"
    REVOLUTION = "revolution"
    LOFT = "loft"
    SWEEP = "sweep"
    FILLET = "fillet"
    CHAMFER = "chamfer"
    HULL = "hull"
    BEAM = "beam"
    PLATE = "plate"
    UNKNOWN = "unknown"


@dataclass
class ParsedCommand:
    """Structured representation of a parsed command"""
    intent: CommandIntent
    object_type: Optional[ObjectType]
    parameters: Dict[str, Any]
    modifiers: List[str]
    context: Dict[str, Any]
    confidence: float
    raw_prompt: str


class NaturalLanguageParser:
    """
    Parser for converting natural language to structured CAD commands
    """
    
    def __init__(self):
        """Initialize the parser with patterns and rules"""
        self.intent_patterns = self._build_intent_patterns()
        self.object_patterns = self._build_object_patterns()
        self.parameter_patterns = self._build_parameter_patterns()
        self.unit_conversions = self._build_unit_conversions()
        self.context = {}  # Maintains conversation context
        
    def _build_intent_patterns(self) -> Dict[CommandIntent, List[str]]:
        """Build regex patterns for intent recognition"""
        return {
            CommandIntent.CREATE_OBJECT: [
                r'\b(create|make|build|add|generate|construct|draw)\b',
                r'\b(new)\s+(box|cylinder|sphere|object)\b'
            ],
            CommandIntent.MODIFY_OBJECT: [
                r'\b(modify|change|edit|update|alter|adjust|resize|scale|move|rotate)\b',
                r'\b(set|make)\s+\w+\s+(to|as)\b'
            ],
            CommandIntent.DELETE_OBJECT: [
                r'\b(delete|remove|erase|clear|destroy)\b'
            ],
            CommandIntent.EXPORT_FILE: [
                r'\b(export|save as|convert to|output)\b',
                r'\b(to)\s+(step|iges|stl|dxf|pdf)\b'
            ],
            CommandIntent.IMPORT_FILE: [
                r'\b(import|open|load|read)\b.*\.(fcstd|step|iges|stl|dxf)'
            ],
            CommandIntent.SAVE_DOCUMENT: [
                r'\b(save)\b(?!\s+as)'
            ],
            CommandIntent.CREATE_SKETCH: [
                r'\b(sketch|draw)\s+(a|the)?\s*(rectangle|circle|polygon|line)\b',
                r'\b(create|make)\s+(a|the)?\s*sketch\b'
            ],
            CommandIntent.ADD_CONSTRAINT: [
                r'\b(constrain|fix|lock|set)\s+(distance|angle|radius|length)\b',
                r'\b(make)\s+\w+\s+(parallel|perpendicular|horizontal|vertical|tangent)\b'
            ],
            CommandIntent.ASSEMBLY_OPERATION: [
                r'\b(assemble|join|connect|attach|mate)\b',
                r'\b(create|make)\s+(an?\s+)?assembly\b'
            ],
            CommandIntent.MEASURE: [
                r'\b(measure|calculate|compute|find)\s+(distance|area|volume|length|angle)\b'
            ],
            CommandIntent.ANALYSIS: [
                r'\b(analyze|simulate|test|check)\b',
                r'\b(run|perform)\s+(fem|stress|thermal|flow)\s+analysis\b'
            ]
        }
    
    def _build_object_patterns(self) -> Dict[ObjectType, List[str]]:
        """Build patterns for object type recognition"""
        return {
            ObjectType.BOX: [r'\b(box|cube|cuboid|block|brick)\b'],
            ObjectType.CYLINDER: [r'\b(cylinder|rod|barrel|pipe)\b'],
            ObjectType.SPHERE: [r'\b(sphere|ball|globe|orb)\b'],
            ObjectType.CONE: [r'\b(cone|conical)\b'],
            ObjectType.TORUS: [r'\b(torus|donut|ring)\b'],
            ObjectType.WEDGE: [r'\b(wedge|ramp)\b'],
            ObjectType.PYRAMID: [r'\b(pyramid)\b'],
            ObjectType.TUBE: [r'\b(tube|hollow cylinder|pipe)\b'],
            ObjectType.HULL: [r'\b(hull|ship hull|vessel)\b'],
            ObjectType.BEAM: [r'\b(beam|girder|joist)\b'],
            ObjectType.PLATE: [r'\b(plate|sheet|panel)\b'],
            ObjectType.FILLET: [r'\b(fillet|round|rounded edge)\b'],
            ObjectType.CHAMFER: [r'\b(chamfer|bevel|beveled edge)\b']
        }
    
    def _build_parameter_patterns(self) -> Dict[str, str]:
        """Build patterns for parameter extraction"""
        return {
            'dimension': r'(\d+(?:\.\d+)?)\s*(mm|cm|m|in|ft|inch|inches|foot|feet)?',
            'angle': r'(\d+(?:\.\d+)?)\s*(deg|degree|degrees|rad|radians?)?',
            'radius': r'radius\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(mm|cm|m|in)?',
            'diameter': r'diameter\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(mm|cm|m|in)?',
            'length': r'length\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(mm|cm|m|in|ft)?',
            'width': r'width\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(mm|cm|m|in|ft)?',
            'height': r'height\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(mm|cm|m|in|ft)?',
            'thickness': r'thickness\s+(?:of\s+)?(\d+(?:\.\d+)?)\s*(mm|cm|m|in)?',
            'position': r'at\s+\(?\s*(-?\d+(?:\.\d+)?)\s*,\s*(-?\d+(?:\.\d+)?)\s*,?\s*(-?\d+(?:\.\d+)?)?\s*\)?',
            'color': r'(red|green|blue|yellow|orange|purple|black|white|gray|grey)',
            'material': r'(steel|aluminum|copper|brass|plastic|wood|glass)',
            'count': r'(\d+)\s+(?:of|times|copies|instances)'
        }
    
    def _build_unit_conversions(self) -> Dict[str, float]:
        """Build unit conversion factors to mm"""
        return {
            'mm': 1.0,
            'cm': 10.0,
            'm': 1000.0,
            'in': 25.4,
            'inch': 25.4,
            'inches': 25.4,
            'ft': 304.8,
            'foot': 304.8,
            'feet': 304.8
        }
    
    def parse(self, prompt: str) -> ParsedCommand:
        """
        Parse a natural language prompt into a structured command
        
        Args:
            prompt: Natural language prompt
            
        Returns:
            ParsedCommand object with extracted information
        """
        logger.debug(f"Parsing prompt: {prompt}")
        
        # Normalize prompt
        prompt_lower = prompt.lower().strip()
        
        # Detect intent
        intent = self._detect_intent(prompt_lower)
        
        # Detect object type
        object_type = self._detect_object_type(prompt_lower)
        
        # Extract parameters
        parameters = self._extract_parameters(prompt_lower)
        
        # Extract modifiers
        modifiers = self._extract_modifiers(prompt_lower)
        
        # Calculate confidence
        confidence = self._calculate_confidence(intent, object_type, parameters)
        
        # Build command
        command = ParsedCommand(
            intent=intent,
            object_type=object_type,
            parameters=parameters,
            modifiers=modifiers,
            context=self.context.copy(),
            confidence=confidence,
            raw_prompt=prompt
        )
        
        # Update context for next command
        self._update_context(command)
        
        logger.info(f"Parsed command: intent={intent.value}, object={object_type.value if object_type else 'None'}, confidence={confidence:.2f}")
        
        return command
    
    def _detect_intent(self, prompt: str) -> CommandIntent:
        """Detect the primary intent of the command"""
        for intent, patterns in self.intent_patterns.items():
            for pattern in patterns:
                if re.search(pattern, prompt):
                    return intent
        return CommandIntent.UNKNOWN
    
    def _detect_object_type(self, prompt: str) -> Optional[ObjectType]:
        """Detect the type of object mentioned"""
        for obj_type, patterns in self.object_patterns.items():
            for pattern in patterns:
                if re.search(pattern, prompt):
                    return obj_type
        return None
    
    def _extract_parameters(self, prompt: str) -> Dict[str, Any]:
        """Extract numerical and other parameters from the prompt"""
        parameters = {}
        
        # Extract dimensions (looking for patterns like "100x50x25")
        dim_pattern = r'(\d+(?:\.\d+)?)\s*[xX×]\s*(\d+(?:\.\d+)?)\s*[xX×]?\s*(\d+(?:\.\d+)?)?'
        dim_match = re.search(dim_pattern, prompt)
        if dim_match:
            parameters['length'] = float(dim_match.group(1))
            parameters['width'] = float(dim_match.group(2))
            if dim_match.group(3):
                parameters['height'] = float(dim_match.group(3))
        
        # Extract individual parameters
        for param_name, pattern in self.parameter_patterns.items():
            match = re.search(pattern, prompt)
            if match:
                if param_name == 'position':
                    x = float(match.group(1))
                    y = float(match.group(2))
                    z = float(match.group(3)) if match.group(3) else 0
                    parameters['position'] = (x, y, z)
                elif param_name in ['dimension', 'radius', 'diameter', 'length', 'width', 'height', 'thickness']:
                    value = float(match.group(1))
                    unit = match.group(2) if len(match.groups()) > 1 and match.group(2) else 'mm'
                    # Convert to mm
                    value_mm = value * self.unit_conversions.get(unit, 1.0)
                    parameters[param_name] = value_mm
                elif param_name == 'angle':
                    value = float(match.group(1))
                    unit = match.group(2) if len(match.groups()) > 1 and match.group(2) else 'deg'
                    # Convert to degrees
                    if 'rad' in unit:
                        value = value * 180 / 3.14159
                    parameters[param_name] = value
                elif param_name == 'count':
                    parameters[param_name] = int(match.group(1))
                else:
                    parameters[param_name] = match.group(1)
        
        # Handle radius/diameter conversion
        if 'diameter' in parameters and 'radius' not in parameters:
            parameters['radius'] = parameters['diameter'] / 2
        
        # Extract object name if specified
        name_pattern = r'(?:called|named)\s+"?([^"]+)"?'
        name_match = re.search(name_pattern, prompt)
        if name_match:
            parameters['name'] = name_match.group(1)
        
        return parameters
    
    def _extract_modifiers(self, prompt: str) -> List[str]:
        """Extract modifier keywords from the prompt"""
        modifiers = []
        
        modifier_keywords = [
            'transparent', 'opaque', 'smooth', 'rough', 'textured',
            'centered', 'aligned', 'rotated', 'scaled', 'mirrored',
            'hollow', 'solid', 'filled', 'empty',
            'temporary', 'permanent', 'reference', 'construction'
        ]
        
        for keyword in modifier_keywords:
            if keyword in prompt:
                modifiers.append(keyword)
        
        return modifiers
    
    def _calculate_confidence(self, intent: CommandIntent, 
                            object_type: Optional[ObjectType],
                            parameters: Dict[str, Any]) -> float:
        """Calculate confidence score for the parsed command"""
        confidence = 0.0
        
        # Intent recognition confidence
        if intent != CommandIntent.UNKNOWN:
            confidence += 0.4
        
        # Object type recognition confidence
        if object_type and object_type != ObjectType.UNKNOWN:
            confidence += 0.3
        
        # Parameter extraction confidence
        if parameters:
            param_score = min(len(parameters) * 0.1, 0.3)
            confidence += param_score
        
        return min(confidence, 1.0)
    
    def _update_context(self, command: ParsedCommand) -> None:
        """Update context based on the parsed command"""
        self.context['last_intent'] = command.intent
        self.context['last_object_type'] = command.object_type
        if command.parameters.get('name'):
            self.context['last_object_name'] = command.parameters['name']
        self.context['command_count'] = self.context.get('command_count', 0) + 1
    
    def clear_context(self) -> None:
        """Clear the conversation context"""
        self.context = {}
        logger.debug("Context cleared")
    
    def get_suggestions(self, partial_prompt: str) -> List[str]:
        """
        Get autocomplete suggestions for a partial prompt
        
        Args:
            partial_prompt: Incomplete prompt text
            
        Returns:
            List of suggested completions
        """
        suggestions = []
        partial_lower = partial_prompt.lower().strip()
        
        # Suggest intents
        if len(partial_lower) < 10:
            intent_keywords = ['create', 'modify', 'delete', 'export', 'save', 'open']
            suggestions.extend([k for k in intent_keywords if k.startswith(partial_lower)])
        
        # Suggest object types
        if 'create' in partial_lower or 'make' in partial_lower:
            object_keywords = ['box', 'cylinder', 'sphere', 'sketch', 'hull', 'beam']
            for keyword in object_keywords:
                if keyword.startswith(partial_lower.split()[-1] if partial_lower.split() else ''):
                    suggestions.append(f"{partial_prompt} {keyword}")
        
        # Suggest dimension formats
        if any(obj in partial_lower for obj in ['box', 'cylinder', 'sphere']):
            suggestions.append(f"{partial_prompt} 100x50x25")
            suggestions.append(f"{partial_prompt} with radius 50")
            suggestions.append(f"{partial_prompt} at position (0, 0, 0)")
        
        return suggestions[:5]  # Limit to 5 suggestions