"""
Knowledge Integrator Module

Integrates analyzed OrcaFlex examples into the module agent's knowledge base.
"""

import json
import logging
from pathlib import Path
from typing import Dict, List, Any
from datetime import datetime

logger = logging.getLogger(__name__)


class KnowledgeIntegrator:
    """Integrates extracted knowledge into the OrcaFlex module agent."""
    
    def __init__(self, agent_dir: str = "agents/orcaflex"):
        """
        Initialize the integrator.
        
        Args:
            agent_dir: Directory of the OrcaFlex module agent
        """
        self.agent_dir = Path(agent_dir)
        self.context_dir = self.agent_dir / "context"
        self.knowledge_file = self.context_dir / "examples_knowledge.json"
        
        # Ensure directories exist
        self.context_dir.mkdir(parents=True, exist_ok=True)
    
    def load_existing_knowledge(self) -> Dict:
        """Load existing knowledge base if it exists."""
        if self.knowledge_file.exists():
            with open(self.knowledge_file, 'r') as f:
                return json.load(f)
        
        return {
            'last_updated': None,
            'examples': {},
            'patterns': {},
            'best_practices': [],
            'common_configurations': {}
        }
    
    def integrate_examples(self, analyses: List[Dict]) -> Dict:
        """
        Integrate analyzed examples into knowledge base.
        
        Args:
            analyses: List of analysis dictionaries from FeatureAnalyzer
            
        Returns:
            Updated knowledge base
        """
        knowledge = self.load_existing_knowledge()
        
        # Update timestamp
        knowledge['last_updated'] = datetime.now().isoformat()
        
        # Process each analysis
        for analysis in analyses:
            example_name = analysis['metadata']['name']
            
            # Store example data
            knowledge['examples'][example_name] = {
                'features': analysis,
                'added': datetime.now().isoformat()
            }
            
            # Extract patterns
            self._extract_patterns(analysis, knowledge['patterns'])
            
            # Extract common configurations
            self._extract_configurations(analysis, knowledge['common_configurations'])
        
        # Derive best practices
        knowledge['best_practices'] = self._derive_best_practices(knowledge)
        
        return knowledge
    
    def _extract_patterns(self, analysis: Dict, patterns: Dict):
        """Extract common patterns from analysis."""
        # Component combinations
        components = analysis['components']
        
        # Track vessel-line combinations
        if components['vessels']['count'] > 0 and components['lines']['count'] > 0:
            pattern_key = f"vessel_line_{components['vessels']['count']}_{components['lines']['count']}"
            patterns.setdefault('component_combinations', {})
            patterns['component_combinations'].setdefault(pattern_key, 0)
            patterns['component_combinations'][pattern_key] += 1
        
        # Track analysis combinations
        analysis_types = analysis['analysis']
        active_analyses = [k for k, v in analysis_types.items() if v]
        if active_analyses:
            pattern_key = '_'.join(sorted(active_analyses))
            patterns.setdefault('analysis_combinations', {})
            patterns['analysis_combinations'].setdefault(pattern_key, 0)
            patterns['analysis_combinations'][pattern_key] += 1
    
    def _extract_configurations(self, analysis: Dict, configurations: Dict):
        """Extract common configurations."""
        # Environment configurations
        env = analysis['environment']
        
        # Wave configurations
        if env['waves']['type']:
            configurations.setdefault('wave_types', {})
            wave_type = env['waves']['type']
            configurations['wave_types'].setdefault(wave_type, 0)
            configurations['wave_types'][wave_type] += 1
        
        # Current profiles
        if env['current']['profile']:
            configurations.setdefault('current_profiles', {})
            profile = env['current']['profile']
            configurations['current_profiles'].setdefault(profile, 0)
            configurations['current_profiles'][profile] += 1
    
    def _derive_best_practices(self, knowledge: Dict) -> List[str]:
        """Derive best practices from patterns and configurations."""
        practices = []
        
        # Analyze patterns for best practices
        if 'patterns' in knowledge:
            patterns = knowledge['patterns']
            
            # Component combination best practices
            if 'component_combinations' in patterns:
                most_common = max(patterns['component_combinations'].items(), 
                                key=lambda x: x[1], default=(None, 0))
                if most_common[0]:
                    practices.append(f"Common configuration: {most_common[0].replace('_', ' ')}")
            
            # Analysis combination best practices
            if 'analysis_combinations' in patterns:
                for combo, count in patterns['analysis_combinations'].items():
                    if count > 2:  # If pattern appears multiple times
                        analyses = combo.replace('_analysis', '').replace('_', ', ')
                        practices.append(f"Frequently combined analyses: {analyses}")
        
        # Configuration best practices
        if 'common_configurations' in knowledge:
            configs = knowledge['common_configurations']
            
            if 'wave_types' in configs:
                most_common_wave = max(configs['wave_types'].items(), 
                                     key=lambda x: x[1], default=(None, 0))
                if most_common_wave[0]:
                    practices.append(f"Most common wave type: {most_common_wave[0]}")
        
        return practices
    
    def save_knowledge(self, knowledge: Dict):
        """
        Save knowledge base to file.
        
        Args:
            knowledge: Knowledge dictionary to save
        """
        with open(self.knowledge_file, 'w') as f:
            json.dump(knowledge, f, indent=2)
        
        logger.info(f"Knowledge base saved to {self.knowledge_file}")
    
    def update_agent_context(self, knowledge: Dict):
        """
        Update the agent's context files with extracted knowledge.
        
        Args:
            knowledge: Knowledge dictionary
        """
        # Create a markdown summary for the agent
        summary_file = self.context_dir / "examples_summary.md"
        
        summary = [
            "# OrcaFlex Examples Knowledge Base",
            "",
            f"Last Updated: {knowledge['last_updated']}",
            f"Total Examples: {len(knowledge['examples'])}",
            "",
            "## Best Practices",
            ""
        ]
        
        for practice in knowledge['best_practices']:
            summary.append(f"- {practice}")
        
        summary.extend([
            "",
            "## Common Patterns",
            ""
        ])
        
        if 'patterns' in knowledge:
            for pattern_type, patterns in knowledge['patterns'].items():
                summary.append(f"### {pattern_type.replace('_', ' ').title()}")
                for pattern, count in sorted(patterns.items(), key=lambda x: x[1], reverse=True)[:5]:
                    summary.append(f"- {pattern}: {count} occurrences")
                summary.append("")
        
        summary.extend([
            "## Common Configurations",
            ""
        ])
        
        if 'common_configurations' in knowledge:
            for config_type, configs in knowledge['common_configurations'].items():
                summary.append(f"### {config_type.replace('_', ' ').title()}")
                for config, count in sorted(configs.items(), key=lambda x: x[1], reverse=True)[:5]:
                    summary.append(f"- {config}: {count} occurrences")
                summary.append("")
        
        # Save summary
        with open(summary_file, 'w') as f:
            f.write('\n'.join(summary))
        
        logger.info(f"Agent context updated at {summary_file}")
    
    def build_searchable_index(self, knowledge: Dict) -> Dict:
        """
        Build a searchable index of examples.
        
        Args:
            knowledge: Knowledge dictionary
            
        Returns:
            Searchable index dictionary
        """
        index = {
            'by_component': {},
            'by_analysis': {},
            'by_environment': {},
            'by_feature': {}
        }
        
        for example_name, example_data in knowledge['examples'].items():
            features = example_data['features']
            
            # Index by components
            for component_type in ['vessels', 'lines', 'buoys']:
                if features['components'][component_type].get('count', 0) > 0:
                    index['by_component'].setdefault(component_type, [])
                    index['by_component'][component_type].append(example_name)
            
            # Index by analysis type
            for analysis_type, is_active in features['analysis'].items():
                if is_active:
                    index['by_analysis'].setdefault(analysis_type, [])
                    index['by_analysis'][analysis_type].append(example_name)
            
            # Index by environment
            if features['environment']['waves']['type']:
                wave_type = features['environment']['waves']['type']
                index['by_environment'].setdefault(f'wave_{wave_type}', [])
                index['by_environment'][f'wave_{wave_type}'].append(example_name)
        
        return index
    
    def save_index(self, index: Dict):
        """Save searchable index."""
        index_file = self.context_dir / "examples_index.json"
        
        with open(index_file, 'w') as f:
            json.dump(index, f, indent=2)
        
        logger.info(f"Searchable index saved to {index_file}")