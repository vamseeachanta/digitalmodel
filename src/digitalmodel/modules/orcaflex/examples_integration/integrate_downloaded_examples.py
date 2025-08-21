#!/usr/bin/env python3
"""
Integrate Downloaded OrcaFlex Examples into Module Agent Knowledge Base

This script processes the 54 downloaded OrcaFlex examples and integrates them
into the module agent's knowledge base for reference and learning.
"""

import json
import logging
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class DownloadedExamplesIntegrator:
    """Process and integrate the downloaded OrcaFlex examples."""
    
    def __init__(self, base_dir: str = "docs/modules/orcaflex/examples"):
        """Initialize the integrator with downloaded examples."""
        self.base_dir = Path(base_dir)
        self.raw_dir = self.base_dir / "raw"
        self.metadata_dir = self.base_dir / "metadata"
        self.agent_dir = Path("agents/orcaflex")
        self.context_dir = self.agent_dir / "context"
        
        # Ensure agent context directory exists
        self.context_dir.mkdir(parents=True, exist_ok=True)
    
    def analyze_downloaded_examples(self) -> List[Dict]:
        """Analyze all downloaded examples and extract metadata."""
        analyses = []
        
        # Load the download manifest
        manifest_file = self.metadata_dir / "complete_manifest.json"
        if manifest_file.exists():
            with open(manifest_file, 'r') as f:
                manifest = json.load(f)
        else:
            logger.error(f"Manifest file not found: {manifest_file}")
            return analyses
        
        # Process examples from the manifest
        for example_name, example_info in manifest.get('examples', {}).items():
            # Parse category from the path (format: category/filename)
            if '/' in example_name:
                category, filename = example_name.split('/', 1)
            else:
                category = 'unknown'
                filename = example_name
            
            # Get category title from letter_categories if available
            category_title = 'Unknown'
            if 'letter_categories' in manifest:
                for letter, cat_info in manifest['letter_categories'].items():
                    if letter == category:
                        category_title = cat_info.get('title', 'Unknown')
                        break
            
            # Create file info from example info
            file_info = {
                'name': filename,
                'size': example_info.get('size', 0),
                'checksum': example_info.get('checksum', '')
            }
            
            analysis = self._analyze_example_file(
                category, 
                category_title,
                file_info
            )
            if analysis:
                analyses.append(analysis)
        
        logger.info(f"Analyzed {len(analyses)} examples")
        return analyses
    
    def _analyze_example_file(self, category: str, category_title: str, 
                              file_info: Dict) -> Dict:
        """Analyze a single example file and extract features."""
        file_path = self.raw_dir / category / file_info['name']
        
        # Basic metadata from file info
        analysis = {
            'metadata': {
                'name': file_info['name'],
                'category': category,
                'category_title': category_title,
                'file_type': file_path.suffix.lower(),
                'size_bytes': file_info.get('size', 0),
                'source': f"Orcina Examples Portal - Category {category.upper()}"
            },
            'components': self._infer_components(file_info['name'], category_title),
            'analysis': self._infer_analysis_type(file_info['name'], category_title),
            'environment': self._infer_environment(file_info['name'], category_title),
            'features': self._extract_features(file_info['name'], category_title)
        }
        
        return analysis
    
    def _infer_components(self, filename: str, category_title: str) -> Dict:
        """Infer components from filename and category."""
        components = {
            'vessels': {'count': 0, 'types': []},
            'lines': {'count': 0, 'types': []},
            'buoys': {'count': 0, 'types': []},
            'winches': {'count': 0},
            'constraints': {'count': 0}
        }
        
        # Lowercase for case-insensitive matching
        name_lower = filename.lower()
        category_lower = category_title.lower()
        
        # Vessel detection
        vessel_keywords = ['fpso', 'vessel', 'ship', 'tanker', 'barge', 'platform']
        if any(kw in name_lower or kw in category_lower for kw in vessel_keywords):
            components['vessels']['count'] = 1
            if 'fpso' in name_lower or 'fpso' in category_lower:
                components['vessels']['types'].append('FPSO')
            else:
                components['vessels']['types'].append('Vessel')
        
        # Line detection
        line_keywords = ['riser', 'mooring', 'line', 'cable', 'umbilical', 'catenary', 
                        'lazy', 'steep', 'pliant', 'tether']
        line_types = []
        if 'riser' in name_lower or 'riser' in category_lower:
            line_types.append('Riser')
        if 'mooring' in name_lower or 'mooring' in category_lower:
            line_types.append('Mooring')
        if 'catenary' in name_lower or 'catenary' in category_lower:
            line_types.append('Catenary')
        if 'lazy' in name_lower:
            line_types.append('Lazy Wave')
        if 'steep' in name_lower:
            line_types.append('Steep Wave')
        if 'pliant' in name_lower:
            line_types.append('Pliant Wave')
        
        if line_types:
            components['lines']['count'] = len(line_types)
            components['lines']['types'] = line_types
        elif any(kw in name_lower or kw in category_lower for kw in line_keywords):
            components['lines']['count'] = 1
            components['lines']['types'] = ['Line']
        
        # Buoy detection
        buoy_keywords = ['buoy', 'calm', 'arch', 'float']
        if any(kw in name_lower or kw in category_lower for kw in buoy_keywords):
            components['buoys']['count'] = 1
            if 'calm' in name_lower or 'calm' in category_lower:
                components['buoys']['types'].append('CALM Buoy')
            else:
                components['buoys']['types'].append('Buoy')
        
        # Winch detection
        if 'winch' in name_lower or 'lowering' in category_lower:
            components['winches']['count'] = 1
        
        return components
    
    def _infer_analysis_type(self, filename: str, category_title: str) -> Dict:
        """Infer analysis types from filename and category."""
        name_lower = filename.lower()
        category_lower = category_title.lower()
        combined = f"{name_lower} {category_lower}"
        
        return {
            'static_analysis': True,  # Most examples include statics
            'dynamic_analysis': 'dynamic' in combined or 'motion' in combined,
            'fatigue_analysis': 'fatigue' in combined or 'viv' in combined or 'shear7' in combined,
            'installation_analysis': 'install' in combined or 'lowering' in combined or 'pull' in combined,
            'viv_analysis': 'viv' in combined or 'shear7' in combined
        }
    
    def _infer_environment(self, filename: str, category_title: str) -> Dict:
        """Infer environment settings from filename and category."""
        name_lower = filename.lower()
        category_lower = category_title.lower()
        combined = f"{name_lower} {category_lower}"
        
        env = {
            'water_depth': None,
            'waves': {'type': None, 'height': None, 'period': None},
            'current': {'profile': None, 'speed': None},
            'wind': {'speed': None}
        }
        
        # Wave type inference
        if 'wave' in combined:
            if 'irregular' in combined:
                env['waves']['type'] = 'Irregular'
            elif 'regular' in combined:
                env['waves']['type'] = 'Regular'
            else:
                env['waves']['type'] = 'Wave'
        
        # Current inference
        if 'current' in combined:
            env['current']['profile'] = 'Profile'
        
        # Deep water indication
        if 'deep' in combined or 'deepwater' in combined:
            env['water_depth'] = 'Deep'
        
        return env
    
    def _extract_features(self, filename: str, category_title: str) -> List[str]:
        """Extract notable features from the example."""
        features = []
        name_lower = filename.lower()
        category_lower = category_title.lower()
        combined = f"{name_lower} {category_lower}"
        
        # Configuration types
        if 'lazy' in name_lower:
            features.append('Lazy wave configuration')
        if 'steep' in name_lower:
            features.append('Steep wave configuration')
        if 'pliant' in name_lower:
            features.append('Pliant wave configuration')
        if 'catenary' in name_lower:
            features.append('Catenary configuration')
        
        # Special features
        if 'turret' in combined:
            features.append('Turret mooring system')
        if 'disconnect' in combined:
            features.append('Disconnectable system')
        if 'shear7' in combined:
            features.append('SHEAR7 VIV analysis interface')
        if 'drag' in combined and 'amplif' in combined:
            features.append('Drag amplification modeling')
        if 'chinese lantern' in combined:
            features.append('Chinese lantern configuration')
        if 'fish farm' in combined or 'aquaculture' in combined:
            features.append('Aquaculture application')
        if 'drilling' in combined:
            features.append('Drilling operations')
        if 'bop' in combined:
            features.append('BOP handling')
        if 'j-tube' in combined or 'jtube' in combined:
            features.append('J-tube pull-in')
        if 'compensation' in combined:
            features.append('Heave compensation')
        if 'multibody' in combined or 'multi-body' in combined:
            features.append('Multibody dynamics')
        
        return features
    
    def create_knowledge_base(self, analyses: List[Dict]) -> Dict:
        """Create a comprehensive knowledge base from analyses."""
        knowledge = {
            'last_updated': datetime.now().isoformat(),
            'source': 'Orcina Examples Portal - Complete Download',
            'total_examples': len(analyses),
            'examples': {},
            'patterns': {},
            'best_practices': [],
            'common_configurations': {},
            'categories': {},
            'index': {
                'by_component': {},
                'by_analysis': {},
                'by_feature': {},
                'by_category': {}
            }
        }
        
        # Process each analysis
        for analysis in analyses:
            example_name = analysis['metadata']['name']
            category = analysis['metadata']['category']
            
            # Store example
            knowledge['examples'][example_name] = analysis
            
            # Build category index
            if category not in knowledge['categories']:
                knowledge['categories'][category] = {
                    'title': analysis['metadata']['category_title'],
                    'examples': []
                }
            knowledge['categories'][category]['examples'].append(example_name)
            
            # Build component index
            for comp_type, comp_data in analysis['components'].items():
                if comp_data.get('count', 0) > 0:
                    if comp_type not in knowledge['index']['by_component']:
                        knowledge['index']['by_component'][comp_type] = []
                    knowledge['index']['by_component'][comp_type].append(example_name)
            
            # Build analysis type index
            for analysis_type, is_active in analysis['analysis'].items():
                if is_active:
                    if analysis_type not in knowledge['index']['by_analysis']:
                        knowledge['index']['by_analysis'][analysis_type] = []
                    knowledge['index']['by_analysis'][analysis_type].append(example_name)
            
            # Build feature index
            for feature in analysis['features']:
                if feature not in knowledge['index']['by_feature']:
                    knowledge['index']['by_feature'][feature] = []
                knowledge['index']['by_feature'][feature].append(example_name)
        
        # Extract patterns and best practices
        knowledge['patterns'] = self._extract_patterns(analyses)
        knowledge['best_practices'] = self._derive_best_practices(knowledge)
        knowledge['common_configurations'] = self._extract_common_configs(analyses)
        
        return knowledge
    
    def _extract_patterns(self, analyses: List[Dict]) -> Dict:
        """Extract common patterns from all analyses."""
        patterns = {
            'component_combinations': {},
            'analysis_combinations': {},
            'configuration_types': {}
        }
        
        for analysis in analyses:
            # Component combinations
            components = []
            for comp_type, comp_data in analysis['components'].items():
                if comp_data.get('count', 0) > 0:
                    components.append(comp_type)
            
            if components:
                combo_key = '+'.join(sorted(components))
                patterns['component_combinations'][combo_key] = \
                    patterns['component_combinations'].get(combo_key, 0) + 1
            
            # Analysis combinations
            active_analyses = [k for k, v in analysis['analysis'].items() if v]
            if active_analyses:
                combo_key = '+'.join(sorted(active_analyses))
                patterns['analysis_combinations'][combo_key] = \
                    patterns['analysis_combinations'].get(combo_key, 0) + 1
            
            # Configuration types
            for feature in analysis['features']:
                if 'configuration' in feature.lower():
                    patterns['configuration_types'][feature] = \
                        patterns['configuration_types'].get(feature, 0) + 1
        
        return patterns
    
    def _extract_common_configs(self, analyses: List[Dict]) -> Dict:
        """Extract common configurations."""
        configs = {
            'riser_types': {},
            'mooring_types': {},
            'vessel_types': {},
            'analysis_types': {}
        }
        
        for analysis in analyses:
            # Riser configurations
            if 'lines' in analysis['components']:
                for line_type in analysis['components']['lines'].get('types', []):
                    if 'riser' in line_type.lower() or 'wave' in line_type.lower():
                        configs['riser_types'][line_type] = \
                            configs['riser_types'].get(line_type, 0) + 1
            
            # Vessel types
            if 'vessels' in analysis['components']:
                for vessel_type in analysis['components']['vessels'].get('types', []):
                    configs['vessel_types'][vessel_type] = \
                        configs['vessel_types'].get(vessel_type, 0) + 1
            
            # Analysis types
            for analysis_type, is_active in analysis['analysis'].items():
                if is_active:
                    configs['analysis_types'][analysis_type] = \
                        configs['analysis_types'].get(analysis_type, 0) + 1
        
        return configs
    
    def _derive_best_practices(self, knowledge: Dict) -> List[str]:
        """Derive best practices from the knowledge base."""
        practices = []
        
        # Most common component combinations
        if knowledge['patterns'].get('component_combinations'):
            top_combo = max(knowledge['patterns']['component_combinations'].items(),
                          key=lambda x: x[1])
            practices.append(f"Most common setup: {top_combo[0].replace('+', ' with ')}")
        
        # Most common analysis types
        if knowledge['common_configurations'].get('analysis_types'):
            top_analyses = sorted(knowledge['common_configurations']['analysis_types'].items(),
                                key=lambda x: x[1], reverse=True)[:3]
            practices.append(f"Primary analysis types: {', '.join([a[0].replace('_', ' ') for a in top_analyses])}")
        
        # Riser configurations
        if knowledge['common_configurations'].get('riser_types'):
            riser_types = list(knowledge['common_configurations']['riser_types'].keys())
            practices.append(f"Common riser configurations: {', '.join(riser_types)}")
        
        # Category insights
        num_categories = len(knowledge['categories'])
        practices.append(f"Examples cover {num_categories} distinct application categories")
        
        # Feature diversity
        num_features = len(knowledge['index']['by_feature'])
        practices.append(f"{num_features} unique features/configurations demonstrated")
        
        return practices
    
    def save_knowledge_base(self, knowledge: Dict):
        """Save the knowledge base to agent context."""
        # Save JSON knowledge base
        kb_file = self.context_dir / "examples_knowledge.json"
        with open(kb_file, 'w') as f:
            json.dump(knowledge, f, indent=2)
        logger.info(f"Knowledge base saved to {kb_file}")
        
        # Create markdown summary
        self._create_markdown_summary(knowledge)
        
        # Create searchable index
        index_file = self.context_dir / "examples_index.json"
        with open(index_file, 'w') as f:
            json.dump(knowledge['index'], f, indent=2)
        logger.info(f"Searchable index saved to {index_file}")
    
    def _create_markdown_summary(self, knowledge: Dict):
        """Create a markdown summary for the agent."""
        summary_file = self.context_dir / "examples_knowledge_summary.md"
        
        lines = [
            "# OrcaFlex Examples Knowledge Base",
            "",
            f"**Last Updated:** {knowledge['last_updated']}",
            f"**Total Examples:** {knowledge['total_examples']}",
            f"**Source:** {knowledge['source']}",
            "",
            "## Overview",
            "",
            "This knowledge base contains analyzed metadata from 54 official OrcaFlex examples ",
            "downloaded from the Orcina resources portal. The examples cover 13 categories ",
            "spanning various offshore engineering applications.",
            "",
            "## Categories",
            ""
        ]
        
        # List categories with example counts
        for cat_key in sorted(knowledge['categories'].keys()):
            cat_info = knowledge['categories'][cat_key]
            lines.append(f"- **{cat_key.upper()}**: {cat_info['title']} ({len(cat_info['examples'])} examples)")
        
        lines.extend([
            "",
            "## Best Practices Identified",
            ""
        ])
        
        for practice in knowledge['best_practices']:
            lines.append(f"- {practice}")
        
        lines.extend([
            "",
            "## Common Patterns",
            "",
            "### Component Combinations",
            ""
        ])
        
        # Top 5 component combinations
        top_combos = sorted(knowledge['patterns']['component_combinations'].items(),
                          key=lambda x: x[1], reverse=True)[:5]
        for combo, count in top_combos:
            lines.append(f"- {combo.replace('+', ' + ')}: {count} examples")
        
        lines.extend([
            "",
            "### Analysis Types",
            ""
        ])
        
        # Analysis type distribution
        if knowledge['common_configurations'].get('analysis_types'):
            for analysis_type, count in sorted(
                knowledge['common_configurations']['analysis_types'].items(),
                key=lambda x: x[1], reverse=True
            ):
                lines.append(f"- {analysis_type.replace('_', ' ').title()}: {count} examples")
        
        lines.extend([
            "",
            "## Configuration Types",
            "",
            "### Riser Configurations",
            ""
        ])
        
        if knowledge['common_configurations'].get('riser_types'):
            for riser_type, count in knowledge['common_configurations']['riser_types'].items():
                lines.append(f"- {riser_type}: {count} examples")
        
        lines.extend([
            "",
            "### Vessel Types",
            ""
        ])
        
        if knowledge['common_configurations'].get('vessel_types'):
            for vessel_type, count in knowledge['common_configurations']['vessel_types'].items():
                lines.append(f"- {vessel_type}: {count} examples")
        
        lines.extend([
            "",
            "## Unique Features Demonstrated",
            ""
        ])
        
        # List unique features
        for feature in sorted(knowledge['index']['by_feature'].keys()):
            example_count = len(knowledge['index']['by_feature'][feature])
            lines.append(f"- {feature} ({example_count} examples)")
        
        lines.extend([
            "",
            "## Usage",
            "",
            "This knowledge base can be queried to find relevant examples for specific:",
            "- Component types (vessels, lines, buoys)",
            "- Analysis types (static, dynamic, fatigue, VIV)",
            "- Configurations (lazy wave, steep wave, catenary)",
            "- Applications (drilling, installation, aquaculture)",
            "",
            "Use the `examples_index.json` for programmatic access to find examples by criteria.",
            "",
            "---",
            f"*Generated from downloaded OrcaFlex examples on {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}*"
        ])
        
        with open(summary_file, 'w') as f:
            f.write('\n'.join(lines))
        
        logger.info(f"Markdown summary saved to {summary_file}")
    
    def run(self):
        """Run the complete integration process."""
        logger.info("Starting OrcaFlex examples integration...")
        
        # Analyze downloaded examples
        logger.info("Analyzing downloaded examples...")
        analyses = self.analyze_downloaded_examples()
        
        if not analyses:
            logger.error("No examples found to analyze")
            return
        
        # Create knowledge base
        logger.info("Creating knowledge base...")
        knowledge = self.create_knowledge_base(analyses)
        
        # Save to agent context
        logger.info("Saving knowledge base to agent context...")
        self.save_knowledge_base(knowledge)
        
        # Print summary
        print("\n" + "="*60)
        print("OrcaFlex Examples Integration Complete!")
        print("="*60)
        print(f"✅ Processed {len(analyses)} examples")
        print(f"✅ Identified {len(knowledge['categories'])} categories")
        print(f"✅ Extracted {len(knowledge['index']['by_feature'])} unique features")
        print(f"✅ Knowledge base saved to: agents/orcaflex/context/")
        print("\nFiles created:")
        print("  - examples_knowledge.json (complete knowledge base)")
        print("  - examples_knowledge_summary.md (human-readable summary)")
        print("  - examples_index.json (searchable index)")
        print("\nThe OrcaFlex module agent now has access to:")
        for practice in knowledge['best_practices'][:3]:
            print(f"  • {practice}")
        print("\n" + "="*60)


if __name__ == "__main__":
    integrator = DownloadedExamplesIntegrator()
    integrator.run()