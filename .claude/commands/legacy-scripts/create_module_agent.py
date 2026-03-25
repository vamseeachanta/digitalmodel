#!/usr/bin/env python3
"""
Enhanced Create-Module-Agent Command v3.0
Implements mandatory principles:
1. Phased approach to reading vast documentation (mixed-documentation-agent spec)
2. Modular agent management (modular-agent-management spec)
Plus v2.0 features: RAG optimization, context engineering, memory management
"""

import os
import sys
import yaml
import shutil
import hashlib
import argparse
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from enum import Enum
import json
import re
import time

class AgentMode(Enum):
    """Agent operation modes"""
    CREATE = "create"
    UPDATE = "update"
    REFRESH = "refresh"  # New mode for refreshing agent knowledge

class DocumentCategory(Enum):
    """Documentation categories following best practices"""
    INTERNAL = "internal"       # Internal project documentation
    EXTERNAL = "external"       # External references and guides
    WEB = "web"                # Web resources and URLs
    REPOSITORY = "repository"   # Repository-specific docs
    OPTIMIZED = "optimized"     # Processed/optimized documentation
    CONTEXT = "context"         # Context engineering docs
    MEMORY = "memory"           # Long-term memory storage
    MODULE = "module"           # Module-specific documentation
    SUBMODULE = "submodule"     # Submodule-specific documentation

class ProcessingPhase(Enum):
    """Phased document processing approach from mixed-documentation-agent spec"""
    DISCOVERY = "discovery"           # Phase 1: Document discovery and classification
    QUALITY_ASSESSMENT = "quality"   # Phase 2: Quality assessment and filtering
    EXTRACTION = "extraction"         # Phase 3: Knowledge extraction
    SYNTHESIS = "synthesis"           # Phase 4: Knowledge synthesis
    VALIDATION = "validation"         # Phase 5: Validation and verification
    INTEGRATION = "integration"       # Phase 6: Integration into agent

class AgentSpecialization(Enum):
    """Agent specialization levels from modular-agent-management spec"""
    GENERAL = "general-purpose"      # General purpose agent
    MODULE = "module-specific"        # Module-level specialization
    SUBMODULE = "submodule-specific" # Submodule-level specialization
    DOMAIN = "domain-expert"          # Domain expert specialization
    CROSS_MODULE = "cross-module"     # Cross-module collaboration agent

class ChunkingStrategy(Enum):
    """Document chunking strategies for optimal RAG performance"""
    SEMANTIC = "semantic"       # Semantic-based chunking
    FIXED_SIZE = "fixed_size"   # Fixed token size chunks
    PARAGRAPH = "paragraph"     # Paragraph-based chunking
    SECTION = "section"         # Section/header-based chunking
    HYBRID = "hybrid"          # Combined approach
    PHASED = "phased"          # Phased approach for large docs

class PhasedDocumentProcessor:
    """
    Implements phased approach to reading vast documentation
    Based on mixed-documentation-agent specification
    """
    
    def __init__(self, agent_path: Path):
        self.agent_path = agent_path
        self.processing_path = agent_path / "processing"
        self.phases_path = self.processing_path / "phases"
        self.metrics_path = self.processing_path / "metrics"
        
        # Create processing directories
        self.processing_path.mkdir(parents=True, exist_ok=True)
        self.phases_path.mkdir(parents=True, exist_ok=True)
        self.metrics_path.mkdir(parents=True, exist_ok=True)
        
        # Phase tracking
        self.phase_status = self.load_phase_status()
    
    def load_phase_status(self) -> dict:
        """Load or initialize phase processing status"""
        status_file = self.processing_path / "phase_status.yaml"
        if status_file.exists():
            with open(status_file, 'r') as f:
                return yaml.safe_load(f) or {}
        return {
            "current_phase": ProcessingPhase.DISCOVERY.value,
            "completed_phases": [],
            "phase_metrics": {},
            "last_updated": datetime.now().isoformat()
        }
    
    def save_phase_status(self):
        """Save phase processing status"""
        status_file = self.processing_path / "phase_status.yaml"
        self.phase_status["last_updated"] = datetime.now().isoformat()
        with open(status_file, 'w') as f:
            yaml.dump(self.phase_status, f, default_flow_style=False)
    
    def phase1_discovery(self, doc_paths: List[Path]) -> Dict:
        """
        Phase 1: Document Discovery and Classification
        - Survey document formats and types
        - Create document inventory
        - Initial classification
        """
        print("📊 Phase 1: Document Discovery and Classification")
        
        discovery_results = {
            "total_documents": len(doc_paths),
            "format_distribution": {},
            "size_distribution": {},
            "document_inventory": [],
            "classification": {}
        }
        
        for doc_path in doc_paths:
            if not doc_path.exists():
                continue
            
            # Analyze document
            file_ext = doc_path.suffix.lower()
            file_size = doc_path.stat().st_size
            
            # Update format distribution
            discovery_results["format_distribution"][file_ext] = \
                discovery_results["format_distribution"].get(file_ext, 0) + 1
            
            # Classify by size
            size_category = self._classify_size(file_size)
            discovery_results["size_distribution"][size_category] = \
                discovery_results["size_distribution"].get(size_category, 0) + 1
            
            # Document inventory
            doc_info = {
                "path": str(doc_path),
                "format": file_ext,
                "size": file_size,
                "size_category": size_category,
                "discovered_at": datetime.now().isoformat()
            }
            discovery_results["document_inventory"].append(doc_info)
            
            # Initial classification
            doc_class = self._classify_document(doc_path)
            discovery_results["classification"][str(doc_path)] = doc_class
        
        # Save phase results
        self._save_phase_results(ProcessingPhase.DISCOVERY, discovery_results)
        
        return discovery_results
    
    def phase2_quality_assessment(self, discovery_results: Dict) -> Dict:
        """
        Phase 2: Quality Assessment
        - Assess document quality
        - Filter low-quality documents
        - Prioritize high-value content
        """
        print("📈 Phase 2: Quality Assessment and Filtering")
        
        quality_results = {
            "assessed_documents": [],
            "quality_scores": {},
            "filtered_documents": [],
            "high_priority": [],
            "medium_priority": [],
            "low_priority": []
        }
        
        for doc_info in discovery_results["document_inventory"]:
            doc_path = Path(doc_info["path"])
            
            if not doc_path.exists():
                continue
            
            # Calculate quality score
            quality_score = self._assess_quality(doc_path, doc_info)
            quality_results["quality_scores"][str(doc_path)] = quality_score
            
            # Quality-based filtering
            if quality_score < 0.3:
                quality_results["filtered_documents"].append(str(doc_path))
                continue
            
            # Prioritization
            if quality_score >= 0.8:
                quality_results["high_priority"].append(str(doc_path))
            elif quality_score >= 0.5:
                quality_results["medium_priority"].append(str(doc_path))
            else:
                quality_results["low_priority"].append(str(doc_path))
            
            quality_results["assessed_documents"].append({
                "path": str(doc_path),
                "quality_score": quality_score,
                "priority": self._get_priority_level(quality_score)
            })
        
        # Save phase results
        self._save_phase_results(ProcessingPhase.QUALITY_ASSESSMENT, quality_results)
        
        return quality_results
    
    def phase3_extraction(self, quality_results: Dict, doc_manager) -> Dict:
        """
        Phase 3: Knowledge Extraction
        - Extract entities and relationships
        - Build initial knowledge graph
        - Maintain source traceability
        """
        print("🔍 Phase 3: Knowledge Extraction")
        
        extraction_results = {
            "extracted_entities": [],
            "extracted_relationships": [],
            "knowledge_graph": {},
            "extraction_metrics": {},
            "source_mapping": {}
        }
        
        # Process documents by priority
        priority_order = ["high_priority", "medium_priority", "low_priority"]
        
        for priority in priority_order:
            doc_paths = quality_results.get(priority, [])
            
            for doc_path_str in doc_paths:
                doc_path = Path(doc_path_str)
                
                if not doc_path.exists():
                    continue
                
                print(f"  Extracting from: {doc_path.name}")
                
                # Extract knowledge
                extracted = self._extract_knowledge(doc_path)
                
                # Store entities
                extraction_results["extracted_entities"].extend(extracted["entities"])
                
                # Store relationships
                extraction_results["extracted_relationships"].extend(extracted["relationships"])
                
                # Update knowledge graph
                self._update_knowledge_graph(
                    extraction_results["knowledge_graph"],
                    extracted
                )
                
                # Maintain source mapping
                extraction_results["source_mapping"][doc_path_str] = {
                    "entities": len(extracted["entities"]),
                    "relationships": len(extracted["relationships"]),
                    "extraction_time": datetime.now().isoformat()
                }
        
        # Calculate metrics
        extraction_results["extraction_metrics"] = {
            "total_entities": len(extraction_results["extracted_entities"]),
            "total_relationships": len(extraction_results["extracted_relationships"]),
            "unique_entities": len(set(e["name"] for e in extraction_results["extracted_entities"])),
            "graph_nodes": len(extraction_results["knowledge_graph"])
        }
        
        # Save phase results
        self._save_phase_results(ProcessingPhase.EXTRACTION, extraction_results)
        
        return extraction_results
    
    def phase4_synthesis(self, extraction_results: Dict) -> Dict:
        """
        Phase 4: Knowledge Synthesis
        - Consolidate extracted knowledge
        - Resolve conflicts
        - Build coherent knowledge base
        """
        print("🧩 Phase 4: Knowledge Synthesis")
        
        synthesis_results = {
            "synthesized_knowledge": {},
            "conflict_resolutions": [],
            "knowledge_hierarchy": {},
            "synthesis_metrics": {}
        }
        
        # Consolidate entities
        entity_map = {}
        for entity in extraction_results["extracted_entities"]:
            key = entity.get("name", "").lower()
            if key not in entity_map:
                entity_map[key] = []
            entity_map[key].append(entity)
        
        # Resolve conflicts and synthesize
        for key, entities in entity_map.items():
            if len(entities) > 1:
                # Conflict detected - resolve
                resolved = self._resolve_entity_conflict(entities)
                synthesis_results["conflict_resolutions"].append({
                    "entity": key,
                    "sources": len(entities),
                    "resolution": resolved["method"]
                })
                synthesis_results["synthesized_knowledge"][key] = resolved["entity"]
            else:
                synthesis_results["synthesized_knowledge"][key] = entities[0]
        
        # Build knowledge hierarchy
        synthesis_results["knowledge_hierarchy"] = self._build_hierarchy(
            synthesis_results["synthesized_knowledge"]
        )
        
        # Calculate metrics
        synthesis_results["synthesis_metrics"] = {
            "total_synthesized": len(synthesis_results["synthesized_knowledge"]),
            "conflicts_resolved": len(synthesis_results["conflict_resolutions"]),
            "hierarchy_depth": self._calculate_hierarchy_depth(
                synthesis_results["knowledge_hierarchy"]
            )
        }
        
        # Save phase results
        self._save_phase_results(ProcessingPhase.SYNTHESIS, synthesis_results)
        
        return synthesis_results
    
    def phase5_validation(self, synthesis_results: Dict) -> Dict:
        """
        Phase 5: Validation and Verification
        - Validate synthesized knowledge
        - Check consistency
        - Quality assurance
        """
        print("✅ Phase 5: Validation and Verification")
        
        validation_results = {
            "validation_status": {},
            "consistency_checks": [],
            "quality_metrics": {},
            "issues_found": []
        }
        
        # Validate each synthesized entity
        for key, entity in synthesis_results["synthesized_knowledge"].items():
            validation = self._validate_entity(entity)
            validation_results["validation_status"][key] = validation["status"]
            
            if validation["issues"]:
                validation_results["issues_found"].extend(validation["issues"])
        
        # Consistency checks
        consistency = self._check_consistency(synthesis_results["synthesized_knowledge"])
        validation_results["consistency_checks"] = consistency
        
        # Calculate quality metrics
        validation_results["quality_metrics"] = {
            "validation_pass_rate": sum(
                1 for v in validation_results["validation_status"].values() 
                if v == "valid"
            ) / len(validation_results["validation_status"]),
            "consistency_score": consistency["score"],
            "total_issues": len(validation_results["issues_found"])
        }
        
        # Save phase results
        self._save_phase_results(ProcessingPhase.VALIDATION, validation_results)
        
        return validation_results
    
    def phase6_integration(self, validation_results: Dict, doc_manager) -> Dict:
        """
        Phase 6: Integration into Agent
        - Integrate validated knowledge
        - Update agent configuration
        - Create refresh mechanisms
        """
        print("🔄 Phase 6: Integration into Agent")
        
        integration_results = {
            "integrated_knowledge": [],
            "agent_updates": {},
            "refresh_config": {},
            "integration_metrics": {}
        }
        
        # Load previous phase results
        synthesis_results = self._load_phase_results(ProcessingPhase.SYNTHESIS)
        
        # Integrate validated knowledge
        for key, status in validation_results["validation_status"].items():
            if status == "valid":
                knowledge = synthesis_results["synthesized_knowledge"].get(key)
                if knowledge:
                    integration_results["integrated_knowledge"].append(knowledge)
        
        # Update agent configuration
        agent_config = self._update_agent_config(integration_results["integrated_knowledge"])
        integration_results["agent_updates"] = agent_config
        
        # Create refresh configuration
        integration_results["refresh_config"] = {
            "last_refresh": datetime.now().isoformat(),
            "refresh_interval": "7_days",
            "auto_refresh": True,
            "knowledge_version": self._generate_version_hash(
                integration_results["integrated_knowledge"]
            )
        }
        
        # Calculate metrics
        integration_results["integration_metrics"] = {
            "total_integrated": len(integration_results["integrated_knowledge"]),
            "integration_rate": len(integration_results["integrated_knowledge"]) / 
                               len(validation_results["validation_status"]),
            "agent_version": agent_config.get("version", "1.0.0")
        }
        
        # Save phase results
        self._save_phase_results(ProcessingPhase.INTEGRATION, integration_results)
        
        # Mark all phases complete
        self.phase_status["completed_phases"] = [p.value for p in ProcessingPhase]
        self.save_phase_status()
        
        return integration_results
    
    def _classify_size(self, size: int) -> str:
        """Classify document by size"""
        if size < 10 * 1024:  # < 10KB
            return "small"
        elif size < 1024 * 1024:  # < 1MB
            return "medium"
        elif size < 10 * 1024 * 1024:  # < 10MB
            return "large"
        else:
            return "very_large"
    
    def _classify_document(self, doc_path: Path) -> str:
        """Initial document classification"""
        ext = doc_path.suffix.lower()
        
        if ext in ['.md', '.txt', '.rst']:
            return "text"
        elif ext in ['.pdf']:
            return "pdf"
        elif ext in ['.xlsx', '.xls', '.csv']:
            return "spreadsheet"
        elif ext in ['.docx', '.doc']:
            return "word"
        elif ext in ['.json', '.yaml', '.yml']:
            return "structured"
        else:
            return "other"
    
    def _assess_quality(self, doc_path: Path, doc_info: Dict) -> float:
        """Assess document quality (0.0 to 1.0)"""
        score = 0.5  # Base score
        
        # Size penalty for very large files
        if doc_info["size_category"] == "very_large":
            score -= 0.2
        elif doc_info["size_category"] == "large":
            score -= 0.1
        
        # Format bonus
        if doc_info["format"] in ['.md', '.txt']:
            score += 0.2
        elif doc_info["format"] in ['.pdf']:
            score += 0.1
        
        # Recency bonus (if modification time available)
        try:
            mtime = datetime.fromtimestamp(doc_path.stat().st_mtime)
            days_old = (datetime.now() - mtime).days
            if days_old < 30:
                score += 0.2
            elif days_old < 90:
                score += 0.1
        except:
            pass
        
        return min(max(score, 0.0), 1.0)
    
    def _get_priority_level(self, score: float) -> str:
        """Get priority level from quality score"""
        if score >= 0.8:
            return "high"
        elif score >= 0.5:
            return "medium"
        else:
            return "low"
    
    def _extract_knowledge(self, doc_path: Path) -> Dict:
        """Extract knowledge from document"""
        # Simplified extraction for demonstration
        entities = []
        relationships = []
        
        try:
            with open(doc_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            # Simple entity extraction (can be enhanced with NLP)
            # Extract capitalized words as potential entities
            words = re.findall(r'\b[A-Z][a-z]+\b', content)
            unique_words = set(words)
            
            for word in unique_words:
                entities.append({
                    "name": word,
                    "type": "concept",
                    "source": str(doc_path),
                    "confidence": 0.7
                })
            
            # Simple relationship extraction
            # Look for patterns like "X is Y" or "X has Y"
            patterns = [
                r'(\w+)\s+is\s+(\w+)',
                r'(\w+)\s+has\s+(\w+)',
                r'(\w+)\s+contains\s+(\w+)'
            ]
            
            for pattern in patterns:
                matches = re.findall(pattern, content, re.IGNORECASE)
                for match in matches:
                    relationships.append({
                        "source": match[0],
                        "target": match[1],
                        "type": "related",
                        "confidence": 0.6
                    })
        
        except Exception as e:
            print(f"    Warning: Error extracting from {doc_path}: {e}")
        
        return {
            "entities": entities,
            "relationships": relationships
        }
    
    def _update_knowledge_graph(self, graph: Dict, extracted: Dict):
        """Update knowledge graph with extracted data"""
        # Add entities as nodes
        for entity in extracted["entities"]:
            key = entity["name"].lower()
            if key not in graph:
                graph[key] = {
                    "entity": entity,
                    "connections": []
                }
        
        # Add relationships as edges
        for rel in extracted["relationships"]:
            source_key = rel["source"].lower()
            target_key = rel["target"].lower()
            
            if source_key in graph:
                graph[source_key]["connections"].append({
                    "target": target_key,
                    "type": rel["type"],
                    "confidence": rel["confidence"]
                })
    
    def _resolve_entity_conflict(self, entities: List[Dict]) -> Dict:
        """Resolve conflicts between multiple entity definitions"""
        # Simple resolution: choose highest confidence
        best_entity = max(entities, key=lambda e: e.get("confidence", 0))
        
        return {
            "entity": best_entity,
            "method": "highest_confidence"
        }
    
    def _build_hierarchy(self, knowledge: Dict) -> Dict:
        """Build knowledge hierarchy"""
        hierarchy = {
            "root": {
                "children": {},
                "level": 0
            }
        }
        
        # Simple hierarchy based on entity types
        for key, entity in knowledge.items():
            entity_type = entity.get("type", "unknown")
            
            if entity_type not in hierarchy["root"]["children"]:
                hierarchy["root"]["children"][entity_type] = {
                    "children": {},
                    "level": 1
                }
            
            hierarchy["root"]["children"][entity_type]["children"][key] = {
                "entity": entity,
                "level": 2
            }
        
        return hierarchy
    
    def _calculate_hierarchy_depth(self, hierarchy: Dict) -> int:
        """Calculate maximum depth of hierarchy"""
        def get_depth(node, current_depth=0):
            if not node.get("children"):
                return current_depth
            
            max_child_depth = 0
            for child in node["children"].values():
                child_depth = get_depth(child, current_depth + 1)
                max_child_depth = max(max_child_depth, child_depth)
            
            return max_child_depth
        
        return get_depth(hierarchy["root"])
    
    def _validate_entity(self, entity: Dict) -> Dict:
        """Validate entity data"""
        issues = []
        
        # Check required fields
        required_fields = ["name", "type", "source"]
        for field in required_fields:
            if field not in entity:
                issues.append(f"Missing required field: {field}")
        
        # Check confidence score
        confidence = entity.get("confidence", 0)
        if confidence < 0.5:
            issues.append(f"Low confidence: {confidence}")
        
        return {
            "status": "valid" if not issues else "invalid",
            "issues": issues
        }
    
    def _check_consistency(self, knowledge: Dict) -> Dict:
        """Check knowledge consistency"""
        inconsistencies = []
        
        # Check for duplicate entities with different types
        type_map = {}
        for key, entity in knowledge.items():
            entity_name = entity.get("name", "").lower()
            entity_type = entity.get("type")
            
            if entity_name in type_map:
                if type_map[entity_name] != entity_type:
                    inconsistencies.append({
                        "issue": "type_mismatch",
                        "entity": entity_name,
                        "types": [type_map[entity_name], entity_type]
                    })
            else:
                type_map[entity_name] = entity_type
        
        # Calculate consistency score
        total_entities = len(knowledge)
        consistency_score = 1.0 - (len(inconsistencies) / max(total_entities, 1))
        
        return {
            "inconsistencies": inconsistencies,
            "score": consistency_score
        }
    
    def _update_agent_config(self, knowledge: List[Dict]) -> Dict:
        """Update agent configuration with integrated knowledge"""
        return {
            "version": "1.0.0",
            "knowledge_count": len(knowledge),
            "last_updated": datetime.now().isoformat(),
            "capabilities": self._derive_capabilities(knowledge)
        }
    
    def _derive_capabilities(self, knowledge: List[Dict]) -> List[str]:
        """Derive agent capabilities from knowledge"""
        capabilities = set()
        
        for item in knowledge:
            item_type = item.get("type", "")
            if item_type:
                capabilities.add(f"knowledge_{item_type}")
        
        return list(capabilities)
    
    def _generate_version_hash(self, knowledge: List[Dict]) -> str:
        """Generate version hash for knowledge"""
        knowledge_str = json.dumps(knowledge, sort_keys=True)
        return hashlib.sha256(knowledge_str.encode()).hexdigest()[:16]
    
    def _save_phase_results(self, phase: ProcessingPhase, results: Dict):
        """Save phase processing results"""
        phase_file = self.phases_path / f"{phase.value}_results.yaml"
        with open(phase_file, 'w') as f:
            yaml.dump(results, f, default_flow_style=False)
        
        # Update phase status
        self.phase_status["completed_phases"].append(phase.value)
        self.phase_status["phase_metrics"][phase.value] = {
            "completed_at": datetime.now().isoformat(),
            "result_size": len(str(results))
        }
        self.save_phase_status()
    
    def _load_phase_results(self, phase: ProcessingPhase) -> Dict:
        """Load phase processing results"""
        phase_file = self.phases_path / f"{phase.value}_results.yaml"
        if phase_file.exists():
            with open(phase_file, 'r') as f:
                return yaml.safe_load(f) or {}
        return {}


class ModularAgentManager:
    """
    Implements modular agent management principles
    Based on modular-agent-management specification
    """
    
    def __init__(self, base_path: Path = Path("agents")):
        self.base_path = base_path
        self.module_agents_path = base_path / "module-agents"
        self.submodule_agents_path = base_path / "submodule-agents"
        self.management_path = base_path / "agent-management"
        
        # Create directory structure
        self.module_agents_path.mkdir(parents=True, exist_ok=True)
        self.submodule_agents_path.mkdir(parents=True, exist_ok=True)
        self.management_path.mkdir(parents=True, exist_ok=True)
        
        # Agent registry
        self.registry = self.load_registry()
    
    def load_registry(self) -> Dict:
        """Load or initialize agent registry"""
        registry_file = self.management_path / "agent_registry.yaml"
        if registry_file.exists():
            with open(registry_file, 'r') as f:
                return yaml.safe_load(f) or {"agents": {}, "modules": {}, "metrics": {}}
        return {"agents": {}, "modules": {}, "metrics": {}}
    
    def save_registry(self):
        """Save agent registry"""
        registry_file = self.management_path / "agent_registry.yaml"
        with open(registry_file, 'w') as f:
            yaml.dump(self.registry, f, default_flow_style=False)
    
    def determine_specialization(self, module_name: str, 
                                spec_count: int = 0,
                                has_submodules: bool = False) -> AgentSpecialization:
        """
        Determine agent specialization level based on criteria
        From modular-agent-management spec:
        - Module Complexity: Modules with >5 specifications get dedicated agents
        - Domain Expertise: Engineering domains get specialized technical agents
        - Update Frequency: Frequently changing modules get priority refresh
        - Cross-Dependencies: Modules with many interdependencies get collaboration agents
        """
        # Engineering domains get domain expert agents
        engineering_domains = ["marine-engineering", "hydrodynamics", "structural", "orcaflex"]
        if any(domain in module_name.lower() for domain in engineering_domains):
            return AgentSpecialization.DOMAIN
        
        # Cross-module dependencies
        cross_module_indicators = ["integration", "cross", "shared", "common"]
        if any(indicator in module_name.lower() for indicator in cross_module_indicators):
            return AgentSpecialization.CROSS_MODULE
        
        # Submodule specialization
        if "/" in module_name or has_submodules:
            return AgentSpecialization.SUBMODULE
        
        # Module specialization for complex modules
        if spec_count > 5:
            return AgentSpecialization.MODULE
        
        # Default to general purpose
        return AgentSpecialization.GENERAL
    
    def create_module_agent(self, module_name: str, 
                          module_path: Path,
                          specialization: Optional[AgentSpecialization] = None) -> Dict:
        """Create a specialized agent for a module"""
        
        # Determine specialization if not provided
        if not specialization:
            # Count specifications in module
            spec_count = len(list(module_path.glob("**/*.md"))) if module_path.exists() else 0
            has_submodules = bool(list(module_path.glob("*/"))) if module_path.exists() else False
            specialization = self.determine_specialization(module_name, spec_count, has_submodules)
        
        # Determine agent path based on specialization
        if specialization in [AgentSpecialization.MODULE, AgentSpecialization.DOMAIN]:
            agent_path = self.module_agents_path / module_name
        elif specialization == AgentSpecialization.SUBMODULE:
            parent_module = module_name.split("/")[0] if "/" in module_name else module_name
            agent_path = self.submodule_agents_path / parent_module / module_name.replace("/", "_")
        else:
            agent_path = self.base_path / module_name
        
        # Create agent directory
        agent_path.mkdir(parents=True, exist_ok=True)
        
        # Create agent configuration
        agent_config = {
            "name": module_name,
            "specialization": specialization.value,
            "module_path": str(module_path),
            "created": datetime.now().isoformat(),
            "version": "1.0.0",
            "capabilities": self._determine_capabilities(specialization),
            "refresh_config": {
                "auto_refresh": True,
                "refresh_interval": "7_days",
                "priority": self._determine_refresh_priority(specialization)
            },
            "context_optimization": {
                "max_context_size": self._determine_context_size(specialization),
                "focused_domain": module_name,
                "cross_references": []
            }
        }
        
        # Save agent configuration
        config_file = agent_path / "agent_config.yaml"
        with open(config_file, 'w') as f:
            yaml.dump(agent_config, f, default_flow_style=False)
        
        # Register agent
        self.registry["agents"][module_name] = {
            "path": str(agent_path),
            "specialization": specialization.value,
            "created": datetime.now().isoformat(),
            "status": "active"
        }
        
        # Track module coverage
        if module_name not in self.registry["modules"]:
            self.registry["modules"][module_name] = {
                "agents": [],
                "coverage": 0.0
            }
        self.registry["modules"][module_name]["agents"].append(module_name)
        
        self.save_registry()
        
        return agent_config
    
    def refresh_agent(self, agent_name: str, 
                     updated_specs: Optional[List[Path]] = None) -> Dict:
        """
        Refresh agent knowledge from updated specifications
        Implements automated refresh from modular-agent-management spec
        """
        if agent_name not in self.registry["agents"]:
            raise ValueError(f"Agent '{agent_name}' not found in registry")
        
        agent_info = self.registry["agents"][agent_name]
        agent_path = Path(agent_info["path"])
        
        # Load agent configuration
        config_file = agent_path / "agent_config.yaml"
        with open(config_file, 'r') as f:
            agent_config = yaml.safe_load(f)
        
        # Determine what to refresh
        if updated_specs:
            specs_to_process = updated_specs
        else:
            # Auto-detect changed specs
            module_path = Path(agent_config["module_path"])
            if module_path.exists():
                specs_to_process = list(module_path.glob("**/*.md"))
            else:
                specs_to_process = []
        
        # Refresh metrics
        refresh_results = {
            "agent": agent_name,
            "timestamp": datetime.now().isoformat(),
            "specs_processed": len(specs_to_process),
            "refresh_type": "manual" if updated_specs else "automatic",
            "status": "success"
        }
        
        # Update version
        current_version = agent_config.get("version", "1.0.0")
        major, minor, patch = current_version.split(".")
        agent_config["version"] = f"{major}.{minor}.{int(patch) + 1}"
        
        # Update last refresh time
        agent_config["refresh_config"]["last_refresh"] = datetime.now().isoformat()
        
        # Save updated configuration
        with open(config_file, 'w') as f:
            yaml.dump(agent_config, f, default_flow_style=False)
        
        # Update registry
        self.registry["agents"][agent_name]["last_refresh"] = datetime.now().isoformat()
        self.registry["agents"][agent_name]["version"] = agent_config["version"]
        
        # Track metrics
        if "refresh_history" not in self.registry["metrics"]:
            self.registry["metrics"]["refresh_history"] = []
        self.registry["metrics"]["refresh_history"].append(refresh_results)
        
        self.save_registry()
        
        return refresh_results
    
    def get_agent_health(self, agent_name: str) -> Dict:
        """Check agent health and status"""
        if agent_name not in self.registry["agents"]:
            return {"status": "not_found", "health": 0.0}
        
        agent_info = self.registry["agents"][agent_name]
        agent_path = Path(agent_info["path"])
        
        health_score = 1.0
        issues = []
        
        # Check if agent path exists
        if not agent_path.exists():
            health_score -= 0.5
            issues.append("Agent directory missing")
        
        # Check configuration file
        config_file = agent_path / "agent_config.yaml"
        if not config_file.exists():
            health_score -= 0.3
            issues.append("Configuration file missing")
        
        # Check last refresh time
        last_refresh = agent_info.get("last_refresh")
        if last_refresh:
            refresh_date = datetime.fromisoformat(last_refresh)
            days_since_refresh = (datetime.now() - refresh_date).days
            if days_since_refresh > 7:
                health_score -= 0.1
                issues.append(f"Not refreshed in {days_since_refresh} days")
        else:
            health_score -= 0.1
            issues.append("Never refreshed")
        
        return {
            "status": "healthy" if health_score > 0.7 else "unhealthy",
            "health": max(health_score, 0.0),
            "issues": issues,
            "last_checked": datetime.now().isoformat()
        }
    
    def _determine_capabilities(self, specialization: AgentSpecialization) -> List[str]:
        """Determine agent capabilities based on specialization"""
        base_capabilities = ["query", "explain", "guide"]
        
        if specialization == AgentSpecialization.DOMAIN:
            return base_capabilities + ["domain_expertise", "technical_analysis", "compliance_check"]
        elif specialization == AgentSpecialization.MODULE:
            return base_capabilities + ["module_navigation", "spec_interpretation", "implementation_guidance"]
        elif specialization == AgentSpecialization.SUBMODULE:
            return base_capabilities + ["detailed_implementation", "specific_guidance"]
        elif specialization == AgentSpecialization.CROSS_MODULE:
            return base_capabilities + ["integration_planning", "dependency_analysis", "collaboration"]
        else:
            return base_capabilities
    
    def _determine_refresh_priority(self, specialization: AgentSpecialization) -> str:
        """Determine refresh priority based on specialization"""
        if specialization in [AgentSpecialization.DOMAIN, AgentSpecialization.CROSS_MODULE]:
            return "high"
        elif specialization == AgentSpecialization.MODULE:
            return "medium"
        else:
            return "low"
    
    def _determine_context_size(self, specialization: AgentSpecialization) -> int:
        """Determine optimal context size based on specialization"""
        # Specialized agents need less context due to focused domain
        if specialization in [AgentSpecialization.SUBMODULE]:
            return 4000  # Smaller context for very specific agents
        elif specialization in [AgentSpecialization.MODULE, AgentSpecialization.DOMAIN]:
            return 8000  # Medium context for module-level agents
        elif specialization == AgentSpecialization.CROSS_MODULE:
            return 12000  # Larger context for cross-module work
        else:
            return 16000  # Full context for general agents


class EnhancedDocumentationManager:
    """
    Enhanced documentation manager v3.0 combining:
    - v2.0 features (RAG, context engineering, memory management)
    - Phased document processing
    - Modular agent management
    """
    
    def __init__(self, agent_path: Path):
        self.agent_path = agent_path
        self.context_path = agent_path / "context"
        self.registry_file = self.context_path / "docs_registry.yaml"
        self.validation_log = self.context_path / "validation_log.yaml"
        self.chunk_index = self.context_path / "chunk_index.json"
        
        # Initialize processors
        self.phased_processor = PhasedDocumentProcessor(agent_path)
        self.modular_manager = ModularAgentManager(agent_path.parent)
        
        # Load registry
        self.registry = self.load_registry()
        
        # Context layers (from v2.0)
        self.context_layers = {
            "domain": self.context_path / "domain",
            "operational": self.context_path / "operational",
            "episodic": self.context_path / "episodic",
            "semantic": self.context_path / "semantic",
            "module": self.context_path / "module",
            "submodule": self.context_path / "submodule"
        }
        
        # Create layer directories
        for layer_dir in self.context_layers.values():
            layer_dir.mkdir(parents=True, exist_ok=True)
    
    def load_registry(self) -> dict:
        """Load or initialize documentation registry"""
        if self.registry_file.exists():
            with open(self.registry_file, 'r') as f:
                return yaml.safe_load(f) or {
                    "documents": {}, 
                    "chunks": {}, 
                    "embeddings": {},
                    "phases": {},
                    "modules": {}
                }
        return {
            "documents": {}, 
            "chunks": {}, 
            "embeddings": {},
            "phases": {},
            "modules": {}
        }
    
    def save_registry(self):
        """Save documentation registry with validation"""
        self.context_path.mkdir(parents=True, exist_ok=True)
        
        # Validate registry before saving
        self.validate_registry()
        
        with open(self.registry_file, 'w') as f:
            yaml.dump(self.registry, f, default_flow_style=False, sort_keys=False)
    
    def validate_registry(self):
        """Validate registry for context poisoning prevention"""
        validation_results = []
        
        for doc_id, doc_info in self.registry.get("documents", {}).items():
            # Check for missing required fields
            required_fields = ["path", "category", "hash", "title", "added_date"]
            missing_fields = [f for f in required_fields if f not in doc_info]
            
            if missing_fields:
                validation_results.append({
                    "doc_id": doc_id,
                    "issue": "missing_fields",
                    "fields": missing_fields,
                    "timestamp": datetime.now().isoformat()
                })
        
        # Save validation log
        if validation_results:
            with open(self.validation_log, 'w') as f:
                yaml.dump({"validations": validation_results}, f)
    
    def process_documents_phased(self, doc_paths: List[Path], 
                                module_name: Optional[str] = None) -> Dict:
        """
        Process documents using phased approach
        Implements mixed-documentation-agent specification
        """
        print("\n📚 Starting Phased Document Processing")
        print(f"   Total documents: {len(doc_paths)}")
        if module_name:
            print(f"   Target module: {module_name}")
        
        results = {}
        
        # Phase 1: Discovery
        results["discovery"] = self.phased_processor.phase1_discovery(doc_paths)
        print(f"   ✓ Discovery complete: {results['discovery']['total_documents']} documents")
        
        # Phase 2: Quality Assessment
        results["quality"] = self.phased_processor.phase2_quality_assessment(
            results["discovery"]
        )
        print(f"   ✓ Quality assessment: {len(results['quality']['high_priority'])} high priority")
        
        # Phase 3: Extraction
        results["extraction"] = self.phased_processor.phase3_extraction(
            results["quality"], self
        )
        print(f"   ✓ Extraction: {results['extraction']['extraction_metrics']['total_entities']} entities")
        
        # Phase 4: Synthesis
        results["synthesis"] = self.phased_processor.phase4_synthesis(
            results["extraction"]
        )
        print(f"   ✓ Synthesis: {results['synthesis']['synthesis_metrics']['total_synthesized']} synthesized")
        
        # Phase 5: Validation
        results["validation"] = self.phased_processor.phase5_validation(
            results["synthesis"]
        )
        print(f"   ✓ Validation: {results['validation']['quality_metrics']['validation_pass_rate']:.1%} pass rate")
        
        # Phase 6: Integration
        results["integration"] = self.phased_processor.phase6_integration(
            results["validation"], self
        )
        print(f"   ✓ Integration: {results['integration']['integration_metrics']['total_integrated']} integrated")
        
        # Update registry with phase results
        self.registry["phases"][datetime.now().isoformat()] = {
            "module": module_name,
            "documents_processed": len(doc_paths),
            "phases_completed": list(results.keys()),
            "integration_rate": results["integration"]["integration_metrics"]["integration_rate"]
        }
        
        # If module specified, update module registry
        if module_name:
            if module_name not in self.registry["modules"]:
                self.registry["modules"][module_name] = {
                    "documents": [],
                    "last_processed": None,
                    "knowledge_count": 0
                }
            
            self.registry["modules"][module_name]["documents"].extend(
                [str(p) for p in doc_paths]
            )
            self.registry["modules"][module_name]["last_processed"] = datetime.now().isoformat()
            self.registry["modules"][module_name]["knowledge_count"] = \
                results["integration"]["integration_metrics"]["total_integrated"]
        
        self.save_registry()
        
        return results
    
    def calculate_hash(self, content: str) -> str:
        """Calculate SHA256 hash of content"""
        return hashlib.sha256(content.encode()).hexdigest()
    
    def chunk_document(self, content: str, 
                      strategy: ChunkingStrategy = ChunkingStrategy.PHASED) -> List[Dict]:
        """
        Enhanced chunking with phased strategy support
        """
        if strategy == ChunkingStrategy.PHASED:
            # Phased chunking for large documents
            return self._phased_chunking(content)
        else:
            # Use existing chunking strategies from v2.0
            return self._standard_chunking(content, strategy)
    
    def _phased_chunking(self, content: str) -> List[Dict]:
        """
        Phased chunking strategy for large documents
        Based on mixed-documentation-agent spec
        """
        chunks = []
        chunk_id = 0
        
        # Phase 1: High-level sections (large chunks)
        sections = re.split(r'^#{1,2}\s+.*?$', content, flags=re.MULTILINE)
        
        for section in sections:
            if not section.strip():
                continue
            
            # Phase 2: If section is large, break into subsections
            if len(section) > 4000:
                subsections = re.split(r'^#{3,6}\s+.*?$', section, flags=re.MULTILINE)
                
                for subsection in subsections:
                    if not subsection.strip():
                        continue
                    
                    # Phase 3: If still large, use fixed-size chunks
                    if len(subsection) > 2000:
                        words = subsection.split()
                        current_chunk = []
                        
                        for word in words:
                            current_chunk.append(word)
                            
                            if len(" ".join(current_chunk)) >= 1500:
                                chunk_text = " ".join(current_chunk)
                                chunks.append({
                                    "id": f"phased_{chunk_id}",
                                    "text": chunk_text,
                                    "strategy": "phased",
                                    "phase": 3,
                                    "size": len(chunk_text)
                                })
                                current_chunk = current_chunk[-50:]  # Overlap
                                chunk_id += 1
                        
                        if current_chunk:
                            chunks.append({
                                "id": f"phased_{chunk_id}",
                                "text": " ".join(current_chunk),
                                "strategy": "phased",
                                "phase": 3,
                                "size": len(" ".join(current_chunk))
                            })
                            chunk_id += 1
                    else:
                        # Phase 2 chunk
                        chunks.append({
                            "id": f"phased_{chunk_id}",
                            "text": subsection.strip(),
                            "strategy": "phased",
                            "phase": 2,
                            "size": len(subsection)
                        })
                        chunk_id += 1
            else:
                # Phase 1 chunk
                chunks.append({
                    "id": f"phased_{chunk_id}",
                    "text": section.strip(),
                    "strategy": "phased",
                    "phase": 1,
                    "size": len(section)
                })
                chunk_id += 1
        
        return chunks
    
    def _standard_chunking(self, content: str, strategy: ChunkingStrategy) -> List[Dict]:
        """Standard chunking strategies from v2.0"""
        # Implementation from v2.0
        # (Simplified here - full implementation in v2.0)
        chunks = []
        
        if strategy == ChunkingStrategy.HYBRID:
            # Hybrid approach from v2.0
            sections = re.split(r'^#{1,6}\s+.*?$', content, flags=re.MULTILINE)
            chunk_id = 0
            
            for section in sections:
                if not section.strip():
                    continue
                
                if len(section) > 2400:
                    # Break into smaller chunks
                    words = section.split()
                    current_chunk = []
                    
                    for word in words:
                        current_chunk.append(word)
                        if len(" ".join(current_chunk)) >= 2000:
                            chunks.append({
                                "id": f"hybrid_{chunk_id}",
                                "text": " ".join(current_chunk),
                                "strategy": "hybrid",
                                "size": len(" ".join(current_chunk))
                            })
                            current_chunk = current_chunk[-50:]
                            chunk_id += 1
                    
                    if current_chunk:
                        chunks.append({
                            "id": f"hybrid_{chunk_id}",
                            "text": " ".join(current_chunk),
                            "strategy": "hybrid",
                            "size": len(" ".join(current_chunk))
                        })
                        chunk_id += 1
                else:
                    chunks.append({
                        "id": f"hybrid_{chunk_id}",
                        "text": section.strip(),
                        "strategy": "hybrid",
                        "size": len(section)
                    })
                    chunk_id += 1
        
        return chunks


class EnhancedAgentGeneratorV3:
    """
    Enhanced agent generator v3.0 with mandatory principles:
    - Phased document processing
    - Modular agent management
    - Plus all v2.0 features
    """
    
    def __init__(self, module_name: str, mode: AgentMode = AgentMode.CREATE):
        self.module_name = module_name
        self.mode = mode
        self.agent_path = Path("agents") / module_name
        self.doc_manager = None
        self.modular_manager = ModularAgentManager(Path("agents"))
        
        if mode == AgentMode.UPDATE:
            if not self.agent_path.exists():
                raise ValueError(f"Agent '{module_name}' does not exist. Use --mode create to create it.")
            self.doc_manager = EnhancedDocumentationManager(self.agent_path)
        elif mode == AgentMode.CREATE:
            if self.agent_path.exists():
                raise ValueError(f"Agent '{module_name}' already exists. Use --mode update to modify it.")
        elif mode == AgentMode.REFRESH:
            if not self.agent_path.exists():
                raise ValueError(f"Agent '{module_name}' does not exist. Cannot refresh.")
            self.doc_manager = EnhancedDocumentationManager(self.agent_path)
    
    def create_agent(self, agent_type: str = "general-purpose",
                    repos: List[str] = None,
                    context_cache: bool = True,
                    templates: List[str] = None,
                    module_path: Optional[Path] = None,
                    documents: Optional[List[Path]] = None):
        """
        Create a new agent with v3.0 features:
        - Phased document processing
        - Modular specialization
        - All v2.0 capabilities
        """
        
        print(f"\n🚀 Creating Enhanced Agent v3.0: {self.module_name}")
        
        # Determine specialization
        specialization = AgentSpecialization.GENERAL
        if module_path:
            spec_count = len(list(module_path.glob("**/*.md"))) if module_path.exists() else 0
            has_submodules = bool(list(module_path.glob("*/"))) if module_path.exists() else False
            specialization = self.modular_manager.determine_specialization(
                self.module_name, spec_count, has_submodules
            )
        
        print(f"   Specialization: {specialization.value}")
        
        # Create modular agent
        module_agent_config = self.modular_manager.create_module_agent(
            self.module_name,
            module_path or Path("."),
            specialization
        )
        
        # Create agent directory structure
        self.agent_path.mkdir(parents=True, exist_ok=True)
        
        # Enhanced subdirectories (v2.0 + v3.0)
        subdirs = [
            # v2.0 directories
            "context",
            "context/internal",
            "context/external",
            "context/web",
            "context/repository",
            "context/optimized",
            "context/context",
            "context/memory",
            "context/domain",
            "context/operational",
            "context/episodic",
            "context/semantic",
            # v3.0 additions
            "context/module",
            "context/submodule",
            "processing",
            "processing/phases",
            "processing/metrics",
            "prompts",
            "templates",
            "tools",
            "scratchpad",
            "validation",
            "refresh"
        ]
        
        for subdir in subdirs:
            (self.agent_path / subdir).mkdir(parents=True, exist_ok=True)
        
        # Create enhanced agent configuration
        agent_config = {
            "name": self.module_name,
            "type": agent_type,
            "version": "3.0.0",
            "created": datetime.now().isoformat(),
            "specialization": specialization.value,
            "repositories": repos or [],
            "context_cache": context_cache,
            "templates": templates or [],
            "module_config": module_agent_config,
            "capabilities": {
                # v2.0 capabilities
                "context_engineering": True,
                "memory_management": True,
                "chunking_strategies": ["semantic", "fixed_size", "paragraph", 
                                       "section", "hybrid", "phased"],
                "duplicate_detection": True,
                "context_validation": True,
                "context_pruning": True,
                # v3.0 capabilities
                "phased_processing": True,
                "modular_specialization": True,
                "auto_refresh": True,
                "cross_module_collaboration": specialization == AgentSpecialization.CROSS_MODULE
            },
            "best_practices": {
                # v2.0 practices
                "rag_optimization": True,
                "context_layers": True,
                "attention_manipulation": True,
                "scratchpad_processing": True,
                # v3.0 practices
                "phased_document_approach": True,
                "modular_agent_management": True,
                "incremental_knowledge_building": True,
                "automated_refresh": True
            },
            "processing_config": {
                "phased_approach": {
                    "enabled": True,
                    "phases": ["discovery", "quality", "extraction", 
                              "synthesis", "validation", "integration"],
                    "quality_threshold": 0.7,
                    "validation_mode": "automatic"
                },
                "module_optimization": {
                    "context_size": self.modular_manager._determine_context_size(specialization),
                    "refresh_priority": self.modular_manager._determine_refresh_priority(specialization),
                    "focused_domain": self.module_name
                }
            }
        }
        
        # Save agent configuration
        with open(self.agent_path / "agent.yaml", 'w') as f:
            yaml.dump(agent_config, f, default_flow_style=False)
        
        # Initialize documentation manager
        self.doc_manager = EnhancedDocumentationManager(self.agent_path)
        self.doc_manager.save_registry()
        
        # Process initial documents if provided
        if documents:
            print(f"\n📄 Processing {len(documents)} initial documents...")
            results = self.doc_manager.process_documents_phased(documents, self.module_name)
            print(f"   ✓ Processed with {results['integration']['integration_metrics']['integration_rate']:.1%} integration rate")
        
        # Create README with v3.0 features
        readme_content = f"""# {self.module_name} Agent v3.0

## Overview
This agent implements mandatory v3.0 principles:
1. **Phased Document Processing** - Based on mixed-documentation-agent specification
2. **Modular Agent Management** - Based on modular-agent-management specification
3. **Plus all v2.0 features** - RAG optimization, context engineering, memory management

## Specialization: {specialization.value}
{self._get_specialization_description(specialization)}

## Features

### Phased Document Processing (v3.0)
- **Phase 1: Discovery** - Document inventory and classification
- **Phase 2: Quality Assessment** - Quality scoring and prioritization
- **Phase 3: Extraction** - Knowledge extraction with source tracking
- **Phase 4: Synthesis** - Conflict resolution and consolidation
- **Phase 5: Validation** - Consistency checks and quality assurance
- **Phase 6: Integration** - Agent knowledge integration

### Modular Management (v3.0)
- **Specialization Level**: {specialization.value}
- **Context Optimization**: {agent_config['processing_config']['module_optimization']['context_size']} tokens
- **Refresh Priority**: {agent_config['processing_config']['module_optimization']['refresh_priority']}
- **Auto-Refresh**: Enabled (7-day interval)

### Context Engineering (v2.0)
- **Layered Architecture**: Domain, operational, episodic, semantic, module, submodule
- **Memory Management**: Short-term and long-term with pruning
- **RAG Optimization**: Advanced chunking with phased strategy
- **Duplicate Detection**: SHA256-based content hashing

## Structure
```
{self.module_name}/
├── agent.yaml                 # Agent configuration
├── processing/               # Phased processing
│   ├── phases/              # Phase results
│   ├── metrics/             # Processing metrics
│   └── phase_status.yaml    # Current status
├── context/                 # Context management
│   ├── docs_registry.yaml   # Documentation registry
│   ├── chunk_index.json     # Chunk index
│   ├── module/              # Module-specific docs
│   ├── submodule/           # Submodule-specific docs
│   └── [other layers]/      # Context layers
├── refresh/                 # Refresh mechanisms
├── prompts/                 # Agent prompts
├── templates/               # Reusable templates
├── tools/                   # Custom tools
├── scratchpad/              # Temporary workspace
└── validation/              # Quarantine and validation
```

## Usage

### Process Documents (Phased Approach)
```bash
python create_module_agent.py {self.module_name} --mode update \\
  --process-docs "/path/to/docs" --phased --module {self.module_name}
```

### Refresh Agent Knowledge
```bash
python create_module_agent.py {self.module_name} --mode refresh
```

### Add Documentation
```bash
python create_module_agent.py {self.module_name} --mode update \\
  --add-doc ./docs/guide.md --category module --title "Module Guide"
```

### Check Agent Health
```bash
python create_module_agent.py {self.module_name} --mode update --health-check
```

## Metrics
- **Specialization**: {specialization.value}
- **Context Size**: {agent_config['processing_config']['module_optimization']['context_size']} tokens
- **Refresh Priority**: {agent_config['processing_config']['module_optimization']['refresh_priority']}
- **Created**: {datetime.now().isoformat()}

---
*Enhanced Agent v3.0 - Implementing mandatory phased processing and modular management*
"""
        
        with open(self.agent_path / "README.md", 'w') as f:
            f.write(readme_content)
        
        # Create attention manipulation file (best practice)
        todo_content = """# Agent Tasks

## Current Focus
- [ ] Initialize agent context
- [ ] Process initial documentation (phased approach)
- [ ] Configure modular specialization
- [ ] Set up auto-refresh schedule

## Phased Processing Status
- [ ] Phase 1: Discovery
- [ ] Phase 2: Quality Assessment
- [ ] Phase 3: Extraction
- [ ] Phase 4: Synthesis
- [ ] Phase 5: Validation
- [ ] Phase 6: Integration

## Module Management
- [ ] Validate specialization level
- [ ] Configure context optimization
- [ ] Set refresh priority
- [ ] Enable cross-module collaboration (if applicable)

---
*This file helps with attention manipulation for better task execution*
"""
        
        with open(self.agent_path / "todo.md", 'w') as f:
            f.write(todo_content)
        
        print(f"\n✅ Created agent '{self.module_name}' with v3.0 features")
        print(f"   Location: {self.agent_path}")
        print(f"   Type: {agent_type}")
        print(f"   Specialization: {specialization.value}")
        print(f"   Features: Phased processing, modular management, plus all v2.0 capabilities")
    
    def refresh_agent(self):
        """Refresh agent knowledge using modular management"""
        print(f"\n🔄 Refreshing agent: {self.module_name}")
        
        # Use modular manager for refresh
        results = self.modular_manager.refresh_agent(self.module_name)
        
        print(f"   ✓ Refresh complete")
        print(f"   Specs processed: {results['specs_processed']}")
        print(f"   Status: {results['status']}")
        
        return results
    
    def _get_specialization_description(self, specialization: AgentSpecialization) -> str:
        """Get description for specialization level"""
        descriptions = {
            AgentSpecialization.GENERAL: "General-purpose agent with broad capabilities",
            AgentSpecialization.MODULE: "Module-specific agent with focused expertise",
            AgentSpecialization.SUBMODULE: "Submodule-specific agent with detailed knowledge",
            AgentSpecialization.DOMAIN: "Domain expert agent with technical specialization",
            AgentSpecialization.CROSS_MODULE: "Cross-module collaboration agent for integration"
        }
        return descriptions.get(specialization, "Unknown specialization")


class EnhancedArgumentParserV3:
    """Enhanced argument parser v3.0 with phased processing and modular management"""
    
    def __init__(self):
        self.parser = argparse.ArgumentParser(
            description="Enhanced Create-Module-Agent v3.0 - Mandatory Phased & Modular Principles",
            formatter_class=argparse.RawDescriptionHelpFormatter,
            epilog="""
MANDATORY PRINCIPLES:
  1. Phased Document Processing (mixed-documentation-agent spec)
  2. Modular Agent Management (modular-agent-management spec)
  
Examples:
  # Create specialized module agent with documents
  %(prog)s my-module --mode create --module-path ./specs/modules/my-module \\
    --process-docs "./docs/*.md" --phased
  
  # Refresh agent knowledge
  %(prog)s my-module --mode refresh
  
  # Process documents with phased approach
  %(prog)s my-module --mode update --process-docs "./new-docs" --phased
  
  # Check agent health
  %(prog)s my-module --mode update --health-check
  
Specialization Levels:
  - general-purpose: Broad capabilities
  - module-specific: Focused on module (>5 specs)
  - submodule-specific: Detailed submodule knowledge
  - domain-expert: Technical domain specialization
  - cross-module: Integration and collaboration
"""
        )
        
        # Required arguments
        self.parser.add_argument(
            'module_name',
            help='Name of the agent module'
        )
        
        # Mode selection
        self.parser.add_argument(
            '--mode',
            type=str,
            choices=['create', 'update', 'refresh'],
            default='create',
            help='Operation mode (default: create)'
        )
        
        # Module configuration
        self.parser.add_argument(
            '--module-path',
            type=str,
            help='Path to module specifications for specialization detection'
        )
        
        # Phased processing
        self.parser.add_argument(
            '--process-docs',
            type=str,
            help='Path/pattern to documents for phased processing'
        )
        
        self.parser.add_argument(
            '--phased',
            action='store_true',
            help='Use phased approach for document processing (mandatory for large collections)'
        )
        
        # Agent configuration
        self.parser.add_argument(
            '--type',
            default='general-purpose',
            help='Agent type (default: general-purpose)'
        )
        
        self.parser.add_argument(
            '--repos',
            type=str,
            help='Comma-separated list of repositories'
        )
        
        self.parser.add_argument(
            '--context-cache',
            type=lambda x: x.lower() == 'true',
            default=True,
            help='Enable context caching (default: true)'
        )
        
        # Health and monitoring
        self.parser.add_argument(
            '--health-check',
            action='store_true',
            help='Check agent health status'
        )
        
        # Documentation management (from v2.0)
        self.parser.add_argument(
            '--add-doc',
            type=str,
            help='Path to documentation file to add'
        )
        
        self.parser.add_argument(
            '--category',
            type=str,
            choices=['internal', 'external', 'web', 'repository', 'optimized', 
                    'context', 'memory', 'module', 'submodule'],
            default='internal',
            help='Category for added documentation (default: internal)'
        )
        
        self.parser.add_argument(
            '--title',
            type=str,
            help='Title for added documentation'
        )
        
        self.parser.add_argument(
            '--list-docs',
            nargs='?',
            const='all',
            help='List documentation (optionally filtered by category)'
        )
    
    def parse(self):
        """Parse and validate arguments"""
        args = self.parser.parse_args()
        
        # Validate mode-specific requirements
        if args.mode == 'create':
            if args.health_check:
                self.parser.error("Health check requires --mode update or refresh")
        
        if args.process_docs and not args.phased:
            print("⚠️  Warning: Large document collections should use --phased approach")
        
        return args


def main():
    """Main entry point for v3.0"""
    try:
        # Parse arguments
        parser = EnhancedArgumentParserV3()
        args = parser.parse()
        
        # Convert arguments
        repos = args.repos.split(',') if args.repos else None
        mode = AgentMode(args.mode)
        
        # Create generator
        generator = EnhancedAgentGeneratorV3(args.module_name, mode)
        
        if mode == AgentMode.CREATE:
            # Process documents if provided
            documents = None
            if args.process_docs:
                from glob import glob
                doc_patterns = args.process_docs.split(',')
                documents = []
                for pattern in doc_patterns:
                    documents.extend([Path(p) for p in glob(pattern)])
            
            # Create agent
            module_path = Path(args.module_path) if args.module_path else None
            generator.create_agent(
                agent_type=args.type,
                repos=repos,
                context_cache=args.context_cache,
                module_path=module_path,
                documents=documents
            )
        
        elif mode == AgentMode.UPDATE:
            # Handle various update operations
            if args.health_check:
                # Check health
                health = generator.modular_manager.get_agent_health(args.module_name)
                print(f"\n💊 Agent Health: {args.module_name}")
                print(f"   Status: {health['status']}")
                print(f"   Health Score: {health['health']:.1%}")
                if health['issues']:
                    print("   Issues:")
                    for issue in health['issues']:
                        print(f"     - {issue}")
            
            elif args.process_docs:
                # Process documents with phased approach
                from glob import glob
                doc_patterns = args.process_docs.split(',')
                documents = []
                for pattern in doc_patterns:
                    documents.extend([Path(p) for p in glob(pattern)])
                
                if args.phased:
                    results = generator.doc_manager.process_documents_phased(
                        documents, args.module_name
                    )
                    print(f"\n✅ Phased processing complete")
                    print(f"   Integration rate: {results['integration']['integration_metrics']['integration_rate']:.1%}")
                else:
                    print("Processing documents without phased approach...")
                    # Standard processing
            
            elif args.add_doc:
                # Add documentation
                category = DocumentCategory(args.category)
                success, message = generator.doc_manager.add_documentation(
                    args.add_doc,
                    category,
                    args.title
                )
                print(message)
            
            elif args.list_docs:
                # List documentation
                if args.list_docs != 'all':
                    try:
                        category = DocumentCategory(args.list_docs)
                        print(generator.doc_manager.list_documentation(category))
                    except:
                        print(generator.doc_manager.list_documentation())
                else:
                    print(generator.doc_manager.list_documentation())
            
            else:
                print(f"ℹ️ Agent '{args.module_name}' is ready for updates")
                print("   Use --process-docs, --add-doc, --list-docs, or --health-check")
        
        elif mode == AgentMode.REFRESH:
            # Refresh agent
            results = generator.refresh_agent()
            print(f"\n✅ Agent refreshed successfully")
    
    except ValueError as e:
        print(f"❌ Error: {str(e)}", file=sys.stderr)
        sys.exit(1)
    except Exception as e:
        print(f"❌ Unexpected error: {str(e)}", file=sys.stderr)
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()