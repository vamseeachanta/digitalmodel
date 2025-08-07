# Technical Specification: Modular Agent Management System

## Architecture Overview

### System Components

#### 1. Agent Directory Structure
```
agents/
├── module-agents/                  # Primary module agents
│   ├── agent-os/
│   │   ├── agent-config.yaml      # Agent configuration
│   │   ├── knowledge-base.json    # Processed specification knowledge
│   │   ├── capabilities.yaml     # Agent capabilities and expertise
│   │   └── prompt-templates/      # Specialized prompt templates
│   ├── marine-engineering/
│   ├── infrastructure/
│   ├── development-tools/
│   ├── ai-workflows/
│   └── test-suite-automation/
├── submodule-agents/               # Specialized submodule agents
│   ├── marine-engineering/
│   │   ├── orcaflex/
│   │   ├── aqwa/
│   │   ├── hydrodynamics/
│   │   └── engineering-standards/
│   └── agent-os/
│       ├── foundation/
│       ├── integration/
│       ├── python-integration/
│       └── slash-commands/
└── agent-management/               # Management infrastructure
    ├── templates/                  # Agent templates and schemas
    ├── refresh-system/            # Automated refresh mechanisms
    ├── discovery/                 # Agent discovery and health monitoring
    └── collaboration/             # Cross-agent communication protocols
```

#### 2. Agent Configuration Schema
```yaml
# agent-config.yaml template
metadata:
  name: "agent-os"
  version: "1.0.0"
  created: "2025-01-25"
  last_updated: "2025-01-25"
  module_path: "specs/modules/agent-os"
  
specialization:
  domain: "Agent OS Framework"
  expertise_areas:
    - "AI-assisted development workflows"
    - "Agent OS integration and setup"
    - "Slash command implementation"
    - "Cross-platform compatibility"
  
knowledge_sources:
  - path: "specs/modules/agent-os/foundation/"
    priority: "high"
    last_synced: "2025-01-25T10:00:00Z"
  - path: "specs/modules/agent-os/integration/"
    priority: "high" 
    last_synced: "2025-01-25T10:00:00Z"
  - path: "specs/modules/agent-os/slash-commands/"
    priority: "medium"
    last_synced: "2025-01-25T10:00:00Z"

capabilities:
  context_optimization: true
  cross_module_collaboration: true
  specification_analysis: true
  task_decomposition: true
  
refresh_config:
  auto_refresh: true
  refresh_triggers:
    - "specification_update"
    - "weekly_schedule"
  refresh_schedule: "0 2 * * 0"  # Weekly at 2 AM Sunday
  
performance_targets:
  context_reduction: 50          # Target % reduction in context size
  response_time_ms: 2000        # Target response time
  accuracy_threshold: 0.95       # Minimum accuracy requirement
```

#### 3. Knowledge Base Format
```json
{
  "agent_id": "agent-os",
  "knowledge_version": "1.0.0",
  "last_updated": "2025-01-25T10:00:00Z",
  "processed_content": {
    "specifications": [
      {
        "source_file": "specs/modules/agent-os/foundation/feature-ai-framework-foundation-setup-2025.md",
        "content_hash": "abc123...",
        "processed_sections": {
          "overview": "Core Agent OS framework setup...",
          "user_stories": "As a developer, I want...",
          "technical_approach": "Implementation strategy involves..."
        },
        "key_concepts": [
          "AI persona system",
          "Command framework", 
          "Integration patterns"
        ],
        "cross_references": [
          "specs/modules/agent-os/integration/spec.md"
        ]
      }
    ],
    "expertise_map": {
      "slash_commands": {
        "confidence": 0.95,
        "key_files": ["specs/modules/agent-os/slash-commands/"]
      },
      "integration_patterns": {
        "confidence": 0.90,
        "key_files": ["specs/modules/agent-os/integration/"]
      }
    }
  }
}
```

### Implementation Components

#### 1. Agent Template System
```python
class AgentTemplate:
    def __init__(self, template_config: Dict):
        self.config = template_config
        self.knowledge_processor = KnowledgeProcessor()
        self.prompt_generator = PromptGenerator()
    
    def create_agent(self, module_path: str) -> Agent:
        """Create new agent from template and module specifications"""
        knowledge_base = self.knowledge_processor.process_specifications(module_path)
        capabilities = self.extract_capabilities(knowledge_base)
        prompts = self.prompt_generator.generate_specialized_prompts(capabilities)
        
        return Agent(
            config=self.generate_config(module_path),
            knowledge_base=knowledge_base,
            capabilities=capabilities,
            prompt_templates=prompts
        )
    
    def extract_capabilities(self, knowledge_base: Dict) -> Dict:
        """Extract agent capabilities from processed knowledge"""
        # Implementation for capability detection
        pass
```

#### 2. Agent Refresh System
```python
class AgentRefreshSystem:
    def __init__(self, agents_directory: str):
        self.agents_dir = Path(agents_directory)
        self.file_monitor = FileSystemMonitor()
        self.refresh_scheduler = RefreshScheduler()
    
    def refresh_agent(self, agent_id: str, force: bool = False) -> RefreshResult:
        """Refresh specific agent with updated knowledge"""
        agent_path = self.agents_dir / agent_id
        
        # Check if refresh needed
        if not force and not self.needs_refresh(agent_path):
            return RefreshResult(skipped=True, reason="No updates needed")
        
        # Backup current agent
        backup_path = self.create_backup(agent_path)
        
        try:
            # Process updated specifications
            updated_knowledge = self.process_updated_specs(agent_path)
            
            # Update agent configuration
            self.update_agent_config(agent_path, updated_knowledge)
            
            # Validate updated agent
            validation_result = self.validate_agent(agent_path)
            
            if validation_result.success:
                return RefreshResult(success=True, agent_id=agent_id)
            else:
                # Rollback on validation failure
                self.rollback_from_backup(agent_path, backup_path)
                return RefreshResult(
                    success=False, 
                    error=validation_result.error,
                    rollback_performed=True
                )
                
        except Exception as e:
            # Rollback on any error
            self.rollback_from_backup(agent_path, backup_path)
            return RefreshResult(success=False, error=str(e), rollback_performed=True)
```

#### 3. Agent Discovery System
```python
class AgentDiscovery:
    def __init__(self, agents_directory: str):
        self.agents_dir = Path(agents_directory)
        self.health_monitor = AgentHealthMonitor()
    
    def discover_agents(self) -> List[AgentInfo]:
        """Discover all available agents and their status"""
        agents = []
        
        for agent_dir in self.agents_dir.rglob("*/agent-config.yaml"):
            try:
                agent_config = self.load_agent_config(agent_dir)
                health_status = self.health_monitor.check_agent_health(agent_dir.parent)
                
                agents.append(AgentInfo(
                    id=agent_config['metadata']['name'],
                    path=agent_dir.parent,
                    config=agent_config,
                    health_status=health_status,
                    last_updated=agent_config['metadata']['last_updated']
                ))
            except Exception as e:
                agents.append(AgentInfo(
                    id=agent_dir.parent.name,
                    path=agent_dir.parent,
                    health_status=HealthStatus.ERROR,
                    error=str(e)
                ))
        
        return agents
    
    def get_agent_by_domain(self, domain: str) -> Optional[AgentInfo]:
        """Find most appropriate agent for given domain"""
        agents = self.discover_agents()
        
        # Find exact domain match first
        for agent in agents:
            if agent.config and agent.config['specialization']['domain'].lower() == domain.lower():
                return agent
        
        # Find best expertise match
        best_match = None
        best_score = 0
        
        for agent in agents:
            if agent.config:
                score = self.calculate_domain_match_score(domain, agent.config['specialization'])
                if score > best_score:
                    best_score = score
                    best_match = agent
        
        return best_match if best_score > 0.5 else None
```

### Slash Command Implementation

#### /refresh-agent Command
```python
class RefreshAgentCommand:
    def __init__(self):
        self.refresh_system = AgentRefreshSystem("agents/")
        self.discovery = AgentDiscovery("agents/")
    
    def execute(self, args: List[str]) -> CommandResult:
        """
        Execute /refresh-agent command
        
        Usage:
          /refresh-agent <module_name>           # Refresh specific module agent
          /refresh-agent <module>/<submodule>   # Refresh submodule agent
          /refresh-agent --all                  # Refresh all agents
          /refresh-agent --dry-run <module>     # Show what would be refreshed
        """
        parser = argparse.ArgumentParser(description="Refresh agent knowledge")
        parser.add_argument("target", nargs="?", help="Module or submodule to refresh")
        parser.add_argument("--all", action="store_true", help="Refresh all agents")
        parser.add_argument("--dry-run", action="store_true", help="Show what would be refreshed")
        parser.add_argument("--force", action="store_true", help="Force refresh even if not needed")
        
        try:
            parsed_args = parser.parse_args(args)
            
            if parsed_args.all:
                return self.refresh_all_agents(parsed_args.dry_run, parsed_args.force)
            elif parsed_args.target:
                return self.refresh_target_agent(parsed_args.target, parsed_args.dry_run, parsed_args.force)
            else:
                return CommandResult(
                    success=False,
                    message="Please specify a target module/submodule or use --all"
                )
                
        except Exception as e:
            return CommandResult(success=False, error=str(e))
    
    def refresh_target_agent(self, target: str, dry_run: bool, force: bool) -> CommandResult:
        """Refresh specific target agent"""
        agent_info = self.discovery.get_agent_by_domain(target)
        
        if not agent_info:
            return CommandResult(
                success=False,
                message=f"No agent found for target: {target}"
            )
        
        if dry_run:
            return CommandResult(
                success=True,
                message=f"Would refresh agent: {agent_info.id} (last updated: {agent_info.last_updated})"
            )
        
        refresh_result = self.refresh_system.refresh_agent(agent_info.id, force)
        
        return CommandResult(
            success=refresh_result.success,
            message=self.format_refresh_result(refresh_result)
        )
```

### Integration with /create-spec

#### Enhanced Spec Generation
```python
class EnhancedSpecGenerator:
    def __init__(self):
        self.agent_discovery = AgentDiscovery("agents/")
        self.spec_analyzer = SpecificationAnalyzer()
    
    def generate_spec_with_agents(self, spec_requirements: Dict) -> SpecificationDocument:
        """Generate specification with appropriate agent assignments"""
        
        # Analyze specification domain and complexity
        domain_analysis = self.spec_analyzer.analyze_domain(spec_requirements)
        
        # Find appropriate agents
        recommended_agents = self.find_recommended_agents(domain_analysis)
        
        # Generate specification document
        spec_doc = self.generate_base_specification(spec_requirements)
        
        # Enhance tasks with agent assignments
        enhanced_tasks = self.enhance_tasks_with_agents(spec_doc.tasks, recommended_agents)
        
        # Add agent collaboration patterns if multi-domain
        if len(recommended_agents) > 1:
            collaboration_section = self.generate_collaboration_section(recommended_agents)
            spec_doc.add_section(collaboration_section)
        
        return SpecificationDocument(
            content=spec_doc.content,
            tasks=enhanced_tasks,
            recommended_agents=recommended_agents,
            collaboration_patterns=collaboration_section if len(recommended_agents) > 1 else None
        )
    
    def find_recommended_agents(self, domain_analysis: DomainAnalysis) -> List[AgentInfo]:
        """Find appropriate agents based on domain analysis"""
        recommended = []
        
        # Primary domain agent
        primary_agent = self.agent_discovery.get_agent_by_domain(domain_analysis.primary_domain)
        if primary_agent:
            recommended.append(primary_agent)
        
        # Secondary domain agents
        for secondary_domain in domain_analysis.secondary_domains:
            agent = self.agent_discovery.get_agent_by_domain(secondary_domain)
            if agent and agent not in recommended:
                recommended.append(agent)
        
        # Specialized submodule agents if needed
        if domain_analysis.complexity_score > 0.7:
            specialized_agents = self.find_specialized_agents(domain_analysis)
            recommended.extend(specialized_agents)
        
        return recommended
    
    def enhance_tasks_with_agents(self, tasks: List[Task], agents: List[AgentInfo]) -> List[Task]:
        """Add agent assignments to tasks"""
        enhanced_tasks = []
        
        for task in tasks:
            # Analyze task domain and complexity
            task_analysis = self.spec_analyzer.analyze_task(task)
            
            # Find best agent for task
            best_agent = self.find_best_agent_for_task(task_analysis, agents)
            
            # Add agent assignment to task
            if best_agent:
                task.add_metadata("assigned_agent", best_agent.id)
                task.add_metadata("agent_expertise", best_agent.config['specialization']['expertise_areas'])
            
            enhanced_tasks.append(task)
        
        return enhanced_tasks
```

### Performance Optimization

#### Context Compression System
```python
class ContextCompressor:
    def __init__(self):
        self.relevance_scorer = RelevanceScorer()
        self.content_summarizer = ContentSummarizer()
    
    def compress_agent_context(self, agent_config: Dict, query_context: str) -> CompressedContext:
        """Compress agent context based on query relevance"""
        
        knowledge_base = agent_config['knowledge_base']
        
        # Score content relevance to query
        relevance_scores = self.relevance_scorer.score_content_relevance(
            knowledge_base, query_context
        )
        
        # Select most relevant content
        relevant_content = self.select_relevant_content(
            knowledge_base, relevance_scores, threshold=0.3
        )
        
        # Summarize less relevant but potentially useful content
        summarized_content = self.content_summarizer.summarize_content(
            knowledge_base, relevance_scores, threshold=0.1
        )
        
        return CompressedContext(
            relevant_content=relevant_content,
            summarized_content=summarized_content,
            compression_ratio=self.calculate_compression_ratio(knowledge_base, relevant_content)
        )
```

### Error Handling and Validation

#### Agent Validation System
```python
class AgentValidator:
    def __init__(self):
        self.schema_validator = SchemaValidator()
        self.knowledge_validator = KnowledgeValidator()
        self.performance_validator = PerformanceValidator()
    
    def validate_agent(self, agent_path: Path) -> ValidationResult:
        """Comprehensive agent validation"""
        results = []
        
        # Validate configuration schema
        config_result = self.schema_validator.validate_config(agent_path / "agent-config.yaml")
        results.append(config_result)
        
        # Validate knowledge base integrity
        knowledge_result = self.knowledge_validator.validate_knowledge(
            agent_path / "knowledge-base.json"
        )
        results.append(knowledge_result)
        
        # Validate agent performance
        performance_result = self.performance_validator.validate_performance(agent_path)
        results.append(performance_result)
        
        # Check cross-references and dependencies
        dependency_result = self.validate_dependencies(agent_path)
        results.append(dependency_result)
        
        return ValidationResult(
            success=all(r.success for r in results),
            results=results,
            overall_score=sum(r.score for r in results) / len(results)
        )
```

---

*This technical specification provides the detailed implementation framework for the modular agent management system with comprehensive architecture, integration patterns, and performance optimization strategies.*