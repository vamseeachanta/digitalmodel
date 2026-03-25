#!/usr/bin/env python3
"""
Enhanced Create-Spec with Agent Assignment System
Automatically assigns agents to tasks and creates specialist agents as needed
Maintains free agents for cost reduction
"""

import os
import yaml
import json
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Tuple
from enum import Enum

class AgentType(Enum):
    """Types of agents available"""
    GENERAL = "general-purpose"
    SPECIALIST = "specialist"
    FREE = "free-tier"
    DOMAIN = "domain-expert"
    MODULE = "module-specific"

class TaskComplexity(Enum):
    """Task complexity levels"""
    SIMPLE = "simple"          # < 30 min
    MODERATE = "moderate"      # 30 min - 2 hours
    COMPLEX = "complex"        # 2-6 hours
    EXTENSIVE = "extensive"    # > 6 hours

class AgentRegistry:
    """
    Manages agent assignments and creation for tasks
    Maintains registry of available agents and their specializations
    """
    
    def __init__(self, repo_path: Path):
        self.repo_path = repo_path
        self.agents_path = repo_path / "agents"
        self.registry_file = self.agents_path / "agent_registry.yaml"
        self.free_agents_file = self.agents_path / "free_agents.yaml"
        
        # Create directories
        self.agents_path.mkdir(parents=True, exist_ok=True)
        
        # Load registries
        self.registry = self.load_registry()
        self.free_agents = self.load_free_agents()
    
    def load_registry(self) -> Dict:
        """Load agent registry"""
        if self.registry_file.exists():
            with open(self.registry_file, 'r') as f:
                return yaml.safe_load(f) or {
                    "agents": {},
                    "assignments": {},
                    "specialist_areas": {},
                    "creation_log": []
                }
        return {
            "agents": {},
            "assignments": {},
            "specialist_areas": {},
            "creation_log": []
        }
    
    def load_free_agents(self) -> Dict:
        """Load free agents registry"""
        if self.free_agents_file.exists():
            with open(self.free_agents_file, 'r') as f:
                return yaml.safe_load(f) or {
                    "agents": {},
                    "task_mappings": {},
                    "last_refresh": None,
                    "usage_stats": {}
                }
        return {
            "agents": {},
            "task_mappings": {},
            "last_refresh": None,
            "usage_stats": {}
        }
    
    def save_registry(self):
        """Save agent registry"""
        with open(self.registry_file, 'w') as f:
            yaml.dump(self.registry, f, default_flow_style=False)
    
    def save_free_agents(self):
        """Save free agents registry"""
        with open(self.free_agents_file, 'w') as f:
            yaml.dump(self.free_agents, f, default_flow_style=False)
    
    def get_or_create_agent(self, task_domain: str, 
                           complexity: TaskComplexity,
                           specialization_needed: bool = False) -> Dict:
        """
        Get existing agent or create new specialist agent for task
        Returns agent assignment details
        """
        # Check for free agent first (cost optimization)
        if complexity in [TaskComplexity.SIMPLE, TaskComplexity.MODERATE]:
            free_agent = self.get_free_agent(task_domain)
            if free_agent:
                return free_agent
        
        # Check for existing specialist
        if specialization_needed:
            specialist = self.get_specialist_agent(task_domain)
            if specialist:
                return specialist
            
            # Create new specialist if needed
            return self.create_specialist_agent(task_domain, complexity)
        
        # Return general-purpose agent
        return self.get_general_agent()
    
    def get_free_agent(self, task_domain: str) -> Optional[Dict]:
        """Get available free agent for task"""
        # Check if free agents need refresh (weekly)
        if self.needs_refresh():
            self.refresh_free_agents()
        
        # Find matching free agent
        for agent_name, agent_info in self.free_agents["agents"].items():
            if task_domain.lower() in agent_info.get("capabilities", []):
                # Track usage
                if agent_name not in self.free_agents["usage_stats"]:
                    self.free_agents["usage_stats"][agent_name] = {
                        "uses": 0,
                        "last_used": None
                    }
                
                self.free_agents["usage_stats"][agent_name]["uses"] += 1
                self.free_agents["usage_stats"][agent_name]["last_used"] = datetime.now().isoformat()
                self.save_free_agents()
                
                return {
                    "name": agent_name,
                    "type": AgentType.FREE.value,
                    "cost": 0,
                    "capabilities": agent_info["capabilities"],
                    "description": f"Free agent for {task_domain}"
                }
        
        return None
    
    def get_specialist_agent(self, domain: str) -> Optional[Dict]:
        """Get existing specialist agent for domain"""
        domain_key = domain.lower().replace(" ", "_")
        
        if domain_key in self.registry["specialist_areas"]:
            agent_name = self.registry["specialist_areas"][domain_key]
            if agent_name in self.registry["agents"]:
                return self.registry["agents"][agent_name]
        
        return None
    
    def create_specialist_agent(self, domain: str, complexity: TaskComplexity) -> Dict:
        """Create new specialist agent for domain"""
        domain_key = domain.lower().replace(" ", "_")
        agent_name = f"{domain_key}_specialist"
        
        # Determine capabilities based on domain
        capabilities = self.determine_capabilities(domain, complexity)
        
        # Create agent configuration
        agent_config = {
            "name": agent_name,
            "type": AgentType.SPECIALIST.value,
            "domain": domain,
            "capabilities": capabilities,
            "created": datetime.now().isoformat(),
            "complexity_handled": complexity.value,
            "cost": self.estimate_cost(complexity),
            "description": f"Specialist agent for {domain} tasks"
        }
        
        # Register agent
        self.registry["agents"][agent_name] = agent_config
        self.registry["specialist_areas"][domain_key] = agent_name
        
        # Log creation
        self.registry["creation_log"].append({
            "agent": agent_name,
            "created_at": datetime.now().isoformat(),
            "reason": f"Specialist needed for {domain}",
            "complexity": complexity.value
        })
        
        self.save_registry()
        
        # Actually create the agent using create-module-agent
        self.invoke_create_module_agent(agent_name, domain, capabilities)
        
        return agent_config
    
    def get_general_agent(self) -> Dict:
        """Get general-purpose agent"""
        general_agent_name = "general_purpose"
        
        if general_agent_name not in self.registry["agents"]:
            # Create general agent if doesn't exist
            self.registry["agents"][general_agent_name] = {
                "name": general_agent_name,
                "type": AgentType.GENERAL.value,
                "capabilities": ["general", "research", "implementation", "testing"],
                "created": datetime.now().isoformat(),
                "cost": "standard",
                "description": "General-purpose agent for various tasks"
            }
            self.save_registry()
        
        return self.registry["agents"][general_agent_name]
    
    def determine_capabilities(self, domain: str, complexity: TaskComplexity) -> List[str]:
        """Determine agent capabilities based on domain and complexity"""
        base_capabilities = ["research", "implementation", "testing"]
        
        # Domain-specific capabilities
        domain_capabilities = {
            "database": ["sql", "schema_design", "optimization", "migrations"],
            "api": ["rest", "graphql", "authentication", "rate_limiting"],
            "frontend": ["react", "vue", "css", "responsive_design"],
            "backend": ["server", "microservices", "caching", "queuing"],
            "testing": ["unit_tests", "integration_tests", "e2e_tests", "mocking"],
            "documentation": ["technical_writing", "api_docs", "user_guides"],
            "devops": ["ci_cd", "deployment", "monitoring", "containerization"],
            "security": ["authentication", "authorization", "encryption", "audit"],
            "data": ["etl", "analytics", "visualization", "machine_learning"],
            "performance": ["optimization", "profiling", "caching", "scaling"]
        }
        
        # Add domain-specific capabilities
        for key, caps in domain_capabilities.items():
            if key in domain.lower():
                base_capabilities.extend(caps)
                break
        
        # Add complexity-based capabilities
        if complexity in [TaskComplexity.COMPLEX, TaskComplexity.EXTENSIVE]:
            base_capabilities.extend(["architecture", "system_design", "optimization"])
        
        return list(set(base_capabilities))
    
    def estimate_cost(self, complexity: TaskComplexity) -> str:
        """Estimate agent cost based on complexity"""
        cost_map = {
            TaskComplexity.SIMPLE: "low",
            TaskComplexity.MODERATE: "medium",
            TaskComplexity.COMPLEX: "high",
            TaskComplexity.EXTENSIVE: "premium"
        }
        return cost_map.get(complexity, "standard")
    
    def needs_refresh(self) -> bool:
        """Check if free agents need weekly refresh"""
        if not self.free_agents["last_refresh"]:
            return True
        
        last_refresh = datetime.fromisoformat(self.free_agents["last_refresh"])
        days_since = (datetime.now() - last_refresh).days
        
        return days_since >= 7
    
    def refresh_free_agents(self):
        """Refresh free agents weekly"""
        print("🔄 Refreshing free agents (weekly maintenance)...")
        
        # Define free agents for common tasks
        free_agent_templates = {
            "code_reviewer": {
                "capabilities": ["review", "testing", "quality", "simple"],
                "description": "Free agent for code review tasks"
            },
            "doc_writer": {
                "capabilities": ["documentation", "readme", "comments", "simple"],
                "description": "Free agent for documentation tasks"
            },
            "test_runner": {
                "capabilities": ["testing", "validation", "ci", "simple"],
                "description": "Free agent for test execution"
            },
            "formatter": {
                "capabilities": ["formatting", "linting", "style", "simple"],
                "description": "Free agent for code formatting"
            },
            "researcher": {
                "capabilities": ["research", "analysis", "exploration", "moderate"],
                "description": "Free agent for research tasks"
            },
            "debugger": {
                "capabilities": ["debugging", "troubleshooting", "fixes", "moderate"],
                "description": "Free agent for debugging tasks"
            }
        }
        
        # Update free agents
        self.free_agents["agents"] = free_agent_templates
        self.free_agents["last_refresh"] = datetime.now().isoformat()
        
        # Reset usage stats older than 30 days
        cutoff_date = datetime.now() - timedelta(days=30)
        for agent_name in list(self.free_agents["usage_stats"].keys()):
            last_used = self.free_agents["usage_stats"][agent_name].get("last_used")
            if last_used:
                last_used_date = datetime.fromisoformat(last_used)
                if last_used_date < cutoff_date:
                    del self.free_agents["usage_stats"][agent_name]
        
        self.save_free_agents()
        print("   ✓ Free agents refreshed")
    
    def invoke_create_module_agent(self, agent_name: str, domain: str, capabilities: List[str]):
        """
        Invoke create-module-agent command to actually create the agent
        This would call the v3.1 create-module-agent system
        """
        # This is a placeholder for actual agent creation
        # In production, this would call the create-module-agent command
        agent_path = self.agents_path / agent_name
        agent_path.mkdir(parents=True, exist_ok=True)
        
        # Create agent configuration
        config = {
            "name": agent_name,
            "domain": domain,
            "capabilities": capabilities,
            "created": datetime.now().isoformat(),
            "type": "specialist",
            "auto_created": True
        }
        
        with open(agent_path / "config.yaml", 'w') as f:
            yaml.dump(config, f)
        
        print(f"      ✓ Created specialist agent: {agent_name}")


class TaskAgentAssigner:
    """
    Assigns agents to tasks in create-spec
    Ensures optimal agent selection for each task
    """
    
    def __init__(self, spec_path: Path):
        self.spec_path = spec_path
        self.tasks_file = spec_path / "tasks.md"
        self.agent_registry = AgentRegistry(spec_path.parent.parent.parent)  # repo root
    
    def parse_task_complexity(self, time_estimate: str) -> TaskComplexity:
        """Parse task complexity from time estimate"""
        # Extract time from estimate like "[2-3 hours]"
        import re
        
        # Simple time patterns
        if "min" in time_estimate.lower():
            match = re.search(r'(\d+)', time_estimate)
            if match:
                minutes = int(match.group(1))
                if minutes <= 30:
                    return TaskComplexity.SIMPLE
                elif minutes <= 120:
                    return TaskComplexity.MODERATE
        
        if "hour" in time_estimate.lower():
            match = re.search(r'(\d+)', time_estimate)
            if match:
                hours = int(match.group(1))
                if hours <= 2:
                    return TaskComplexity.MODERATE
                elif hours <= 6:
                    return TaskComplexity.COMPLEX
                else:
                    return TaskComplexity.EXTENSIVE
        
        if "day" in time_estimate.lower():
            return TaskComplexity.EXTENSIVE
        
        # Default to moderate
        return TaskComplexity.MODERATE
    
    def determine_task_domain(self, task_description: str) -> str:
        """Determine task domain from description"""
        task_lower = task_description.lower()
        
        # Domain keywords mapping
        domain_keywords = {
            "database": ["database", "sql", "schema", "migration", "query"],
            "api": ["api", "endpoint", "rest", "graphql", "request"],
            "frontend": ["ui", "component", "react", "vue", "css", "html"],
            "backend": ["server", "backend", "service", "controller"],
            "testing": ["test", "spec", "mock", "validation"],
            "documentation": ["document", "readme", "docs", "comment"],
            "devops": ["deploy", "ci", "cd", "docker", "kubernetes"],
            "security": ["auth", "security", "encrypt", "permission"],
            "data": ["data", "etl", "analytics", "report"],
            "performance": ["optimize", "performance", "cache", "speed"]
        }
        
        # Find matching domain
        for domain, keywords in domain_keywords.items():
            if any(keyword in task_lower for keyword in keywords):
                return domain
        
        return "general"
    
    def assign_agents_to_tasks(self, tasks_content: str) -> str:
        """
        Parse tasks.md content and assign agents to each task
        Returns updated tasks content with agent assignments
        """
        lines = tasks_content.split('\n')
        updated_lines = []
        current_task = None
        
        for line in lines:
            # Check if this is a task line
            if line.strip().startswith('- [ ]'):
                # Extract task info
                import re
                
                # Pattern: - [ ] 1. Task description `[time estimate]`
                task_pattern = r'- \[ \] (\d+(?:\.\d+)?)\. (.*?)(?:`\[(.*?)\]`)?$'
                match = re.match(task_pattern, line.strip())
                
                if match:
                    task_num = match.group(1)
                    task_desc = match.group(2).strip()
                    time_est = match.group(3) or "1 hour"
                    
                    # Determine complexity and domain
                    complexity = self.parse_task_complexity(time_est)
                    domain = self.determine_task_domain(task_desc)
                    
                    # Check if specialization needed
                    needs_specialist = complexity in [TaskComplexity.COMPLEX, TaskComplexity.EXTENSIVE]
                    
                    # Get or create appropriate agent
                    agent = self.agent_registry.get_or_create_agent(
                        domain, complexity, needs_specialist
                    )
                    
                    # Add agent assignment to task line
                    agent_tag = f" 🤖 `Agent: {agent['name']} ({agent['type']})`"
                    
                    # Update line with agent assignment
                    if '`[' in line and ']`' in line:
                        # Insert before time estimate
                        line = line.replace(']`', f"]`{agent_tag}")
                    else:
                        # Add at end
                        line = line.rstrip() + agent_tag
            
            updated_lines.append(line)
        
        return '\n'.join(updated_lines)
    
    def update_tasks_with_agents(self):
        """Update tasks.md file with agent assignments"""
        if not self.tasks_file.exists():
            print("   ⚠️ tasks.md not found")
            return
        
        # Read current tasks
        with open(self.tasks_file, 'r') as f:
            content = f.read()
        
        # Assign agents
        updated_content = self.assign_agents_to_tasks(content)
        
        # Add agent summary section
        agent_summary = self.generate_agent_summary()
        
        # Insert summary after task list
        if "## Agent Assignments" not in updated_content:
            # Find a good place to insert (after task list, before any other section)
            insertion_point = updated_content.rfind('\n\n')
            if insertion_point > 0:
                updated_content = (
                    updated_content[:insertion_point] + 
                    "\n\n" + agent_summary + 
                    updated_content[insertion_point:]
                )
            else:
                updated_content += "\n\n" + agent_summary
        
        # Write updated content
        with open(self.tasks_file, 'w') as f:
            f.write(updated_content)
        
        print("   ✓ Agent assignments added to tasks.md")
    
    def generate_agent_summary(self) -> str:
        """Generate agent assignment summary"""
        summary = """## Agent Assignments

### Agent Types Used
| Agent | Type | Specialization | Tasks |
|-------|------|----------------|-------|
"""
        
        # Count agent usage
        agent_usage = {}
        for assignment in self.agent_registry.registry.get("assignments", {}).values():
            agent_name = assignment.get("agent", "unknown")
            if agent_name not in agent_usage:
                agent_usage[agent_name] = {
                    "count": 0,
                    "type": assignment.get("type", "general"),
                    "domain": assignment.get("domain", "general")
                }
            agent_usage[agent_name]["count"] += 1
        
        # Add usage stats
        for agent_name, usage in agent_usage.items():
            summary += f"| {agent_name} | {usage['type']} | {usage['domain']} | {usage['count']} |\n"
        
        # Add cost optimization note
        free_agent_count = sum(
            1 for a in self.agent_registry.registry.get("agents", {}).values()
            if a.get("type") == AgentType.FREE.value
        )
        
        if free_agent_count > 0:
            summary += f"\n### Cost Optimization\n"
            summary += f"- **Free Agents Used**: {free_agent_count}\n"
            summary += f"- **Estimated Savings**: ~{free_agent_count * 10}% reduction in compute costs\n"
            summary += f"- **Next Refresh**: Weekly (automatic)\n"
        
        summary += """
### Agent Creation Policy
- Simple tasks (< 30 min): Free agents when available
- Moderate tasks (30 min - 2 hours): General-purpose or free agents
- Complex tasks (2-6 hours): Specialist agents created as needed
- Extensive tasks (> 6 hours): Domain expert agents required

*Agents are automatically created and maintained by the system*
"""
        
        return summary


def enhance_create_spec_with_agents(spec_path: Path):
    """
    Main function to enhance create-spec with agent assignments
    Called after tasks.md is created
    """
    print("\n🤖 Assigning agents to tasks...")
    
    # Create task agent assigner
    assigner = TaskAgentAssigner(spec_path)
    
    # Update tasks with agent assignments
    assigner.update_tasks_with_agents()
    
    # Save agent registry
    assigner.agent_registry.save_registry()
    
    print("   ✓ Agent assignment complete")
    
    # Report on created agents
    creation_log = assigner.agent_registry.registry.get("creation_log", [])
    if creation_log:
        print(f"   ✓ Created {len(creation_log)} specialist agents:")
        for entry in creation_log[-3:]:  # Show last 3
            print(f"      • {entry['agent']} ({entry['complexity']})")


# Integration point with create-spec
def integrate_with_create_spec():
    """
    Integration code to be added to create-spec.md
    This would be called after tasks.md generation
    """
    integration_code = '''
# After creating tasks.md, assign agents
from enhanced_create_spec_with_agents import enhance_create_spec_with_agents

# Call after tasks.md is created
enhance_create_spec_with_agents(spec_path)
'''
    return integration_code


if __name__ == "__main__":
    # Example usage
    spec_path = Path(".agent-os/specs/2025-01-10-example-spec")
    if spec_path.exists():
        enhance_create_spec_with_agents(spec_path)