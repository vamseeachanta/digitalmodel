#!/usr/bin/env python3
"""
AI Agent Manager - Interface to AITmpl and Claude Code Templates agents

This command provides access to 48+ specialized AI agents for various
development tasks including security, performance, testing, and more.

Usage:
    /ai-agent list                     # List all available agents
    /ai-agent recommend [context]      # Get agent recommendations
    /ai-agent use [agent-name]         # Use specific agent
    /ai-agent info [agent-name]        # Get agent details
"""

import os
import sys
import yaml
import argparse
from pathlib import Path
from typing import Dict, List, Optional
from dataclasses import dataclass

@dataclass
class Agent:
    """Represents an AI agent."""
    name: str
    category: str
    when_to_use: str
    where: str
    capabilities: List[str]
    integration_points: List[str]

class AIAgentManager:
    """Manages AI agents from AITmpl and Claude Code Templates."""
    
    def __init__(self):
        self.catalog_path = Path("/mnt/github/github/.agent-os/resources/aitmpl_agents_catalog.yaml")
        self.catalog = self._load_catalog()
        
    def _load_catalog(self) -> Dict:
        """Load agent catalog."""
        if self.catalog_path.exists():
            with open(self.catalog_path, 'r') as f:
                return yaml.safe_load(f)
        return {}
    
    def list_agents(self, category: str = None):
        """List available agents."""
        print("\n🤖 Available AI Agents\n")
        print("=" * 60)
        
        total_count = 0
        
        for cat_name, cat_data in self.catalog.get('agent_categories', {}).items():
            if category and category != cat_name:
                continue
                
            print(f"\n📁 {cat_name.upper().replace('_', ' ')}")
            print("-" * 40)
            
            for agent in cat_data.get('agents', []):
                print(f"\n  🔹 {agent['name']}")
                print(f"     When: {agent['when_to_use']}")
                print(f"     Where: {agent['where']}")
                total_count += 1
        
        print(f"\n\nTotal: {total_count} agents available")
        print("\nUse '/ai-agent info [agent-name]' for details")
        print("Use '/ai-agent recommend' for context-based suggestions")
    
    def recommend_agents(self, context: str = None) -> List[Agent]:
        """Recommend agents based on context."""
        recommendations = []
        
        # Analyze context
        if not context:
            # Try to infer from current work
            context = self._infer_context()
        
        context_lower = context.lower() if context else ""
        
        print(f"\n🔍 Analyzing context: {context or 'current project'}")
        print("\n📋 Recommended Agents:\n")
        
        # Match agents based on context keywords
        keyword_map = {
            'api': ['API Security Audit Agent', 'API Documentation Agent'],
            'security': ['API Security Audit Agent', 'Penetration Testing Agent'],
            'performance': ['React Performance Optimization Agent', 'Database Optimization Agent'],
            'test': ['Test Generation Agent', 'E2E Testing Agent'],
            'database': ['Database Optimization Agent'],
            'react': ['React Performance Optimization Agent'],
            'refactor': ['Refactoring Agent', 'Code Review Agent'],
            'deploy': ['CI/CD Pipeline Agent', 'Infrastructure as Code Agent'],
            'data': ['Data Analysis Agent', 'ETL Pipeline Agent'],
            'document': ['Code Documentation Agent', 'API Documentation Agent']
        }
        
        matched_agents = set()
        
        for keyword, agents in keyword_map.items():
            if keyword in context_lower:
                matched_agents.update(agents)
        
        # Get full agent info
        for cat_data in self.catalog.get('agent_categories', {}).values():
            for agent in cat_data.get('agents', []):
                if agent['name'] in matched_agents:
                    print(f"  ✅ {agent['name']}")
                    print(f"     {agent['when_to_use']}")
                    print()
                    recommendations.append(agent)
        
        if not recommendations:
            print("  ℹ️  No specific recommendations. Showing general agents:\n")
            self._show_general_agents()
        
        return recommendations
    
    def _infer_context(self) -> str:
        """Infer context from current project state."""
        context_parts = []
        
        # Check for spec files
        specs_path = Path.cwd() / "specs"
        if specs_path.exists():
            recent_specs = sorted(specs_path.glob("**/spec.md"), 
                                key=lambda p: p.stat().st_mtime, reverse=True)
            if recent_specs:
                spec_name = recent_specs[0].parent.name
                context_parts.append(spec_name)
        
        # Check for recent git commits
        try:
            import subprocess
            result = subprocess.run(
                ["git", "log", "-1", "--pretty=%B"],
                capture_output=True, text=True
            )
            if result.returncode == 0:
                context_parts.append(result.stdout.strip())
        except:
            pass
        
        return " ".join(context_parts) if context_parts else "general development"
    
    def _show_general_agents(self):
        """Show generally useful agents."""
        general_agents = [
            "Code Review Agent",
            "Test Generation Agent",
            "Code Documentation Agent",
            "Refactoring Agent"
        ]
        
        for agent_name in general_agents:
            for cat_data in self.catalog.get('agent_categories', {}).values():
                for agent in cat_data.get('agents', []):
                    if agent['name'] == agent_name:
                        print(f"  • {agent['name']}")
                        print(f"    {agent['when_to_use']}")
                        print()
                        break
    
    def get_agent_info(self, agent_name: str):
        """Get detailed information about an agent."""
        found = False
        
        for cat_name, cat_data in self.catalog.get('agent_categories', {}).items():
            for agent in cat_data.get('agents', []):
                if agent_name.lower() in agent['name'].lower():
                    found = True
                    print(f"\n🤖 {agent['name']}")
                    print("=" * 60)
                    print(f"\n📁 Category: {cat_name.replace('_', ' ').title()}")
                    print(f"\n📋 When to Use:\n   {agent['when_to_use']}")
                    print(f"\n📍 Where:\n   {agent['where']}")
                    
                    print("\n💡 Capabilities:")
                    for cap in agent.get('capabilities', []):
                        print(f"   • {cap}")
                    
                    print("\n🔗 Integration Points:")
                    for point in agent.get('integration_points', []):
                        print(f"   • {point}")
                    
                    # Show related commands
                    self._show_related_commands(agent['name'])
                    
                    print("\n" + "=" * 60)
                    break
        
        if not found:
            print(f"\n❌ Agent '{agent_name}' not found")
            print("\nUse '/ai-agent list' to see all available agents")
    
    def _show_related_commands(self, agent_name: str):
        """Show commands that use this agent."""
        print("\n📌 Related Commands:")
        
        for cmd_category in self.catalog.get('available_commands', {}).values():
            for cmd in cmd_category:
                if cmd.get('agent_used') == agent_name:
                    print(f"   • {cmd['command']}: {cmd['description']}")
    
    def use_agent(self, agent_name: str, task: str = None):
        """Use a specific agent for a task."""
        print(f"\n🚀 Activating {agent_name}...")
        
        # Find agent
        agent_found = None
        for cat_data in self.catalog.get('agent_categories', {}).values():
            for agent in cat_data.get('agents', []):
                if agent_name.lower() in agent['name'].lower():
                    agent_found = agent
                    break
        
        if not agent_found:
            print(f"❌ Agent '{agent_name}' not found")
            return
        
        print(f"\n📋 Agent: {agent_found['name']}")
        print(f"   Task: {task or 'General assistance'}")
        
        # Provide agent-specific guidance
        print("\n💡 Agent Guidance:")
        for cap in agent_found.get('capabilities', []):
            print(f"   • Can help with: {cap}")
        
        print("\n🔧 To use this agent effectively:")
        print(f"   1. Ensure you're in the right context: {agent_found['where']}")
        print(f"   2. Best used when: {agent_found['when_to_use']}")
        print(f"   3. Integration points: {', '.join(agent_found.get('integration_points', []))}")
        
        print("\n✅ Agent is ready. Provide your specific requirements.")
    
    def show_workflow(self, workflow_type: str = None):
        """Show how agents work together in workflows."""
        print("\n🔄 Agent Workflows\n")
        
        workflows = self.catalog.get('usage_in_agent_os', {})
        
        if workflow_type:
            if workflow_type in workflows:
                self._display_workflow(workflow_type, workflows[workflow_type])
            else:
                print(f"❌ Workflow '{workflow_type}' not found")
        else:
            for wf_name, wf_data in workflows.items():
                self._display_workflow(wf_name, wf_data)
                print()
    
    def _display_workflow(self, name: str, workflow: Dict):
        """Display a specific workflow."""
        print(f"📊 {name.upper().replace('_', ' ')}")
        print(f"   {workflow.get('description', '')}")
        print("\n   Steps:")
        
        for step in workflow.get('workflow', []):
            print(f"   {step['step']}:")
            for agent in step.get('agents', []):
                print(f"      • {agent}")

def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        prog='ai-agent',
        description='AI Agent Manager for development tasks'
    )
    
    parser.add_argument('command', nargs='?', default='help',
                       choices=['list', 'recommend', 'info', 'use', 'workflow', 'help'])
    parser.add_argument('target', nargs='?', help='Agent name or context')
    parser.add_argument('--category', help='Filter by category')
    parser.add_argument('--task', help='Specific task for agent')
    
    args = parser.parse_args()
    
    manager = AIAgentManager()
    
    if args.command == 'list':
        manager.list_agents(category=args.category)
    
    elif args.command == 'recommend':
        manager.recommend_agents(context=args.target)
    
    elif args.command == 'info':
        if not args.target:
            print("❌ Please specify an agent name")
            print("Usage: /ai-agent info [agent-name]")
        else:
            manager.get_agent_info(args.target)
    
    elif args.command == 'use':
        if not args.target:
            print("❌ Please specify an agent name")
            print("Usage: /ai-agent use [agent-name]")
        else:
            manager.use_agent(args.target, task=args.task)
    
    elif args.command == 'workflow':
        manager.show_workflow(workflow_type=args.target)
    
    else:  # help
        print("""
🤖 AI Agent Manager

Access 48+ specialized AI agents for development tasks.

Usage: /ai-agent [command] [options]

Commands:
  list [--category CAT]     List all available agents
  recommend [CONTEXT]        Get agent recommendations
  info AGENT                 Show agent details
  use AGENT [--task TASK]    Use specific agent
  workflow [TYPE]            Show agent workflows
  help                       Show this help

Categories:
  • security - Security audit and testing agents
  • performance - Optimization agents
  • code_quality - Review and refactoring agents
  • testing - Test generation and execution
  • documentation - Doc generation agents
  • devops - CI/CD and infrastructure agents
  • data_engineering - Data pipeline agents

Examples:
  /ai-agent list --category security
  /ai-agent recommend "api development"
  /ai-agent info "API Security Audit Agent"
  /ai-agent use "Test Generation Agent" --task "unit tests for auth"
  /ai-agent workflow spec_creation

Integration:
  Agents automatically integrate with:
  • /spec create - Get recommendations during spec creation
  • /task execute - Use agents during implementation
  • /test run - Leverage testing agents
  • /project - Use DevOps agents

Source: AITmpl (https://www.aitmpl.com/)
        Claude Code Templates (https://github.com/davila7/claude-code-templates)
""")

if __name__ == '__main__':
    main()