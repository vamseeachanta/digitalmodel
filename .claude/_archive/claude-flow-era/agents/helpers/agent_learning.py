#!/usr/bin/env python3
"""
Agent Learning System for Execute-Tasks
Continuously enhances agents with common and variation context
Agents improve with every project and task accomplished
"""

import os
import yaml
import json
import hashlib
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Tuple, Any
from enum import Enum
import pickle
import numpy as np

class ContextType(Enum):
    """Types of context for agent learning"""
    COMMON = "common"           # Patterns across all tasks
    VARIATION = "variation"     # Task-specific variations
    DOMAIN = "domain"           # Domain-specific knowledge
    ERROR = "error"             # Error patterns and solutions
    OPTIMIZATION = "optimization"  # Performance optimizations
    SUCCESS = "success"         # Successful patterns

class LearningLevel(Enum):
    """Agent learning maturity levels"""
    NOVICE = "novice"           # 0-10 tasks
    INTERMEDIATE = "intermediate"  # 11-50 tasks
    ADVANCED = "advanced"       # 51-200 tasks
    EXPERT = "expert"           # 200+ tasks
    MASTER = "master"           # 500+ tasks with high success

class AgentLearningSystem:
    """
    Implements continuous learning for agents
    Tracks common and variation context to improve agent performance
    """
    
    def __init__(self, repo_path: Path):
        self.repo_path = repo_path
        self.learning_path = repo_path / ".agent-os" / "agent_learning"
        self.context_path = self.learning_path / "context"
        self.knowledge_path = self.learning_path / "knowledge"
        self.metrics_path = self.learning_path / "metrics"
        
        # Create directories
        self.learning_path.mkdir(parents=True, exist_ok=True)
        self.context_path.mkdir(parents=True, exist_ok=True)
        self.knowledge_path.mkdir(parents=True, exist_ok=True)
        self.metrics_path.mkdir(parents=True, exist_ok=True)
        
        # Load or initialize learning database
        self.learning_db = self.load_learning_database()
        
        # Load agent profiles
        self.agent_profiles = self.load_agent_profiles()
    
    def load_learning_database(self) -> Dict:
        """Load or initialize the learning database"""
        db_file = self.learning_path / "learning_database.yaml"
        
        if db_file.exists():
            with open(db_file, 'r') as f:
                return yaml.safe_load(f) or self.get_default_database()
        return self.get_default_database()
    
    def get_default_database(self) -> Dict:
        """Get default learning database structure"""
        return {
            "version": "1.0.0",
            "created": datetime.now().isoformat(),
            "agents": {},
            "common_context": {
                "patterns": [],
                "best_practices": [],
                "anti_patterns": [],
                "optimizations": []
            },
            "variation_context": {
                "by_domain": {},
                "by_complexity": {},
                "by_technology": {},
                "by_outcome": {}
            },
            "learning_history": [],
            "global_metrics": {
                "total_tasks": 0,
                "success_rate": 0.0,
                "avg_efficiency": 0.0,
                "improvement_rate": 0.0
            }
        }
    
    def save_learning_database(self):
        """Save the learning database"""
        db_file = self.learning_path / "learning_database.yaml"
        with open(db_file, 'w') as f:
            yaml.dump(self.learning_db, f, default_flow_style=False)
    
    def load_agent_profiles(self) -> Dict:
        """Load agent profiles with their learning history"""
        profiles_file = self.learning_path / "agent_profiles.yaml"
        
        if profiles_file.exists():
            with open(profiles_file, 'r') as f:
                return yaml.safe_load(f) or {}
        return {}
    
    def save_agent_profiles(self):
        """Save agent profiles"""
        profiles_file = self.learning_path / "agent_profiles.yaml"
        with open(profiles_file, 'w') as f:
            yaml.dump(self.agent_profiles, f, default_flow_style=False)
    
    def extract_task_context(self, task_info: Dict) -> Tuple[Dict, Dict]:
        """
        Extract common and variation context from completed task
        Returns (common_context, variation_context)
        """
        common_context = {}
        variation_context = {}
        
        # Extract common patterns
        common_context = {
            "task_type": task_info.get("type", "general"),
            "duration": task_info.get("duration"),
            "complexity": task_info.get("complexity"),
            "technologies": task_info.get("technologies", []),
            "patterns_used": [],
            "success_factors": [],
            "challenges": []
        }
        
        # Identify patterns used
        if "implementation" in task_info:
            impl = task_info["implementation"]
            
            # Common patterns detection
            patterns = self.detect_patterns(impl)
            common_context["patterns_used"] = patterns
            
            # Success factors
            if task_info.get("outcome") == "success":
                common_context["success_factors"] = self.extract_success_factors(task_info)
            
            # Challenges faced
            if "errors" in task_info or "issues" in task_info:
                common_context["challenges"] = task_info.get("errors", []) + task_info.get("issues", [])
        
        # Extract variation context
        variation_context = {
            "domain_specific": {
                "domain": task_info.get("domain", "general"),
                "domain_patterns": self.extract_domain_patterns(task_info),
                "domain_tools": task_info.get("domain_tools", [])
            },
            "complexity_specific": {
                "level": task_info.get("complexity", "moderate"),
                "optimization_used": task_info.get("optimizations", []),
                "performance_metrics": task_info.get("performance", {})
            },
            "technology_specific": {
                "stack": task_info.get("tech_stack", []),
                "integrations": task_info.get("integrations", []),
                "dependencies": task_info.get("dependencies", [])
            },
            "outcome_specific": {
                "result": task_info.get("outcome", "unknown"),
                "efficiency_score": task_info.get("efficiency", 0.0),
                "quality_score": task_info.get("quality", 0.0),
                "lessons": task_info.get("lessons_learned", [])
            }
        }
        
        return common_context, variation_context
    
    def detect_patterns(self, implementation: Any) -> List[str]:
        """Detect common patterns in implementation"""
        patterns = []
        
        # Convert implementation to string for pattern matching
        impl_str = str(implementation).lower()
        
        # Common software patterns
        pattern_indicators = {
            "singleton": ["singleton", "instance", "get_instance"],
            "factory": ["factory", "create", "build"],
            "observer": ["observer", "notify", "subscribe"],
            "strategy": ["strategy", "algorithm", "policy"],
            "decorator": ["decorator", "wrapper", "enhance"],
            "mvc": ["model", "view", "controller"],
            "repository": ["repository", "data_access", "dao"],
            "dependency_injection": ["inject", "dependency", "container"],
            "async": ["async", "await", "promise", "future"],
            "caching": ["cache", "memoize", "store"],
            "validation": ["validate", "check", "verify"],
            "error_handling": ["try", "catch", "error", "exception"]
        }
        
        for pattern, indicators in pattern_indicators.items():
            if any(indicator in impl_str for indicator in indicators):
                patterns.append(pattern)
        
        return patterns
    
    def extract_success_factors(self, task_info: Dict) -> List[str]:
        """Extract factors that contributed to task success"""
        factors = []
        
        # Performance-based success
        if task_info.get("efficiency", 0) > 0.8:
            factors.append("high_efficiency")
        
        # Quality-based success
        if task_info.get("quality", 0) > 0.8:
            factors.append("high_quality")
        
        # Time-based success
        if task_info.get("completed_under_estimate", False):
            factors.append("completed_early")
        
        # Testing success
        if task_info.get("tests_passed", 0) == task_info.get("total_tests", 1):
            factors.append("all_tests_passed")
        
        # Clean implementation
        if not task_info.get("errors") and not task_info.get("warnings"):
            factors.append("clean_implementation")
        
        return factors
    
    def extract_domain_patterns(self, task_info: Dict) -> List[str]:
        """Extract domain-specific patterns"""
        domain = task_info.get("domain", "general")
        patterns = []
        
        # Domain-specific pattern detection
        domain_patterns = {
            "database": ["schema_design", "query_optimization", "indexing", "transactions"],
            "api": ["rest_patterns", "authentication", "rate_limiting", "versioning"],
            "frontend": ["component_patterns", "state_management", "routing", "responsive"],
            "backend": ["service_patterns", "middleware", "queuing", "caching"],
            "testing": ["test_patterns", "mocking", "fixtures", "coverage"],
            "security": ["auth_patterns", "encryption", "validation", "sanitization"],
            "devops": ["deployment_patterns", "ci_cd", "monitoring", "scaling"]
        }
        
        if domain in domain_patterns:
            # Check which patterns were likely used based on task details
            for pattern in domain_patterns[domain]:
                if pattern in str(task_info).lower():
                    patterns.append(pattern)
        
        return patterns
    
    def update_agent_knowledge(self, agent_name: str, 
                              common_context: Dict, 
                              variation_context: Dict,
                              task_outcome: Dict):
        """
        Update agent's knowledge base with new learning
        """
        # Initialize agent profile if new
        if agent_name not in self.agent_profiles:
            self.agent_profiles[agent_name] = {
                "created": datetime.now().isoformat(),
                "learning_level": LearningLevel.NOVICE.value,
                "tasks_completed": 0,
                "success_rate": 0.0,
                "knowledge_base": {
                    "common_patterns": {},
                    "variation_patterns": {},
                    "domain_expertise": {},
                    "error_recovery": {},
                    "optimizations": {}
                },
                "performance_history": [],
                "improvement_metrics": {}
            }
        
        agent = self.agent_profiles[agent_name]
        
        # Update task count
        agent["tasks_completed"] += 1
        
        # Update common patterns knowledge
        for pattern in common_context.get("patterns_used", []):
            if pattern not in agent["knowledge_base"]["common_patterns"]:
                agent["knowledge_base"]["common_patterns"][pattern] = {
                    "count": 0,
                    "success_rate": 0.0,
                    "avg_efficiency": 0.0
                }
            
            pattern_info = agent["knowledge_base"]["common_patterns"][pattern]
            pattern_info["count"] += 1
            
            # Update success rate
            if task_outcome.get("success"):
                pattern_info["success_rate"] = (
                    (pattern_info["success_rate"] * (pattern_info["count"] - 1) + 1) / 
                    pattern_info["count"]
                )
            
            # Update efficiency
            efficiency = task_outcome.get("efficiency", 0.5)
            pattern_info["avg_efficiency"] = (
                (pattern_info["avg_efficiency"] * (pattern_info["count"] - 1) + efficiency) / 
                pattern_info["count"]
            )
        
        # Update variation patterns
        domain = variation_context["domain_specific"]["domain"]
        if domain not in agent["knowledge_base"]["variation_patterns"]:
            agent["knowledge_base"]["variation_patterns"][domain] = {
                "count": 0,
                "patterns": [],
                "best_practices": [],
                "common_issues": []
            }
        
        domain_knowledge = agent["knowledge_base"]["variation_patterns"][domain]
        domain_knowledge["count"] += 1
        
        # Add new patterns
        for pattern in variation_context["domain_specific"]["domain_patterns"]:
            if pattern not in domain_knowledge["patterns"]:
                domain_knowledge["patterns"].append(pattern)
        
        # Update domain expertise
        if domain not in agent["knowledge_base"]["domain_expertise"]:
            agent["knowledge_base"]["domain_expertise"][domain] = {
                "level": "beginner",
                "tasks": 0,
                "success_rate": 0.0
            }
        
        domain_exp = agent["knowledge_base"]["domain_expertise"][domain]
        domain_exp["tasks"] += 1
        
        # Update expertise level
        if domain_exp["tasks"] >= 50:
            domain_exp["level"] = "expert"
        elif domain_exp["tasks"] >= 20:
            domain_exp["level"] = "advanced"
        elif domain_exp["tasks"] >= 5:
            domain_exp["level"] = "intermediate"
        
        # Learn from errors
        if "challenges" in common_context and common_context["challenges"]:
            for challenge in common_context["challenges"]:
                challenge_key = self.normalize_challenge(challenge)
                if challenge_key not in agent["knowledge_base"]["error_recovery"]:
                    agent["knowledge_base"]["error_recovery"][challenge_key] = {
                        "occurrences": 0,
                        "solutions": [],
                        "prevention": []
                    }
                
                agent["knowledge_base"]["error_recovery"][challenge_key]["occurrences"] += 1
                
                # Add solution if task was successful despite challenge
                if task_outcome.get("success"):
                    solution = task_outcome.get("solution_approach", "standard_approach")
                    if solution not in agent["knowledge_base"]["error_recovery"][challenge_key]["solutions"]:
                        agent["knowledge_base"]["error_recovery"][challenge_key]["solutions"].append(solution)
        
        # Learn optimizations
        if variation_context["complexity_specific"]["optimization_used"]:
            for optimization in variation_context["complexity_specific"]["optimization_used"]:
                if optimization not in agent["knowledge_base"]["optimizations"]:
                    agent["knowledge_base"]["optimizations"][optimization] = {
                        "usage_count": 0,
                        "avg_improvement": 0.0,
                        "applicable_contexts": []
                    }
                
                opt_info = agent["knowledge_base"]["optimizations"][optimization]
                opt_info["usage_count"] += 1
                
                # Track performance improvement
                improvement = task_outcome.get("performance_improvement", 0.0)
                opt_info["avg_improvement"] = (
                    (opt_info["avg_improvement"] * (opt_info["usage_count"] - 1) + improvement) / 
                    opt_info["usage_count"]
                )
                
                # Track applicable contexts
                context = f"{domain}_{variation_context['complexity_specific']['level']}"
                if context not in opt_info["applicable_contexts"]:
                    opt_info["applicable_contexts"].append(context)
        
        # Update learning level
        self.update_learning_level(agent)
        
        # Track performance history
        agent["performance_history"].append({
            "task_id": task_outcome.get("task_id"),
            "timestamp": datetime.now().isoformat(),
            "success": task_outcome.get("success"),
            "efficiency": task_outcome.get("efficiency"),
            "quality": task_outcome.get("quality")
        })
        
        # Keep only last 100 entries for performance
        if len(agent["performance_history"]) > 100:
            agent["performance_history"] = agent["performance_history"][-100:]
        
        # Calculate improvement metrics
        self.calculate_improvement_metrics(agent)
        
        # Save updated profile
        self.save_agent_profiles()
    
    def normalize_challenge(self, challenge: str) -> str:
        """Normalize challenge description for consistent tracking"""
        # Simple normalization - can be enhanced with NLP
        return challenge.lower().replace(" ", "_").replace("-", "_")[:50]
    
    def update_learning_level(self, agent: Dict):
        """Update agent's learning level based on experience"""
        tasks = agent["tasks_completed"]
        success_rate = agent.get("success_rate", 0.0)
        
        if tasks >= 500 and success_rate > 0.9:
            agent["learning_level"] = LearningLevel.MASTER.value
        elif tasks >= 200:
            agent["learning_level"] = LearningLevel.EXPERT.value
        elif tasks >= 50:
            agent["learning_level"] = LearningLevel.ADVANCED.value
        elif tasks >= 10:
            agent["learning_level"] = LearningLevel.INTERMEDIATE.value
        else:
            agent["learning_level"] = LearningLevel.NOVICE.value
    
    def calculate_improvement_metrics(self, agent: Dict):
        """Calculate agent's improvement over time"""
        history = agent["performance_history"]
        
        if len(history) < 2:
            return
        
        # Calculate recent vs. early performance
        recent_tasks = history[-10:] if len(history) >= 10 else history[-len(history)//2:]
        early_tasks = history[:10] if len(history) >= 20 else history[:len(history)//2]
        
        recent_success = sum(1 for t in recent_tasks if t.get("success")) / len(recent_tasks)
        early_success = sum(1 for t in early_tasks if t.get("success")) / len(early_tasks) if early_tasks else 0
        
        recent_efficiency = sum(t.get("efficiency", 0) for t in recent_tasks) / len(recent_tasks)
        early_efficiency = sum(t.get("efficiency", 0) for t in early_tasks) / len(early_tasks) if early_tasks else 0
        
        agent["improvement_metrics"] = {
            "success_improvement": recent_success - early_success,
            "efficiency_improvement": recent_efficiency - early_efficiency,
            "learning_rate": (recent_success - early_success) / max(len(history), 1),
            "current_performance": recent_success,
            "trend": "improving" if recent_success > early_success else "stable" if recent_success == early_success else "declining"
        }
    
    def get_agent_recommendations(self, agent_name: str, task_type: str) -> Dict:
        """
        Get recommendations for agent based on learned knowledge
        """
        if agent_name not in self.agent_profiles:
            return {
                "level": "novice",
                "recommendations": ["No prior experience - use standard approaches"],
                "patterns": [],
                "optimizations": [],
                "warnings": []
            }
        
        agent = self.agent_profiles[agent_name]
        recommendations = {
            "level": agent["learning_level"],
            "recommendations": [],
            "patterns": [],
            "optimizations": [],
            "warnings": []
        }
        
        # Recommend successful patterns
        successful_patterns = [
            pattern for pattern, info in agent["knowledge_base"]["common_patterns"].items()
            if info["success_rate"] > 0.8 and info["count"] > 3
        ]
        recommendations["patterns"] = successful_patterns[:5]  # Top 5 patterns
        
        # Recommend optimizations
        effective_optimizations = [
            opt for opt, info in agent["knowledge_base"]["optimizations"].items()
            if info["avg_improvement"] > 0.1 and info["usage_count"] > 2
        ]
        recommendations["optimizations"] = effective_optimizations[:3]  # Top 3 optimizations
        
        # Provide warnings based on past errors
        common_errors = [
            error for error, info in agent["knowledge_base"]["error_recovery"].items()
            if info["occurrences"] > 2
        ]
        if common_errors:
            recommendations["warnings"] = [
                f"Watch for: {error} (occurred {agent['knowledge_base']['error_recovery'][error]['occurrences']} times)"
                for error in common_errors[:3]
            ]
        
        # Specific recommendations based on learning level
        if agent["learning_level"] == LearningLevel.MASTER.value:
            recommendations["recommendations"].append("Apply advanced optimization techniques")
            recommendations["recommendations"].append("Consider novel approaches for edge cases")
        elif agent["learning_level"] == LearningLevel.EXPERT.value:
            recommendations["recommendations"].append("Leverage domain expertise for optimization")
            recommendations["recommendations"].append("Apply learned patterns confidently")
        elif agent["learning_level"] == LearningLevel.ADVANCED.value:
            recommendations["recommendations"].append("Use proven patterns from knowledge base")
            recommendations["recommendations"].append("Apply optimizations where applicable")
        elif agent["learning_level"] == LearningLevel.INTERMEDIATE.value:
            recommendations["recommendations"].append("Follow established patterns")
            recommendations["recommendations"].append("Be cautious with complex optimizations")
        else:  # NOVICE
            recommendations["recommendations"].append("Follow standard best practices")
            recommendations["recommendations"].append("Focus on correctness over optimization")
        
        return recommendations
    
    def share_learning_across_agents(self):
        """
        Share successful patterns across all agents
        Implements collective learning
        """
        # Collect all successful patterns
        global_patterns = {}
        
        for agent_name, agent in self.agent_profiles.items():
            for pattern, info in agent["knowledge_base"]["common_patterns"].items():
                if info["success_rate"] > 0.8 and info["count"] > 5:
                    if pattern not in global_patterns:
                        global_patterns[pattern] = {
                            "agents_using": [],
                            "total_success_rate": 0.0,
                            "total_count": 0
                        }
                    
                    global_patterns[pattern]["agents_using"].append(agent_name)
                    global_patterns[pattern]["total_count"] += info["count"]
                    # Weighted average success rate
                    global_patterns[pattern]["total_success_rate"] = (
                        (global_patterns[pattern]["total_success_rate"] * 
                         (global_patterns[pattern]["total_count"] - info["count"]) +
                         info["success_rate"] * info["count"]) /
                        global_patterns[pattern]["total_count"]
                    )
        
        # Update common context in learning database
        self.learning_db["common_context"]["patterns"] = [
            {
                "pattern": pattern,
                "success_rate": info["total_success_rate"],
                "usage_count": info["total_count"],
                "agents": info["agents_using"]
            }
            for pattern, info in global_patterns.items()
        ]
        
        # Identify best practices
        best_practices = [
            pattern for pattern, info in global_patterns.items()
            if info["total_success_rate"] > 0.9 and len(info["agents_using"]) > 2
        ]
        
        self.learning_db["common_context"]["best_practices"] = best_practices
        
        # Save updated database
        self.save_learning_database()
    
    def generate_learning_report(self, agent_name: Optional[str] = None) -> str:
        """Generate a learning report for an agent or all agents"""
        report = ["# Agent Learning Report\n"]
        report.append(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M')}\n")
        
        if agent_name and agent_name in self.agent_profiles:
            # Single agent report
            agent = self.agent_profiles[agent_name]
            report.append(f"## Agent: {agent_name}\n")
            report.append(f"- **Level**: {agent['learning_level']}")
            report.append(f"- **Tasks Completed**: {agent['tasks_completed']}")
            report.append(f"- **Success Rate**: {agent.get('success_rate', 0):.1%}")
            
            # Top patterns
            report.append("\n### Top Patterns Used")
            top_patterns = sorted(
                agent["knowledge_base"]["common_patterns"].items(),
                key=lambda x: x[1]["count"],
                reverse=True
            )[:5]
            for pattern, info in top_patterns:
                report.append(f"- {pattern}: {info['count']} uses, {info['success_rate']:.1%} success")
            
            # Domain expertise
            report.append("\n### Domain Expertise")
            for domain, exp in agent["knowledge_base"]["domain_expertise"].items():
                report.append(f"- {domain}: {exp['level']} ({exp['tasks']} tasks)")
            
            # Improvement metrics
            if "improvement_metrics" in agent:
                metrics = agent["improvement_metrics"]
                report.append("\n### Improvement Metrics")
                report.append(f"- **Success Improvement**: {metrics['success_improvement']:+.1%}")
                report.append(f"- **Efficiency Improvement**: {metrics['efficiency_improvement']:+.1%}")
                report.append(f"- **Trend**: {metrics['trend']}")
        else:
            # Global report
            report.append("## Global Learning Statistics\n")
            report.append(f"- **Total Agents**: {len(self.agent_profiles)}")
            report.append(f"- **Total Tasks**: {self.learning_db['global_metrics']['total_tasks']}")
            report.append(f"- **Global Success Rate**: {self.learning_db['global_metrics']['success_rate']:.1%}")
            
            # Best practices
            report.append("\n### Discovered Best Practices")
            for practice in self.learning_db["common_context"]["best_practices"][:10]:
                report.append(f"- {practice}")
            
            # Top performing agents
            report.append("\n### Top Performing Agents")
            top_agents = sorted(
                self.agent_profiles.items(),
                key=lambda x: x[1].get("success_rate", 0),
                reverse=True
            )[:5]
            for name, agent in top_agents:
                report.append(f"- {name}: {agent['learning_level']}, {agent.get('success_rate', 0):.1%} success")
        
        return "\n".join(report)


class ExecuteTasksEnhancement:
    """
    Enhancement for execute-tasks command to include agent learning
    """
    
    def __init__(self, repo_path: Path):
        self.repo_path = repo_path
        self.learning_system = AgentLearningSystem(repo_path)
    
    def enhance_task_execution(self, task_info: Dict, agent_name: str) -> Dict:
        """
        Enhance task execution with learning capabilities
        Called before task execution
        """
        # Get agent recommendations based on learning
        recommendations = self.learning_system.get_agent_recommendations(
            agent_name, 
            task_info.get("type", "general")
        )
        
        # Enhance task info with recommendations
        task_info["agent_recommendations"] = recommendations
        task_info["agent_level"] = recommendations["level"]
        
        # Add suggested patterns
        if recommendations["patterns"]:
            task_info["suggested_patterns"] = recommendations["patterns"]
            print(f"📚 Agent {agent_name} suggests using: {', '.join(recommendations['patterns'])}")
        
        # Add optimization suggestions
        if recommendations["optimizations"]:
            task_info["suggested_optimizations"] = recommendations["optimizations"]
            print(f"⚡ Recommended optimizations: {', '.join(recommendations['optimizations'])}")
        
        # Add warnings
        if recommendations["warnings"]:
            task_info["warnings"] = recommendations["warnings"]
            print(f"⚠️ Warnings from past experience:")
            for warning in recommendations["warnings"]:
                print(f"   - {warning}")
        
        return task_info
    
    def process_task_completion(self, task_info: Dict, 
                               task_result: Dict, 
                               agent_name: str):
        """
        Process completed task for learning
        Called after task execution
        """
        print(f"\n📖 Processing task for agent learning...")
        
        # Extract context from completed task
        common_context, variation_context = self.learning_system.extract_task_context(task_info)
        
        # Prepare task outcome
        task_outcome = {
            "task_id": task_info.get("id", f"task_{datetime.now().timestamp()}"),
            "success": task_result.get("success", False),
            "efficiency": task_result.get("efficiency", 0.5),
            "quality": task_result.get("quality", 0.5),
            "performance_improvement": task_result.get("performance_improvement", 0.0),
            "solution_approach": task_result.get("approach", "standard")
        }
        
        # Update agent knowledge
        self.learning_system.update_agent_knowledge(
            agent_name,
            common_context,
            variation_context,
            task_outcome
        )
        
        # Share learning across agents
        self.learning_system.share_learning_across_agents()
        
        # Get agent's updated profile
        agent = self.learning_system.agent_profiles[agent_name]
        
        print(f"   ✓ Agent {agent_name} learning updated")
        print(f"   📊 Level: {agent['learning_level']}")
        print(f"   📈 Tasks completed: {agent['tasks_completed']}")
        
        # Show improvement if available
        if "improvement_metrics" in agent:
            metrics = agent["improvement_metrics"]
            if metrics["success_improvement"] > 0:
                print(f"   🎯 Success rate improved by {metrics['success_improvement']:+.1%}")
            if metrics["efficiency_improvement"] > 0:
                print(f"   ⚡ Efficiency improved by {metrics['efficiency_improvement']:+.1%}")
        
        # Generate and save learning report
        report = self.learning_system.generate_learning_report(agent_name)
        report_file = self.learning_system.learning_path / f"report_{agent_name}_{datetime.now().strftime('%Y%m%d')}.md"
        with open(report_file, 'w') as f:
            f.write(report)
        
        print(f"   📄 Learning report saved: {report_file.name}")


def integrate_with_execute_tasks() -> str:
    """
    Generate integration code for execute-tasks command
    """
    return '''
# Integration with execute-tasks command

from agent_learning_system import ExecuteTasksEnhancement

# Initialize enhancement
enhancement = ExecuteTasksEnhancement(Path.cwd())

# Before task execution
task_info = enhancement.enhance_task_execution(task_info, agent_name)

# After task execution
enhancement.process_task_completion(task_info, task_result, agent_name)

# Agents now learn and improve with every task!
'''


if __name__ == "__main__":
    # Example usage
    print("Agent Learning System initialized")
    print("Agents will now improve with every task!")
    print("\nFeatures:")
    print("- Common context accumulation")
    print("- Variation pattern tracking")
    print("- Cross-agent knowledge sharing")
    print("- Performance improvement metrics")
    print("- Automatic best practice discovery")