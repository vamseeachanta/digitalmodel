"""Prompt Evolution Tracking for Agent OS.

This module tracks prompt performance and evolution to optimize 
agent responses over time.
"""

import json
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List, Optional
from dataclasses import dataclass, asdict


@dataclass
class PromptEntry:
    """Single prompt evolution entry."""
    
    timestamp: str
    prompt_content: str
    template_name: str
    parameters: Dict[str, Any]
    response_quality: float
    execution_time: float
    tokens_used: int
    improvements: List[str]
    context_size: int
    success: bool


class PromptEvolutionTracker:
    """Track and optimize prompt evolution for agents."""
    
    def __init__(self, agent_path: Path):
        """Initialize prompt evolution tracker.
        
        Args:
            agent_path: Path to the agent directory
        """
        self.agent_path = agent_path
        self.context_dir = agent_path / "context"
        self.evolution_file = self.context_dir / "prompt_evolution.md"
        self.data_file = self.context_dir / "prompt_evolution.json"
        self.prompt_history: List[PromptEntry] = []
        
        # Create context directory if it doesn't exist
        self.context_dir.mkdir(parents=True, exist_ok=True)
        
        # Load existing history
        self._load_history()
    
    def _load_history(self) -> None:
        """Load existing prompt history."""
        if self.data_file.exists():
            try:
                with open(self.data_file, 'r') as f:
                    data = json.load(f)
                    self.prompt_history = [
                        PromptEntry(**entry) for entry in data
                    ]
            except (json.JSONDecodeError, TypeError):
                self.prompt_history = []
    
    def _save_history(self) -> None:
        """Save prompt history to file."""
        with open(self.data_file, 'w') as f:
            json.dump(
                [asdict(entry) for entry in self.prompt_history],
                f, indent=2
            )
    
    def track_prompt(self, 
                    prompt_content: str,
                    template_name: str,
                    parameters: Dict[str, Any],
                    response_quality: float,
                    execution_time: float,
                    tokens_used: int,
                    context_size: int = 0,
                    success: bool = True) -> None:
        """Track a prompt execution.
        
        Args:
            prompt_content: The actual prompt content
            template_name: Name of the template used
            parameters: Parameters passed to the template
            response_quality: Quality score (0.0-1.0)
            execution_time: Time taken to execute
            tokens_used: Number of tokens consumed
            context_size: Size of context provided
            success: Whether the prompt execution succeeded
        """
        # Generate improvement suggestions
        improvements = self._suggest_improvements(
            prompt_content, response_quality, execution_time, 
            tokens_used, success
        )
        
        # Create entry
        entry = PromptEntry(
            timestamp=datetime.now().isoformat(),
            prompt_content=prompt_content,
            template_name=template_name,
            parameters=parameters,
            response_quality=response_quality,
            execution_time=execution_time,
            tokens_used=tokens_used,
            improvements=improvements,
            context_size=context_size,
            success=success
        )
        
        # Add to history
        self.prompt_history.append(entry)
        
        # Keep only last 100 entries
        if len(self.prompt_history) > 100:
            self.prompt_history = self.prompt_history[-100:]
        
        # Save and update evolution file
        self._save_history()
        self._update_evolution_file()
    
    def _suggest_improvements(self, 
                            prompt_content: str,
                            response_quality: float,
                            execution_time: float,
                            tokens_used: int,
                            success: bool) -> List[str]:
        """Suggest improvements based on prompt performance.
        
        Args:
            prompt_content: The prompt content
            response_quality: Quality score
            execution_time: Execution time
            tokens_used: Tokens consumed
            success: Success status
            
        Returns:
            List of improvement suggestions
        """
        improvements = []
        
        # Quality-based suggestions
        if response_quality < 0.7:
            improvements.append("Consider adding more context or examples")
            improvements.append("Clarify ambiguous terms or requirements")
        
        # Performance-based suggestions
        if execution_time > 30.0:
            improvements.append("Consider breaking down into smaller prompts")
        
        # Token efficiency suggestions
        if tokens_used > 1000:
            improvements.append("Optimize prompt length to reduce token usage")
        
        # Success-based suggestions
        if not success:
            improvements.append("Review error handling and edge cases")
            improvements.append("Add validation steps to prompt")
        
        # Content analysis suggestions
        if len(prompt_content) < 50:
            improvements.append("Consider adding more specific instructions")
        
        if "please" not in prompt_content.lower():
            improvements.append("Add polite language for better response quality")
        
        return improvements
    
    def _update_evolution_file(self) -> None:
        """Update the evolution markdown file."""
        content = self._generate_evolution_summary()
        
        with open(self.evolution_file, 'w') as f:
            f.write(content)
    
    def _generate_evolution_summary(self) -> str:
        """Generate evolution summary markdown.
        
        Returns:
            Markdown content for evolution summary
        """
        if not self.prompt_history:
            return "# Prompt Evolution Summary\n\nNo prompt history available yet."
        
        # Calculate statistics
        total_prompts = len(self.prompt_history)
        avg_quality = sum(e.response_quality for e in self.prompt_history) / total_prompts
        avg_time = sum(e.execution_time for e in self.prompt_history) / total_prompts
        avg_tokens = sum(e.tokens_used for e in self.prompt_history) / total_prompts
        success_rate = sum(1 for e in self.prompt_history if e.success) / total_prompts
        
        # Get most used template
        template_counts = {}
        for entry in self.prompt_history:
            template_counts[entry.template_name] = template_counts.get(entry.template_name, 0) + 1
        most_used_template = max(template_counts.items(), key=lambda x: x[1])[0]
        
        # Get recent trends
        recent_entries = self.prompt_history[-10:] if len(self.prompt_history) >= 10 else self.prompt_history
        recent_avg_quality = sum(e.response_quality for e in recent_entries) / len(recent_entries)
        
        # Get common improvements
        all_improvements = []
        for entry in self.prompt_history:
            all_improvements.extend(entry.improvements)
        improvement_counts = {}
        for improvement in all_improvements:
            improvement_counts[improvement] = improvement_counts.get(improvement, 0) + 1
        top_improvements = sorted(improvement_counts.items(), key=lambda x: x[1], reverse=True)[:5]
        
        content = f"""# Prompt Evolution Summary

> Last Updated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}
> Total Entries: {total_prompts}

## Statistics

- **Total Prompts:** {total_prompts}
- **Average Quality Score:** {avg_quality:.2f}/1.0
- **Average Response Time:** {avg_time:.2f} seconds
- **Average Token Usage:** {avg_tokens:.0f} tokens
- **Success Rate:** {success_rate:.1%}
- **Most Used Template:** {most_used_template}

## Evolution Patterns

### Quality Trends
- **Recent Average Quality:** {recent_avg_quality:.2f}/1.0
- **Quality Trend:** {"↑ Improving" if recent_avg_quality > avg_quality else "↓ Declining" if recent_avg_quality < avg_quality else "→ Stable"}

### Performance Insights
{self._generate_performance_insights()}

## Most Common Improvements Suggested

{self._format_improvements(top_improvements)}

## Recent Prompt History

{self._format_recent_history(recent_entries)}

## Optimization Recommendations

{self._generate_optimization_recommendations()}
"""
        
        return content
    
    def _generate_performance_insights(self) -> str:
        """Generate performance insights.
        
        Returns:
            Performance insights text
        """
        if len(self.prompt_history) < 5:
            return "- Insufficient data for performance analysis"
        
        insights = []
        
        # Quality over time
        first_half = self.prompt_history[:len(self.prompt_history)//2]
        second_half = self.prompt_history[len(self.prompt_history)//2:]
        
        first_avg = sum(e.response_quality for e in first_half) / len(first_half)
        second_avg = sum(e.response_quality for e in second_half) / len(second_half)
        
        if second_avg > first_avg + 0.1:
            insights.append("- Quality has improved significantly over time")
        elif second_avg < first_avg - 0.1:
            insights.append("- Quality has declined over time - review recent changes")
        else:
            insights.append("- Quality remains consistent over time")
        
        # Token efficiency
        high_quality_entries = [e for e in self.prompt_history if e.response_quality > 0.8]
        if high_quality_entries:
            avg_tokens_high_quality = sum(e.tokens_used for e in high_quality_entries) / len(high_quality_entries)
            all_avg_tokens = sum(e.tokens_used for e in self.prompt_history) / len(self.prompt_history)
            
            if avg_tokens_high_quality < all_avg_tokens:
                insights.append("- Higher quality responses tend to use fewer tokens")
            else:
                insights.append("- Higher quality responses require more tokens")
        
        return "\n".join(insights) if insights else "- No significant patterns detected"
    
    def _format_improvements(self, improvements: List[tuple]) -> str:
        """Format improvement suggestions.
        
        Args:
            improvements: List of (improvement, count) tuples
            
        Returns:
            Formatted improvements text
        """
        if not improvements:
            return "- No improvements suggested yet"
        
        formatted = []
        for improvement, count in improvements:
            formatted.append(f"- **{improvement}** (suggested {count} times)")
        
        return "\n".join(formatted)
    
    def _format_recent_history(self, entries: List[PromptEntry]) -> str:
        """Format recent history entries.
        
        Args:
            entries: List of recent prompt entries
            
        Returns:
            Formatted history text
        """
        if not entries:
            return "- No recent history available"
        
        formatted = []
        for entry in entries[-5:]:  # Show last 5 entries
            status = "✅" if entry.success else "❌"
            formatted.append(
                f"- {status} **{entry.template_name}** "
                f"(Quality: {entry.response_quality:.2f}, "
                f"Time: {entry.execution_time:.1f}s, "
                f"Tokens: {entry.tokens_used})"
            )
        
        return "\n".join(formatted)
    
    def _generate_optimization_recommendations(self) -> str:
        """Generate optimization recommendations.
        
        Returns:
            Optimization recommendations text
        """
        if not self.prompt_history:
            return "- No data available for recommendations"
        
        recommendations = []
        
        # Analyze quality patterns
        low_quality_entries = [e for e in self.prompt_history if e.response_quality < 0.6]
        if len(low_quality_entries) > len(self.prompt_history) * 0.3:
            recommendations.append("- Consider reviewing and improving low-performing templates")
        
        # Analyze token efficiency
        avg_tokens = sum(e.tokens_used for e in self.prompt_history) / len(self.prompt_history)
        if avg_tokens > 800:
            recommendations.append("- Consider optimizing prompts to reduce token usage")
        
        # Analyze execution time
        avg_time = sum(e.execution_time for e in self.prompt_history) / len(self.prompt_history)
        if avg_time > 20.0:
            recommendations.append("- Consider breaking down complex prompts into smaller parts")
        
        # Success rate analysis
        success_rate = sum(1 for e in self.prompt_history if e.success) / len(self.prompt_history)
        if success_rate < 0.8:
            recommendations.append("- Review error handling and add more robust validation")
        
        return "\n".join(recommendations) if recommendations else "- Current performance is optimal"
    
    def get_evolution_stats(self) -> Dict[str, Any]:
        """Get evolution statistics.
        
        Returns:
            Dictionary with evolution statistics
        """
        if not self.prompt_history:
            return {
                "total_prompts": 0,
                "avg_quality": 0.0,
                "success_rate": 0.0,
                "avg_execution_time": 0.0,
                "avg_tokens": 0
            }
        
        return {
            "total_prompts": len(self.prompt_history),
            "avg_quality": sum(e.response_quality for e in self.prompt_history) / len(self.prompt_history),
            "success_rate": sum(1 for e in self.prompt_history if e.success) / len(self.prompt_history),
            "avg_execution_time": sum(e.execution_time for e in self.prompt_history) / len(self.prompt_history),
            "avg_tokens": sum(e.tokens_used for e in self.prompt_history) / len(self.prompt_history),
            "most_used_template": self._get_most_used_template(),
            "recent_trend": self._get_recent_trend()
        }
    
    def _get_most_used_template(self) -> str:
        """Get the most frequently used template."""
        if not self.prompt_history:
            return "None"
        
        template_counts = {}
        for entry in self.prompt_history:
            template_counts[entry.template_name] = template_counts.get(entry.template_name, 0) + 1
        
        return max(template_counts.items(), key=lambda x: x[1])[0]
    
    def _get_recent_trend(self) -> str:
        """Get recent quality trend."""
        if len(self.prompt_history) < 10:
            return "Insufficient data"
        
        recent_entries = self.prompt_history[-5:]
        older_entries = self.prompt_history[-10:-5]
        
        recent_avg = sum(e.response_quality for e in recent_entries) / len(recent_entries)
        older_avg = sum(e.response_quality for e in older_entries) / len(older_entries)
        
        if recent_avg > older_avg + 0.1:
            return "Improving"
        elif recent_avg < older_avg - 0.1:
            return "Declining"
        else:
            return "Stable"