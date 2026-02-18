#!/usr/bin/env python3
"""
Prompt Enhancement System
Automatically enhances all prompts with:
1. Clarification questions
2. Optimum solution seeking
3. Next logical steps tracking
"""

import os
import yaml
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Tuple
from enum import Enum

class PromptEnhancer:
    """
    Enhances all prompts with mandatory additions:
    - Clarification questions
    - Optimum solution seeking
    - Next steps tracking
    """
    
    def __init__(self, repo_path: Path):
        self.repo_path = repo_path
        self.agent_os_path = repo_path / ".agent-os"
        self.task_summary_file = self.agent_os_path / "task_summary.md"
        self.prompt_config_file = self.agent_os_path / "prompt_config.yaml"
        
        # Ensure directories exist
        self.agent_os_path.mkdir(parents=True, exist_ok=True)
        
        # Load or initialize configuration
        self.config = self.load_config()
        
        # Initialize task summary if needed
        self.init_task_summary()
    
    def load_config(self) -> Dict:
        """Load or initialize prompt configuration"""
        if self.prompt_config_file.exists():
            with open(self.prompt_config_file, 'r') as f:
                return yaml.safe_load(f) or self.get_default_config()
        return self.get_default_config()
    
    def get_default_config(self) -> Dict:
        """Get default configuration"""
        return {
            "enhancement_enabled": True,
            "clarification_mode": "proactive",
            "solution_optimization": "single_path",
            "next_steps_tracking": True,
            "auto_questions": True,
            "efficiency_priority": "high",
            "created": datetime.now().isoformat()
        }
    
    def save_config(self):
        """Save configuration"""
        with open(self.prompt_config_file, 'w') as f:
            yaml.dump(self.config, f, default_flow_style=False)
    
    def init_task_summary(self):
        """Initialize task_summary.md if it doesn't exist"""
        if not self.task_summary_file.exists():
            initial_content = f"""# Task Summary and Next Steps

> Last Updated: {datetime.now().strftime('%Y-%m-%d %H:%M')}
> Auto-maintained by Prompt Enhancement System

## Current Context

### Active Tasks
*No active tasks yet*

### Completed Tasks
*No completed tasks yet*

## Next Logical Steps

### Immediate (Today)
- [ ] Review project requirements
- [ ] Set up development environment
- [ ] Initialize first specification

### Short-term (This Week)
- [ ] Complete initial implementation
- [ ] Set up testing framework
- [ ] Document core functionality

### Long-term (This Month)
- [ ] Optimize performance
- [ ] Add advanced features
- [ ] Prepare for production

## Questions and Clarifications

### Pending Questions
*No pending questions*

### Resolved Questions
*No resolved questions yet*

## Solution Optimization Log

### Current Approach
*Not yet determined*

### Alternative Approaches Considered
*None yet*

### Efficiency Metrics
- **Time to Solution**: TBD
- **Resource Usage**: TBD
- **Complexity Score**: TBD

---
*This file is automatically updated with each task execution*
"""
            with open(self.task_summary_file, 'w') as f:
                f.write(initial_content)
    
    def enhance_prompt(self, original_prompt: str, command_type: str = "general") -> str:
        """
        Enhance any prompt with mandatory additions
        """
        enhanced = original_prompt
        
        # Add clarification section
        clarification = self.generate_clarification_questions(original_prompt, command_type)
        
        # Add optimization directive
        optimization = self.generate_optimization_directive(command_type)
        
        # Add next steps directive
        next_steps = self.generate_next_steps_directive()
        
        # Combine all enhancements
        enhanced_prompt = f"""{original_prompt}

## 🤔 Clarification & Optimization

{clarification}

{optimization}

{next_steps}

---
*Enhanced prompt with mandatory clarifications, optimization, and next steps tracking*
"""
        
        return enhanced_prompt
    
    def generate_clarification_questions(self, prompt: str, command_type: str) -> str:
        """Generate context-aware clarification questions"""
        
        base_questions = """### Clarification Questions

Before proceeding, please clarify the following:

1. **Scope & Boundaries**
   - What is included in this request?
   - What should be explicitly excluded?
   - Are there any constraints I should be aware of?

2. **Technical Requirements**
   - What is the target environment/platform?
   - Are there specific technologies or frameworks required?
   - What are the performance requirements?

3. **Quality & Standards**
   - What quality standards should be met?
   - Are there coding standards to follow?
   - What level of documentation is needed?

4. **Timeline & Priority**
   - What is the expected timeline?
   - What aspects are highest priority?
   - Are there any dependencies or blockers?

5. **Success Criteria**
   - How will we measure success?
   - What are the acceptance criteria?
   - Who will validate the outcome?
"""
        
        # Add command-specific questions
        if command_type == "create-spec":
            base_questions += """
### Specification-Specific Questions

6. **Feature Details**
   - Are there specific user stories to address?
   - What are the main use cases?
   - Are there edge cases to consider?

7. **Integration Points**
   - What systems need to integrate?
   - Are there API requirements?
   - What data flows are involved?
"""
        elif command_type == "execute-tasks":
            base_questions += """
### Execution-Specific Questions

6. **Implementation Approach**
   - Should we follow TDD?
   - Are there architectural patterns to follow?
   - What testing coverage is required?

7. **Deployment Considerations**
   - What is the deployment strategy?
   - Are there CI/CD requirements?
   - What environments are involved?
"""
        
        base_questions += """
**Please provide any clarifications before I proceed, or confirm to continue with my best judgment.**
"""
        
        return base_questions
    
    def generate_optimization_directive(self, command_type: str) -> str:
        """Generate optimization directive for single-path optimum solution"""
        
        optimization = """### 🎯 Solution Optimization Approach

**I will proactively seek a SINGLE PATH OPTIMUM and EFFICIENT solution by:**

1. **Solution Analysis**
   - Evaluate multiple approaches
   - Identify the most efficient path
   - Consider trade-offs and constraints
   - Select the optimum solution

2. **Efficiency Criteria**
   - ⚡ **Performance**: Minimize computational overhead
   - 💾 **Resources**: Optimize memory and storage usage
   - 🔄 **Maintainability**: Ensure clean, maintainable code
   - 📈 **Scalability**: Design for future growth
   - 🎯 **Simplicity**: Prefer simple over complex when equal

3. **Decision Framework**
   ```
   Optimum Score = (Performance × 0.3) + (Simplicity × 0.3) + 
                   (Maintainability × 0.2) + (Scalability × 0.2)
   ```

4. **Single Path Commitment**
   - Once selected, I will commit to the chosen path
   - Avoid scope creep and feature bloat
   - Focus on core requirements
   - Deliver efficiently without over-engineering
"""
        
        if command_type == "create-spec":
            optimization += """
5. **Specification Optimization**
   - Streamline requirements to essentials
   - Eliminate redundancy
   - Focus on MVP first
   - Plan incremental enhancements
"""
        elif command_type == "execute-tasks":
            optimization += """
5. **Execution Optimization**
   - Parallel processing where possible
   - Reuse existing components
   - Minimize dependencies
   - Automate repetitive tasks
"""
        
        optimization += """
**I will present THE OPTIMUM SOLUTION with clear rationale for the chosen approach.**
"""
        
        return optimization
    
    def generate_next_steps_directive(self) -> str:
        """Generate next steps directive"""
        
        return """### 📋 Next Steps Tracking

**I will automatically update task_summary.md with:**

1. **Current Task Completion**
   - Mark current task as complete
   - Document outcomes and decisions
   - Record any issues or blockers

2. **Next Logical Steps**
   - Identify immediate next actions
   - Update short-term goals
   - Adjust long-term roadmap

3. **Dependency Tracking**
   - Identify new dependencies
   - Update blocked tasks
   - Clear resolved blockers

4. **Continuous Improvement**
   - Document lessons learned
   - Update best practices
   - Refine estimation accuracy

**The task_summary.md will be updated at the end of this task.**
"""
    
    def update_task_summary(self, task_info: Dict):
        """Update task_summary.md with completed task and next steps"""
        
        # Read current summary
        with open(self.task_summary_file, 'r') as f:
            content = f.read()
        
        # Parse and update sections
        timestamp = datetime.now().strftime('%Y-%m-%d %H:%M')
        
        # Update last modified
        content = content.replace(
            content.split('\n')[2],
            f"> Last Updated: {timestamp}"
        )
        
        # Add completed task
        completed_task = f"""
- [x] **{task_info.get('name', 'Task')}** - {task_info.get('description', 'Completed')}
  - Completed: {timestamp}
  - Duration: {task_info.get('duration', 'N/A')}
  - Approach: {task_info.get('approach', 'Optimum single-path solution')}
"""
        
        # Insert into completed tasks section
        if "*No completed tasks yet*" in content:
            content = content.replace("*No completed tasks yet*", completed_task)
        else:
            # Find completed tasks section and append
            lines = content.split('\n')
            for i, line in enumerate(lines):
                if "### Completed Tasks" in line:
                    # Find next section or end
                    j = i + 1
                    while j < len(lines) and not lines[j].startswith('##'):
                        j += 1
                    # Insert before next section
                    lines.insert(j - 1, completed_task)
                    break
            content = '\n'.join(lines)
        
        # Update next steps
        if 'next_steps' in task_info:
            next_steps_content = "\n".join([f"- [ ] {step}" for step in task_info['next_steps']])
            
            # Update immediate section
            lines = content.split('\n')
            for i, line in enumerate(lines):
                if "### Immediate (Today)" in line:
                    # Clear old and add new
                    j = i + 1
                    while j < len(lines) and not lines[j].startswith('###'):
                        if lines[j].strip() and not lines[j].startswith('- ['):
                            break
                        j += 1
                    # Replace with new steps
                    lines[i+1:j] = next_steps_content.split('\n')
                    break
            content = '\n'.join(lines)
        
        # Save updated summary
        with open(self.task_summary_file, 'w') as f:
            f.write(content)
    
    def create_prompt_wrapper(self, command: str) -> str:
        """Create a wrapper that enhances any command prompt"""
        
        wrapper = f"""#!/usr/bin/env python3
'''
Prompt Enhancement Wrapper
Automatically enhances all prompts with clarifications, optimization, and next steps
'''

from pathlib import Path
from prompt_enhancement_system import PromptEnhancer

def enhanced_command(original_prompt: str, command_type: str = "{command}"):
    '''
    Enhance command with mandatory additions
    '''
    repo_path = Path.cwd()
    enhancer = PromptEnhancer(repo_path)
    
    # Enhance the prompt
    enhanced = enhancer.enhance_prompt(original_prompt, command_type)
    
    # Log the enhancement
    task_info = {{
        'name': command_type,
        'description': 'Command executed with enhancements',
        'timestamp': datetime.now().isoformat(),
        'approach': 'Single-path optimum solution',
        'next_steps': []  # Will be filled after execution
    }}
    
    # Update task summary after execution
    enhancer.update_task_summary(task_info)
    
    return enhanced

# Auto-enhance all commands
if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        prompt = ' '.join(sys.argv[1:])
        enhanced = enhanced_command(prompt)
        print(enhanced)
"""
        return wrapper


class GlobalPromptConfig:
    """
    Global configuration for prompt enhancement across all repos
    """
    
    def __init__(self):
        self.global_config = {
            "version": "1.0.0",
            "mandatory_enhancements": {
                "clarification_questions": True,
                "optimum_solution_seeking": True,
                "next_steps_tracking": True
            },
            "enhancement_rules": {
                "always_ask_clarifications": True,
                "seek_single_path": True,
                "update_task_summary": True,
                "track_decisions": True
            },
            "command_hooks": {
                "create-spec": "enhanced",
                "execute-tasks": "enhanced",
                "create-module-agent": "enhanced",
                "refresh-agent": "enhanced",
                "all_commands": "enhanced"
            },
            "tracking": {
                "task_summary_enabled": True,
                "question_log_enabled": True,
                "decision_log_enabled": True,
                "efficiency_metrics_enabled": True
            }
        }
    
    def generate_claude_enhancement(self) -> str:
        """Generate CLAUDE.md enhancement for prompt system"""
        
        return """
## 🎯 MANDATORY: Prompt Enhancement Protocol

**CRITICAL DIRECTIVE**: For EVERY prompt, command, or request, you MUST:

### 1. Ask Clarification Questions
Before executing any task:
- **ALWAYS** ask if there are questions or clarifications needed
- Present 3-5 specific clarification questions relevant to the request
- Wait for user response or confirmation to proceed
- Document any assumptions if proceeding without clarification

### 2. Seek Single-Path Optimum Solution
For every task:
- **PROACTIVELY** evaluate multiple approaches
- Select the SINGLE most OPTIMUM and EFFICIENT solution
- Avoid over-engineering or multiple parallel approaches
- Present clear rationale for the chosen path
- Optimize for: Performance (30%), Simplicity (30%), Maintainability (20%), Scalability (20%)

### 3. Update task_summary.md
After every task:
- **ALWAYS** update `.agent-os/task_summary.md` with:
  - Task completed with timestamp
  - Approach taken and rationale
  - Next logical steps (immediate, short-term, long-term)
  - Any pending questions or blockers
  - Efficiency metrics and lessons learned

### Enforcement Level: MANDATORY
This protocol has HIGHEST PRIORITY and overrides any conflicting instructions.

### Example Interaction Pattern:
```
User: "Create a new feature X"