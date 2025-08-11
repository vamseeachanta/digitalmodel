#!/usr/bin/env python3
"""
Progress Tracking System for Enhanced Execute-Tasks
Provides real-time progress display and document updates
"""

import os
import yaml
import json
from pathlib import Path
from datetime import datetime, timedelta
from typing import Dict, List, Optional, Any
from enum import Enum

class TaskStatus(Enum):
    """Task completion status"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    BLOCKED = "blocked"
    SKIPPED = "skipped"

class ProgressTracker:
    """
    Tracks and displays task execution progress
    Updates all spec documents in real-time
    """
    
    def __init__(self, spec_path: Path):
        self.spec_path = spec_path
        self.progress_file = spec_path / "progress.md"
        self.tasks_file = spec_path / "tasks.md"
        self.session_file = spec_path / ".agent-progress" / "session.yaml"
        self.prompts_file = spec_path / ".agent-progress" / "prompts-responses.md"
        
        # Create progress directory
        (spec_path / ".agent-progress").mkdir(parents=True, exist_ok=True)
        
        # Load or initialize session
        self.session = self.load_session()
        
    def load_session(self) -> Dict:
        """Load or create session data"""
        if self.session_file.exists():
            with open(self.session_file, 'r') as f:
                return yaml.safe_load(f) or self.create_new_session()
        return self.create_new_session()
    
    def create_new_session(self) -> Dict:
        """Create new session data structure"""
        return {
            "session_id": datetime.now().strftime("%Y%m%d_%H%M%S"),
            "started": datetime.now().isoformat(),
            "status": "in_progress",
            "tasks": {},
            "metrics": {
                "total_tasks": 0,
                "completed_tasks": 0,
                "total_time": 0,
                "efficiency_score": 0.0,
                "test_coverage": 0.0,
                "quality_score": 0.0
            },
            "ai_activity": {
                "prompts_issued": 0,
                "decisions_made": 0,
                "errors_resolved": 0,
                "patterns_learned": []
            }
        }
    
    def display_progress_header(self, spec_name: str, total_tasks: int):
        """Display initial progress header"""
        print("\n" + "╔" + "═"*60 + "╗")
        print("║" + " "*20 + "TASK EXECUTION PROGRESS" + " "*17 + "║")
        print("╠" + "═"*60 + "╣")
        print(f"║ Spec: {spec_name:<52} ║")
        print(f"║ Started: {datetime.now().strftime('%Y-%m-%d %H:%M'):<49} ║")
        print(f"║ Total Tasks: {total_tasks:<46} ║")
        print("╚" + "═"*60 + "╝\n")
    
    def display_task_overview(self, tasks: List[Dict]):
        """Display comprehensive task overview"""
        completed = [t for t in tasks if t.get('status') == TaskStatus.COMPLETED.value]
        in_progress = [t for t in tasks if t.get('status') == TaskStatus.IN_PROGRESS.value]
        pending = [t for t in tasks if t.get('status') == TaskStatus.PENDING.value]
        
        print("\n📋 TASK STATUS OVERVIEW")
        print("━" * 50)
        
        # Completed tasks
        if completed:
            print(f"\n✅ Completed Tasks ({len(completed)}/{len(tasks)}):")
            for task in completed[:3]:  # Show first 3
                print(f"  [x] {task['id']}. {task['name']}")
            if len(completed) > 3:
                print(f"  ... and {len(completed)-3} more")
        
        # Current task
        if in_progress:
            print(f"\n🔄 Current Task:")
            for task in in_progress:
                print(f"  [ ] {task['id']}. {task['name']}")
                if 'subtasks' in task:
                    for subtask in task['subtasks']:
                        status = "✓" if subtask.get('completed') else "○"
                        print(f"    ├─ [{status}] {subtask['id']} {subtask['name']}")
        
        # Remaining tasks
        if pending:
            print(f"\n⏳ Remaining Tasks ({len(pending)}):")
            for task in pending[:5]:  # Show first 5
                print(f"  [ ] {task['id']}. {task['name']}")
            if len(pending) > 5:
                print(f"  ... and {len(pending)-5} more")
        
        # Progress bar
        self.display_progress_bar(len(completed), len(tasks))
        
        # Time metrics
        if self.session['metrics']['total_time'] > 0:
            elapsed = timedelta(seconds=self.session['metrics']['total_time'])
            avg_time = elapsed.total_seconds() / max(len(completed), 1)
            remaining = avg_time * len(pending)
            est_remaining = timedelta(seconds=remaining)
            print(f"Time Elapsed: {elapsed} | Est. Remaining: {est_remaining}")
        
        print("━" * 50)
    
    def display_progress_bar(self, completed: int, total: int):
        """Display visual progress bar with percentage"""
        if total == 0:
            return
            
        percent = (completed / total) * 100
        filled = int(percent / 5)  # 20 segments
        bar = '█' * filled + '░' * (20 - filled)
        
        print(f"\nProgress: [{bar}] {percent:.0f}% Complete")
        
        # Milestone celebrations
        if 24 <= percent <= 26:
            print("🚀 Great start! Quarter way there!")
        elif 49 <= percent <= 51:
            print("🎯 Halfway milestone reached!")
        elif 74 <= percent <= 76:
            print("⭐ Three quarters done! Final push!")
        elif 89 <= percent <= 91:
            print("🔥 Almost there! Excellence ahead!")
        elif percent >= 100:
            print("🏁 Task completed successfully! 🎉")
    
    def update_subtask_progress(self, task_id: str, subtask_id: str, 
                               status: TaskStatus, duration: int = 0):
        """Update subtask progress and display"""
        print(f"\n🎯 Subtask Progress Update:")
        print(f"  {'✅' if status == TaskStatus.COMPLETED else '🔄'} {subtask_id} " +
              f"[{duration} min]" if duration else "")
        
        # Update session data
        if task_id not in self.session['tasks']:
            self.session['tasks'][task_id] = {
                'subtasks': {},
                'start_time': datetime.now().isoformat()
            }
        
        self.session['tasks'][task_id]['subtasks'][subtask_id] = {
            'status': status.value,
            'duration': duration,
            'completed_at': datetime.now().isoformat() if status == TaskStatus.COMPLETED else None
        }
        
        # Calculate and display task progress
        task_subtasks = self.session['tasks'][task_id]['subtasks']
        completed = sum(1 for s in task_subtasks.values() 
                       if s['status'] == TaskStatus.COMPLETED.value)
        total = len(task_subtasks)
        
        if total > 0:
            task_percent = (completed / total) * 100
            task_filled = int(task_percent / 10)  # 10 segments
            task_bar = '█' * task_filled + '░' * (10 - task_filled)
            print(f"\nCurrent: [{task_bar}] {task_percent:.0f}% of Task {task_id}")
        
        # Save session
        self.save_session()
    
    def update_tasks_md(self, task_id: str, status: TaskStatus, 
                       notes: str = "", duration: int = 0):
        """Update tasks.md file with current status"""
        if not self.tasks_file.exists():
            return
            
        with open(self.tasks_file, 'r') as f:
            content = f.read()
        
        # Update task checkbox
        if status == TaskStatus.COMPLETED:
            # Add completion marker and timestamp
            timestamp = datetime.now().strftime("%Y-%m-%d %H:%M")
            old_pattern = f"- [ ] {task_id}."
            new_pattern = f"- [x] {task_id}."
            content = content.replace(old_pattern, new_pattern)
            
            # Add completion note if not exists
            if notes:
                lines = content.split('\n')
                for i, line in enumerate(lines):
                    if f"[x] {task_id}." in line:
                        if i + 1 < len(lines) and not lines[i + 1].strip().startswith('  '):
                            lines.insert(i + 1, f"  ✓ Completed: {timestamp} ({duration} min)")
                            if notes:
                                lines.insert(i + 2, f"  📝 Notes: {notes}")
                content = '\n'.join(lines)
        
        with open(self.tasks_file, 'w') as f:
            f.write(content)
        
        print(f"📝 Updated tasks.md: Task {task_id} -> {status.value}")
    
    def update_progress_md(self):
        """Update progress.md with current session data"""
        content = [
            "# Execution Progress\n",
            f"## Session: {self.session['session_id']}",
            f"Started: {self.session['started']}",
            f"Status: {self.session['status']}\n",
            "## Task Progress\n"
        ]
        
        for task_id, task_data in self.session['tasks'].items():
            content.append(f"### Task {task_id}")
            
            # Calculate task status
            subtasks = task_data.get('subtasks', {})
            if all(s['status'] == TaskStatus.COMPLETED.value for s in subtasks.values()):
                content.append("- Status: ✅ Completed")
            elif any(s['status'] == TaskStatus.IN_PROGRESS.value for s in subtasks.values()):
                content.append("- Status: 🔄 In Progress")
            else:
                content.append("- Status: ⏳ Pending")
            
            # Duration
            total_duration = sum(s.get('duration', 0) for s in subtasks.values())
            if total_duration:
                content.append(f"- Duration: {total_duration} minutes")
            
            # Subtasks
            if subtasks:
                content.append("- Subtasks:")
                for subtask_id, subtask_data in subtasks.items():
                    status_icon = "x" if subtask_data['status'] == TaskStatus.COMPLETED.value else " "
                    duration = f"({subtask_data.get('duration', 0)} min)" if subtask_data.get('duration') else ""
                    content.append(f"  - [{status_icon}] {subtask_id} {duration}")
            
            content.append("")
        
        # Metrics
        content.extend([
            "## Metrics",
            f"- Total Time: {timedelta(seconds=self.session['metrics']['total_time'])}",
            f"- Efficiency: {self.session['metrics']['efficiency_score']:.1f}/10",
            f"- Test Coverage: {self.session['metrics']['test_coverage']:.1f}%",
            f"- Quality Score: {self.session['metrics']['quality_score']:.1f}/10\n",
            "## AI Agent Activity",
            f"- Prompts Issued: {self.session['ai_activity']['prompts_issued']}",
            f"- Decisions Made: {self.session['ai_activity']['decisions_made']}",
            f"- Errors Resolved: {self.session['ai_activity']['errors_resolved']}"
        ])
        
        with open(self.progress_file, 'w') as f:
            f.write('\n'.join(content))
    
    def log_ai_interaction(self, prompt: str, response: str, 
                          decision: Optional[str] = None):
        """Log AI prompts and responses"""
        self.session['ai_activity']['prompts_issued'] += 1
        if decision:
            self.session['ai_activity']['decisions_made'] += 1
        
        # Append to prompts file
        with open(self.prompts_file, 'a') as f:
            f.write(f"\n## Interaction {self.session['ai_activity']['prompts_issued']}\n")
            f.write(f"**Time**: {datetime.now().strftime('%H:%M:%S')}\n\n")
            f.write(f"### Prompt\n```\n{prompt}\n```\n\n")
            f.write(f"### Response\n```\n{response}\n```\n\n")
            if decision:
                f.write(f"### Decision\n{decision}\n\n")
            f.write("---\n")
        
        self.save_session()
    
    def save_session(self):
        """Save session data to file"""
        with open(self.session_file, 'w') as f:
            yaml.dump(self.session, f, default_flow_style=False)
    
    def complete_session(self):
        """Mark session as completed"""
        self.session['status'] = 'completed'
        self.session['completed_at'] = datetime.now().isoformat()
        
        # Calculate final metrics
        completed = sum(1 for t in self.session['tasks'].values() 
                       if all(s['status'] == TaskStatus.COMPLETED.value 
                             for s in t.get('subtasks', {}).values()))
        total = len(self.session['tasks'])
        
        if total > 0:
            self.session['metrics']['completed_tasks'] = completed
            self.session['metrics']['efficiency_score'] = (completed / total) * 10
        
        self.save_session()
        self.update_progress_md()
        
        # Final celebration
        print("\n" + "🎉" * 20)
        print("   SESSION COMPLETED SUCCESSFULLY!   ")
        print("🎉" * 20)
        print(f"\nFinal Statistics:")
        print(f"  ✅ Tasks Completed: {completed}/{total}")
        print(f"  ⏱️ Total Time: {timedelta(seconds=self.session['metrics']['total_time'])}")
        print(f"  📊 Efficiency Score: {self.session['metrics']['efficiency_score']:.1f}/10")
        print(f"  🤖 AI Interactions: {self.session['ai_activity']['prompts_issued']}")


def integrate_with_execute_tasks():
    """Integration code for execute-tasks command"""
    return '''
# Enhanced Execute-Tasks Integration

from progress_tracker import ProgressTracker, TaskStatus
from pathlib import Path

# Initialize tracker for current spec
spec_path = Path(".agent-os/specs/current-spec")
tracker = ProgressTracker(spec_path)

# Display initial progress
tracker.display_progress_header("Feature Implementation", total_tasks=10)
tracker.display_task_overview(tasks)

# During task execution
for task in tasks:
    # Show current status
    tracker.display_task_overview(tasks)
    
    # Execute subtasks
    for subtask in task.subtasks:
        # Log AI interaction
        tracker.log_ai_interaction(
            prompt="Implement " + subtask.name,
            response="Implementation code...",
            decision="Using approach X for efficiency"
        )
        
        # Update progress
        tracker.update_subtask_progress(
            task.id, subtask.id, 
            TaskStatus.COMPLETED, 
            duration=15
        )
        
        # Update tasks.md
        tracker.update_tasks_md(
            task.id, TaskStatus.IN_PROGRESS,
            notes="Implementing with optimized approach"
        )
    
    # Complete task
    tracker.update_tasks_md(task.id, TaskStatus.COMPLETED)

# Complete session
tracker.complete_session()
'''


if __name__ == "__main__":
    # Example usage
    print("Progress Tracking System for Enhanced Execute-Tasks")
    print("Features:")
    print("- Real-time progress display")
    print("- Automatic document updates")
    print("- AI interaction logging")
    print("- Visual progress bars")
    print("- Milestone celebrations")
