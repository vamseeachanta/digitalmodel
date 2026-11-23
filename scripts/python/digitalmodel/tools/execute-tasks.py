#!/usr/bin/env python3
"""
Self-contained /execute-tasks slash command entry point.
Works immediately after git clone with no external dependencies.
"""

import sys
import os
import re
from pathlib import Path
from datetime import datetime

def find_tasks_file(tasks_reference):
    """Find and validate tasks file."""
    if tasks_reference.startswith('@'):
        # Remove @ prefix and resolve path
        file_path = Path(tasks_reference[1:])
    else:
        file_path = Path(tasks_reference)
    
    if not file_path.exists():
        print(f"❌ Tasks file not found: {file_path}")
        return None
    
    return file_path

def parse_tasks(tasks_file):
    """Parse tasks from tasks.md file."""
    with open(tasks_file, 'r') as f:
        content = f.read()
    
    tasks = []
    current_task = None
    
    for line in content.split('\n'):
        # Match main tasks (- [ ] or - [x])
        main_task_match = re.match(r'^- \[([ x])\] (\d+)\. (.+)$', line.strip())
        if main_task_match:
            status = main_task_match.group(1)
            number = main_task_match.group(2)
            description = main_task_match.group(3)
            
            current_task = {
                'number': number,
                'description': description,
                'completed': status == 'x',
                'subtasks': []
            }
            tasks.append(current_task)
        
        # Match subtasks (  - [ ] or  - [x])
        subtask_match = re.match(r'^  - \[([ x])\] (.+)$', line.strip())
        if subtask_match and current_task:
            status = subtask_match.group(1)
            description = subtask_match.group(2)
            
            current_task['subtasks'].append({
                'description': description,
                'completed': status == 'x'
            })
    
    return tasks

def show_task_status(tasks):
    """Display current task status."""
    print("📋 Task Status:")
    print("=" * 50)
    
    total_tasks = len(tasks)
    completed_tasks = sum(1 for task in tasks if task['completed'])
    
    for task in tasks:
        status = "✅" if task['completed'] else "⏳"
        print(f"{status} Task {task['number']}: {task['description']}")
        
        if task['subtasks']:
            completed_subtasks = sum(1 for subtask in task['subtasks'] if subtask['completed'])
            total_subtasks = len(task['subtasks'])
            print(f"    Subtasks: {completed_subtasks}/{total_subtasks} completed")
    
    print("=" * 50)
    print(f"Overall Progress: {completed_tasks}/{total_tasks} tasks completed")
    
    # Find next task to work on
    next_task = None
    for task in tasks:
        if not task['completed']:
            next_task = task
            break
    
    if next_task:
        print(f"\n⏭️  Next Task: {next_task['number']}. {next_task['description']}")
        
        # Find next subtask
        next_subtask = None
        for subtask in next_task['subtasks']:
            if not subtask['completed']:
                next_subtask = subtask
                break
        
        if next_subtask:
            print(f"   Next Subtask: {next_subtask['description']}")
    else:
        print("\n🎉 All tasks completed!")

def main():
    """Main execute-tasks command."""
    if len(sys.argv) < 2:
        print("Usage: python execute-tasks.py <tasks-file>")
        print("Example: python execute-tasks.py @.agent-os/specs/2025-08-06-user-auth/tasks.md")
        return 1
    
    tasks_reference = sys.argv[1]
    
    try:
        # Find tasks file
        tasks_file = find_tasks_file(tasks_reference)
        if not tasks_file:
            return 1
        
        print(f"📄 Loading tasks from: {tasks_file}")
        
        # Parse and display tasks
        tasks = parse_tasks(tasks_file)
        show_task_status(tasks)
        
        print(f"\n💡 Use your preferred development environment to work on the tasks.")
        print(f"💡 Update {tasks_file} manually to mark tasks complete: [ ] → [x]")
        print(f"💡 Follow the coding standards in .agent-os/standards/")
        
        return 0
        
    except Exception as e:
        print(f"❌ Error executing tasks: {e}")
        return 1

if __name__ == "__main__":
    sys.exit(main())
