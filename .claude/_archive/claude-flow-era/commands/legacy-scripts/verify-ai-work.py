#!/usr/bin/env python3
"""
/verify-ai-work - Interactive AI Work Verification System
A lightweight, user-friendly interface for step-by-step verification of AI-generated work
with manual confirmation and iterative feedback loops.

MANDATORY: Must be executed in repository spec folders only.
Reports are saved to verification_report/ subdirectory.
"""

import os
import sys
import json
import time
import textwrap
import subprocess
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional, Tuple
import argparse
from dataclasses import dataclass, asdict
from enum import Enum

# Lightweight UI using only standard library
try:
    import tkinter as tk
    from tkinter import ttk, scrolledtext, messagebox
    HAS_GUI = True
except ImportError:
    HAS_GUI = False

class StepStatus(Enum):
    """Status of each verification step"""
    PENDING = "⏳ Pending"
    IN_PROGRESS = "🔄 In Progress"
    COMPLETED = "✅ Completed"
    FAILED = "❌ Failed"
    SKIPPED = "⏭️ Skipped"
    NEEDS_REVISION = "🔧 Needs Revision"

@dataclass
class VerificationStep:
    """Individual verification step"""
    number: int
    title: str
    description: str
    instructions: List[str]
    expected_outcome: str
    actual_outcome: str = ""
    status: StepStatus = StepStatus.PENDING
    user_feedback: str = ""
    ai_response: str = ""
    attempts: int = 0
    time_spent: float = 0.0
    
    def to_simple_text(self) -> str:
        """Convert to simple text for 10-year-old understanding"""
        simple = f"\n{'='*60}\n"
        simple += f"📌 Step {self.number}: {self.title}\n"
        simple += f"{'='*60}\n\n"
        
        simple += "📝 What to do:\n"
        simple += f"{self.description}\n\n"
        
        simple += "📋 Follow these steps:\n"
        for i, instruction in enumerate(self.instructions, 1):
            simple += f"  {i}. {instruction}\n"
        
        simple += f"\n✨ What should happen:\n"
        simple += f"  {self.expected_outcome}\n"
        
        return simple

class InteractiveVerifier:
    """Main verification system with interactive UI"""
    
    def __init__(self, task_file: Optional[Path] = None):
        self.task_file = task_file
        self.steps: List[VerificationStep] = []
        self.current_step = 0
        self.session_id = datetime.now().strftime("%Y%m%d_%H%M%S")
        self.feedback_log = []
        self.start_time = None
        self.gui = None
        self.spec_folder = None
        self.verification_report_dir = None
        
    def load_tasks(self, file_path: Path) -> bool:
        """Load verification tasks from a file"""
        try:
            if file_path.suffix == '.json':
                with open(file_path, 'r') as f:
                    data = json.load(f)
                    self.steps = [VerificationStep(**step) for step in data['steps']]
            elif file_path.suffix == '.md':
                self.parse_markdown_tasks(file_path)
            else:
                # Try to parse as text
                self.parse_text_tasks(file_path)
            return True
        except Exception as e:
            print(f"❌ Error loading tasks: {e}")
            return False
    
    def parse_markdown_tasks(self, file_path: Path):
        """Parse tasks from markdown format"""
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Simple markdown parser for task format
        import re
        tasks = re.findall(r'##\s+(.+?)\n(.*?)(?=##|\Z)', content, re.DOTALL)
        
        for i, (title, body) in enumerate(tasks, 1):
            lines = body.strip().split('\n')
            description = ""
            instructions = []
            expected = ""
            
            current_section = None
            for line in lines:
                if line.startswith('Description:'):
                    current_section = 'description'
                elif line.startswith('Steps:'):
                    current_section = 'steps'
                elif line.startswith('Expected:'):
                    current_section = 'expected'
                elif current_section == 'description':
                    description += line.strip() + " "
                elif current_section == 'steps' and line.strip().startswith('-'):
                    instructions.append(line.strip()[1:].strip())
                elif current_section == 'expected':
                    expected += line.strip() + " "
            
            self.steps.append(VerificationStep(
                number=i,
                title=title.strip(),
                description=description.strip(),
                instructions=instructions or ["Review and verify"],
                expected_outcome=expected.strip() or "Task completed successfully"
            ))
    
    def parse_text_tasks(self, file_path: Path):
        """Parse simple text task list"""
        with open(file_path, 'r') as f:
            lines = f.readlines()
        
        for i, line in enumerate(lines, 1):
            if line.strip():
                self.steps.append(VerificationStep(
                    number=i,
                    title=f"Task {i}",
                    description=line.strip(),
                    instructions=["Complete this task", "Verify the result"],
                    expected_outcome="Task completed as described"
                ))
    
    def run_cli_mode(self):
        """Run in CLI mode for terminal interaction"""
        self.start_time = time.time()
        
        print("\n" + "="*70)
        print("🤖 AI WORK VERIFICATION SYSTEM")
        print("="*70)
        print(f"📅 Session: {self.session_id}")
        print(f"📋 Total Steps: {len(self.steps)}")
        print("="*70)
        
        for step_idx, step in enumerate(self.steps):
            self.current_step = step_idx
            step.status = StepStatus.IN_PROGRESS
            step_start = time.time()
            
            # Display step in simple format
            self.display_step_cli(step)
            
            # Get user confirmation to proceed
            if not self.get_confirmation_cli(step):
                step.status = StepStatus.SKIPPED
                continue
            
            # Execute verification
            success = self.verify_step_cli(step)
            
            # Collect feedback if needed
            if not success or step.status == StepStatus.NEEDS_REVISION:
                self.collect_feedback_cli(step)
                
                # Ask if user wants AI to revise
                if self.ask_for_revision_cli(step):
                    self.request_ai_revision(step)
            
            step.time_spent = time.time() - step_start
            
            # Summary after each step
            self.show_step_summary_cli(step)
        
        # Final summary
        self.show_final_summary_cli()
    
    def display_step_cli(self, step: VerificationStep):
        """Display step in CLI with child-friendly formatting"""
        print("\n" + "🌟"*30)
        print(step.to_simple_text())
        print("🌟"*30)
        
        # Add emoji-based progress bar
        completed = self.current_step
        total = len(self.steps)
        progress = "█" * completed + "░" * (total - completed)
        print(f"\n📊 Progress: [{progress}] {completed}/{total}")
    
    def get_confirmation_cli(self, step: VerificationStep) -> bool:
        """Get manual confirmation before proceeding"""
        print("\n" + "-"*60)
        print("🤔 Are you ready to start this step?")
        print("   Type 'yes' or 'y' to continue")
        print("   Type 'skip' to skip this step")
        print("   Type 'help' for more information")
        print("-"*60)
        
        while True:
            response = input("👉 Your choice: ").strip().lower()
            
            if response in ['yes', 'y']:
                print("✅ Great! Let's do this step together!")
                return True
            elif response == 'skip':
                print("⏭️ Okay, we'll skip this one!")
                return False
            elif response == 'help':
                self.show_help_cli(step)
            else:
                print("❓ I didn't understand. Please type 'yes', 'skip', or 'help'")
    
    def show_help_cli(self, step: VerificationStep):
        """Show detailed help for a step"""
        print("\n📚 DETAILED HELP")
        print("="*60)
        print(f"This step is about: {step.title}")
        print("\nWhy we're doing this:")
        print(f"  {step.description}")
        print("\nTips for success:")
        print("  • Take your time")
        print("  • Follow each instruction carefully")
        print("  • Ask for help if you're stuck")
        print("  • You can always try again!")
        print("="*60)
    
    def verify_step_cli(self, step: VerificationStep) -> bool:
        """Verify step execution in CLI"""
        print("\n📝 Now, please follow the instructions above.")
        print("   When you're done, I'll ask you about the results!")
        
        input("\n   Press Enter when you've completed the step...")
        
        print("\n" + "-"*60)
        print("🎯 Verification Questions:")
        print("-"*60)
        
        # Ask simple verification questions
        print("\n1️⃣ Did you complete all the steps? (yes/no)")
        completed = input("   👉 ").strip().lower() == 'yes'
        
        print("\n2️⃣ Did everything work as expected? (yes/no)")
        as_expected = input("   👉 ").strip().lower() == 'yes'
        
        if completed and as_expected:
            step.status = StepStatus.COMPLETED
            print("\n🎉 Awesome! You did it perfectly!")
            return True
        else:
            print("\n🤔 Hmm, seems like something didn't work quite right.")
            print("   Let's figure out what happened...")
            
            print("\n3️⃣ What actually happened? (describe in your own words)")
            step.actual_outcome = input("   👉 ").strip()
            
            step.status = StepStatus.NEEDS_REVISION
            return False
    
    def collect_feedback_cli(self, step: VerificationStep):
        """Collect detailed feedback for failed steps"""
        print("\n" + "="*60)
        print("💬 FEEDBACK COLLECTION")
        print("="*60)
        print("Let's gather some information to help fix this!")
        
        print("\n📝 Please tell me:")
        print("   1. What went wrong?")
        print("   2. What error messages did you see?")
        print("   3. What do you think should happen instead?")
        print("\n(Type your feedback, press Enter twice when done)")
        
        feedback_lines = []
        empty_count = 0
        while empty_count < 2:
            line = input()
            if line:
                feedback_lines.append(line)
                empty_count = 0
            else:
                empty_count += 1
        
        step.user_feedback = '\n'.join(feedback_lines)
        step.attempts += 1
        
        # Log feedback
        self.feedback_log.append({
            'timestamp': datetime.now().isoformat(),
            'step': step.number,
            'feedback': step.user_feedback,
            'attempt': step.attempts
        })
        
        print("\n✅ Thank you for the feedback! I've saved it.")
    
    def ask_for_revision_cli(self, step: VerificationStep) -> bool:
        """Ask if user wants AI to revise the solution"""
        print("\n" + "-"*60)
        print("🤖 Would you like me to try fixing this?")
        print("   I can use your feedback to improve the solution!")
        print("-"*60)
        
        response = input("👉 Fix it? (yes/no): ").strip().lower()
        return response == 'yes'
    
    def request_ai_revision(self, step: VerificationStep):
        """Simulate AI revision based on feedback"""
        print("\n🔧 Working on a fix based on your feedback...")
        print("   ⏳ Analyzing the problem...")
        time.sleep(1)
        print("   ⏳ Generating solution...")
        time.sleep(1)
        
        # Simulate AI response
        step.ai_response = f"""
Based on your feedback, here's what I'll adjust:

1. Issue identified: {step.actual_outcome[:100] if step.actual_outcome else 'Task did not complete as expected'}

2. Proposed fix:
   - Revise the approach to handle the reported issue
   - Add error handling for the specific case
   - Provide clearer instructions

3. New approach:
   {self.generate_revised_approach(step)}

Would you like to try again with this revised approach?
"""
        
        print(step.ai_response)
        
        retry = input("\n👉 Try again with the fix? (yes/no): ").strip().lower()
        if retry == 'yes':
            print("\n🔄 Great! Let's try again with the improved approach...")
            step.status = StepStatus.IN_PROGRESS
            self.verify_step_cli(step)
    
    def generate_revised_approach(self, step: VerificationStep) -> str:
        """Generate a revised approach based on feedback"""
        if "error" in step.user_feedback.lower():
            return "• Add better error handling\n   • Provide clearer error messages\n   • Include recovery steps"
        elif "confus" in step.user_feedback.lower():
            return "• Simplify the instructions\n   • Add more examples\n   • Break into smaller steps"
        else:
            return "• Adjust the implementation\n   • Add validation checks\n   • Improve user guidance"
    
    def show_step_summary_cli(self, step: VerificationStep):
        """Show summary after each step"""
        print("\n" + "="*60)
        print(f"📊 Step {step.number} Summary")
        print("="*60)
        print(f"Status: {step.status.value}")
        print(f"Time spent: {step.time_spent:.1f} seconds")
        print(f"Attempts: {step.attempts}")
        
        if step.status == StepStatus.COMPLETED:
            print("🎉 Well done! Moving to the next step...")
        elif step.status == StepStatus.SKIPPED:
            print("⏭️ Skipped - Moving on...")
        else:
            print("🔧 This step needs more work, but that's okay!")
    
    def show_final_summary_cli(self):
        """Show final summary of verification session"""
        total_time = time.time() - self.start_time
        completed = sum(1 for s in self.steps if s.status == StepStatus.COMPLETED)
        failed = sum(1 for s in self.steps if s.status == StepStatus.FAILED)
        revised = sum(1 for s in self.steps if s.status == StepStatus.NEEDS_REVISION)
        skipped = sum(1 for s in self.steps if s.status == StepStatus.SKIPPED)
        
        print("\n" + "🎊"*30)
        print("\n📊 VERIFICATION COMPLETE!")
        print("="*70)
        print(f"✅ Completed: {completed}/{len(self.steps)}")
        print(f"❌ Failed: {failed}")
        print(f"🔧 Needs Revision: {revised}")
        print(f"⏭️ Skipped: {skipped}")
        print(f"⏱️ Total Time: {total_time:.1f} seconds")
        print("="*70)
        
        # Performance rating
        success_rate = (completed / len(self.steps)) * 100 if self.steps else 0
        if success_rate >= 90:
            print("\n🏆 EXCELLENT WORK! You're a superstar! 🌟")
        elif success_rate >= 70:
            print("\n👍 GREAT JOB! You did really well! 🎯")
        elif success_rate >= 50:
            print("\n😊 GOOD EFFORT! You're learning! 📚")
        else:
            print("\n💪 KEEP TRYING! Practice makes perfect! 🚀")
        
        # Save session report
        self.save_session_report()
    
    def save_session_report(self):
        """Save detailed session report to verification_report subdirectory"""
        # Ensure verification_report directory exists
        if not self.verification_report_dir:
            self.verification_report_dir = Path.cwd() / "verification_report"
        
        self.verification_report_dir.mkdir(exist_ok=True)
        
        # Use YYYYMMDD_HHMMSS.json format
        report_file = self.verification_report_dir / f"{self.session_id}.json"
        
        report = {
            'session_id': self.session_id,
            'timestamp': datetime.now().isoformat(),
            'total_steps': len(self.steps),
            'completed': sum(1 for s in self.steps if s.status == StepStatus.COMPLETED),
            'total_time': time.time() - self.start_time if self.start_time else 0,
            'steps': [asdict(step) for step in self.steps],
            'feedback_log': self.feedback_log
        }
        
        # Convert enums to strings
        for step in report['steps']:
            step['status'] = step['status'].value
        
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2, default=str)
        
        print(f"\n📄 Report saved to: {report_file}")
        print(f"📂 Location: {report_file.absolute()}")
    
    def run_gui_mode(self):
        """Run with GUI interface (if tkinter available)"""
        if not HAS_GUI:
            print("⚠️ GUI not available. Install tkinter or use CLI mode.")
            return self.run_cli_mode()
        
        self.gui = VerificationGUI(self)
        self.gui.run()

class VerificationGUI:
    """Lightweight GUI using tkinter"""
    
    def __init__(self, verifier: InteractiveVerifier):
        self.verifier = verifier
        self.root = tk.Tk()
        self.root.title("🤖 AI Work Verification System")
        self.root.geometry("900x700")
        
        self.setup_ui()
        self.current_step_idx = 0
        
    def setup_ui(self):
        """Setup the GUI interface"""
        # Main container
        main_frame = ttk.Frame(self.root, padding="10")
        main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Progress section
        progress_frame = ttk.LabelFrame(main_frame, text="Progress", padding="10")
        progress_frame.grid(row=0, column=0, columnspan=2, sticky=(tk.W, tk.E), pady=5)
        
        self.progress_var = tk.DoubleVar()
        self.progress_bar = ttk.Progressbar(progress_frame, variable=self.progress_var, maximum=100)
        self.progress_bar.grid(row=0, column=0, sticky=(tk.W, tk.E), padx=5)
        
        self.progress_label = ttk.Label(progress_frame, text="Step 0 of 0")
        self.progress_label.grid(row=0, column=1, padx=5)
        
        # Step display
        step_frame = ttk.LabelFrame(main_frame, text="Current Step", padding="10")
        step_frame.grid(row=1, column=0, columnspan=2, sticky=(tk.W, tk.E, tk.N, tk.S), pady=5)
        
        self.step_text = scrolledtext.ScrolledText(step_frame, height=15, width=80, wrap=tk.WORD)
        self.step_text.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Feedback section
        feedback_frame = ttk.LabelFrame(main_frame, text="Your Feedback", padding="10")
        feedback_frame.grid(row=2, column=0, columnspan=2, sticky=(tk.W, tk.E), pady=5)
        
        self.feedback_text = scrolledtext.ScrolledText(feedback_frame, height=5, width=80, wrap=tk.WORD)
        self.feedback_text.grid(row=0, column=0, sticky=(tk.W, tk.E))
        
        # Control buttons
        button_frame = ttk.Frame(main_frame)
        button_frame.grid(row=3, column=0, columnspan=2, pady=10)
        
        self.start_btn = ttk.Button(button_frame, text="✅ Start Verification", command=self.start_verification)
        self.start_btn.grid(row=0, column=0, padx=5)
        
        self.complete_btn = ttk.Button(button_frame, text="✓ Step Complete", command=self.complete_step, state="disabled")
        self.complete_btn.grid(row=0, column=1, padx=5)
        
        self.fail_btn = ttk.Button(button_frame, text="✗ Step Failed", command=self.fail_step, state="disabled")
        self.fail_btn.grid(row=0, column=2, padx=5)
        
        self.skip_btn = ttk.Button(button_frame, text="⏭ Skip Step", command=self.skip_step, state="disabled")
        self.skip_btn.grid(row=0, column=3, padx=5)
        
        self.revise_btn = ttk.Button(button_frame, text="🔧 Request Revision", command=self.request_revision, state="disabled")
        self.revise_btn.grid(row=0, column=4, padx=5)
        
        # Make grid expandable
        self.root.columnconfigure(0, weight=1)
        self.root.rowconfigure(0, weight=1)
        main_frame.columnconfigure(0, weight=1)
        main_frame.rowconfigure(1, weight=1)
        step_frame.columnconfigure(0, weight=1)
        step_frame.rowconfigure(0, weight=1)
    
    def start_verification(self):
        """Start the verification process"""
        self.verifier.start_time = time.time()
        self.start_btn.config(state="disabled")
        self.complete_btn.config(state="normal")
        self.fail_btn.config(state="normal")
        self.skip_btn.config(state="normal")
        
        if self.verifier.steps:
            self.show_current_step()
    
    def show_current_step(self):
        """Display the current step"""
        if self.current_step_idx >= len(self.verifier.steps):
            self.finish_verification()
            return
        
        step = self.verifier.steps[self.current_step_idx]
        step.status = StepStatus.IN_PROGRESS
        
        # Update progress
        progress = (self.current_step_idx / len(self.verifier.steps)) * 100
        self.progress_var.set(progress)
        self.progress_label.config(text=f"Step {self.current_step_idx + 1} of {len(self.verifier.steps)}")
        
        # Display step
        self.step_text.delete(1.0, tk.END)
        self.step_text.insert(tk.END, step.to_simple_text())
        
        # Clear feedback
        self.feedback_text.delete(1.0, tk.END)
    
    def complete_step(self):
        """Mark current step as complete"""
        if self.current_step_idx < len(self.verifier.steps):
            step = self.verifier.steps[self.current_step_idx]
            step.status = StepStatus.COMPLETED
            step.user_feedback = self.feedback_text.get(1.0, tk.END).strip()
            
            messagebox.showinfo("Success", "🎉 Great job! Step completed successfully!")
            self.current_step_idx += 1
            self.show_current_step()
    
    def fail_step(self):
        """Mark current step as failed"""
        if self.current_step_idx < len(self.verifier.steps):
            step = self.verifier.steps[self.current_step_idx]
            step.status = StepStatus.NEEDS_REVISION
            step.user_feedback = self.feedback_text.get(1.0, tk.END).strip()
            
            if step.user_feedback:
                self.revise_btn.config(state="normal")
                messagebox.showwarning("Step Failed", "Step needs revision. You can request AI help!")
            else:
                messagebox.showwarning("Feedback Needed", "Please provide feedback about what went wrong.")
    
    def skip_step(self):
        """Skip current step"""
        if self.current_step_idx < len(self.verifier.steps):
            step = self.verifier.steps[self.current_step_idx]
            step.status = StepStatus.SKIPPED
            
            self.current_step_idx += 1
            self.show_current_step()
    
    def request_revision(self):
        """Request AI revision for failed step"""
        if self.current_step_idx < len(self.verifier.steps):
            step = self.verifier.steps[self.current_step_idx]
            
            # Simulate AI revision
            revision = self.verifier.generate_revised_approach(step)
            step.ai_response = f"AI Revision:\n{revision}"
            
            messagebox.showinfo("AI Revision", f"AI suggests:\n\n{revision}\n\nTry again with this approach!")
            self.revise_btn.config(state="disabled")
    
    def finish_verification(self):
        """Complete the verification process"""
        self.complete_btn.config(state="disabled")
        self.fail_btn.config(state="disabled")
        self.skip_btn.config(state="disabled")
        self.revise_btn.config(state="disabled")
        
        # Show summary
        completed = sum(1 for s in self.verifier.steps if s.status == StepStatus.COMPLETED)
        total = len(self.verifier.steps)
        
        summary = f"""
Verification Complete!

✅ Completed: {completed}/{total}
⏱️ Time: {time.time() - self.verifier.start_time:.1f} seconds

{'🏆 Excellent work!' if completed == total else '💪 Good effort! Keep practicing!'}
"""
        
        messagebox.showinfo("Verification Complete", summary)
        self.verifier.save_session_report()
    
    def run(self):
        """Run the GUI"""
        self.root.mainloop()

def create_sample_tasks():
    """Create sample verification tasks in current directory"""
    sample_tasks = {
        "steps": [
            {
                "number": 1,
                "title": "Check if the website loads",
                "description": "We need to make sure the website opens in your browser without any errors",
                "instructions": [
                    "Open your web browser (Chrome, Firefox, or Safari)",
                    "Type the website address in the address bar",
                    "Press Enter to load the website",
                    "Wait for the page to fully load"
                ],
                "expected_outcome": "The website should load completely with all images and text visible"
            },
            {
                "number": 2,
                "title": "Test the login button",
                "description": "Let's check if the login button works properly",
                "instructions": [
                    "Find the 'Login' button on the page (usually at the top right)",
                    "Click on the Login button",
                    "Check if a login form appears"
                ],
                "expected_outcome": "A login form with username and password fields should appear"
            },
            {
                "number": 3,
                "title": "Verify the search feature",
                "description": "Make sure the search box helps you find things",
                "instructions": [
                    "Find the search box on the page",
                    "Type 'test' in the search box",
                    "Press Enter or click the search button",
                    "Look at the results that appear"
                ],
                "expected_outcome": "Search results related to 'test' should appear on the page"
            }
        ]
    }
    
    sample_file = Path("sample_verification_tasks.json")
    with open(sample_file, 'w') as f:
        json.dump(sample_tasks, f, indent=2)
    
    return sample_file

def validate_spec_folder() -> Tuple[bool, Optional[Path], str]:
    """Validate that current directory is a spec folder
    
    Returns:
        Tuple of (is_valid, spec_path, message)
    """
    current_dir = Path.cwd()
    
    # Check if we're in a spec folder (contains spec.md or is under .agent-os/specs/)
    spec_indicators = [
        current_dir / "spec.md",
        current_dir / "tasks.md",
        current_dir.parent / "spec.md" if current_dir.name == "sub-specs" else None
    ]
    
    # Check if we're in an agent-os spec directory
    is_spec_folder = False
    spec_path = None
    
    for indicator in spec_indicators:
        if indicator and indicator.exists():
            is_spec_folder = True
            spec_path = indicator.parent
            break
    
    # Also check if path contains .agent-os/specs/
    if not is_spec_folder:
        path_parts = current_dir.parts
        if '.agent-os' in path_parts and 'specs' in path_parts:
            agent_os_idx = path_parts.index('.agent-os')
            if agent_os_idx + 1 < len(path_parts) and path_parts[agent_os_idx + 1] == 'specs':
                is_spec_folder = True
                spec_path = current_dir
    
    if is_spec_folder:
        return True, spec_path, f"✅ Valid spec folder: {spec_path}"
    else:
        # Look for nearby spec folders
        suggestions = []
        
        # Check parent directories
        for parent in current_dir.parents:
            agent_os_specs = parent / ".agent-os" / "specs"
            if agent_os_specs.exists():
                spec_folders = [d for d in agent_os_specs.iterdir() if d.is_dir()]
                suggestions.extend(spec_folders[:3])  # Limit to 3 suggestions
                break
        
        # Check current directory for .agent-os/specs
        local_specs = current_dir / ".agent-os" / "specs"
        if local_specs.exists():
            spec_folders = [d for d in local_specs.iterdir() if d.is_dir()]
            suggestions.extend(spec_folders[:3])
        
        msg = "❌ ERROR: /verify-ai-work must be run from a spec folder!\n\n"
        msg += "📁 A spec folder contains:\n"
        msg += "   • spec.md file\n"
        msg += "   • tasks.md file\n"
        msg += "   • Or is located under .agent-os/specs/\n"
        
        if suggestions:
            msg += "\n📍 Found these spec folders you can navigate to:\n"
            for folder in suggestions[:3]:
                msg += f"   cd {folder}\n"
        else:
            msg += "\n💡 To create a spec folder:\n"
            msg += "   1. Navigate to your project root\n"
            msg += "   2. Run: /create-spec to create a new spec\n"
            msg += "   3. Navigate to the created spec folder\n"
            msg += "   4. Run: /verify-ai-work\n"
        
        return False, None, msg

def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Interactive AI Work Verification System (MUST run in spec folder)",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
MANDATORY: This command must be executed from within a spec folder!

Examples:
  cd .agent-os/specs/2025-01-15-my-feature/
  verify-ai-work tasks.json              # Load tasks from JSON file
  verify-ai-work tasks.md                # Load tasks from Markdown  
  verify-ai-work --sample                # Use sample tasks
  verify-ai-work --gui tasks.json        # Use GUI mode
  verify-ai-work --cli tasks.json        # Force CLI mode

Reports are saved to: ./verification_report/YYYYMMDD_HHMMSS.json
        """
    )
    
    parser.add_argument('task_file', nargs='?', 
                       help='Path to task file (JSON or Markdown)')
    parser.add_argument('--sample', action='store_true',
                       help='Use sample verification tasks')
    parser.add_argument('--gui', action='store_true',
                       help='Use GUI mode (if available)')
    parser.add_argument('--cli', action='store_true',
                       help='Force CLI mode')
    parser.add_argument('--create-template', action='store_true',
                       help='Create a template task file')
    
    args = parser.parse_args()
    
    # MANDATORY: Validate spec folder FIRST (unless creating template)
    if not args.create_template:
        is_valid, spec_path, message = validate_spec_folder()
        
        if not is_valid:
            print(message)
            print("\n⚠️  You must navigate to a spec folder before running verification!")
            sys.exit(1)
        else:
            print(message)
            print()
    
    # Create template if requested
    if args.create_template:
        template_file = create_sample_tasks()
        print(f"✅ Template created: {template_file}")
        print("   Edit this file with your verification tasks!")
        return
    
    # Determine task file
    if args.sample:
        task_file = create_sample_tasks()
        print(f"📝 Using sample tasks from: {task_file}")
    elif args.task_file:
        task_file = Path(args.task_file)
        if not task_file.exists():
            print(f"❌ Task file not found: {task_file}")
            print("   Use --sample for example tasks or --create-template to create a template")
            sys.exit(1)
    else:
        print("❌ No task file specified!")
        print("   Use --sample for example tasks or provide a task file")
        print("\nUsage:")
        print("  verify-ai-work tasks.json")
        print("  verify-ai-work --sample")
        print("  verify-ai-work --create-template")
        sys.exit(1)
    
    # Initialize verifier
    verifier = InteractiveVerifier()
    
    # Set spec folder and report directory
    verifier.spec_folder = Path.cwd()
    verifier.verification_report_dir = verifier.spec_folder / "verification_report"
    
    # Load tasks
    if not verifier.load_tasks(task_file):
        sys.exit(1)
    
    print(f"📂 Reports will be saved to: {verifier.verification_report_dir}/")
    print()
    
    # Run verification
    if args.gui and not args.cli:
        verifier.run_gui_mode()
    else:
        verifier.run_cli_mode()

if __name__ == "__main__":
    main()