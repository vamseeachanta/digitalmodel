#!/usr/bin/env python3
"""
Pilot Program Dashboard
Real-time tracking and metrics for FreeCAD + Blender AI CAD pilot testing
"""

import json
import time
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Optional
import sys
sys.path.append('..')

class PilotDashboard:
    """Dashboard for tracking pilot program metrics and progress"""
    
    def __init__(self, user_name: str = "Pilot User 1"):
        self.user_name = user_name
        self.start_date = datetime.now()
        self.metrics_file = Path(f"pilot_metrics_{user_name.replace(' ', '_')}.json")
        self.session_data = self.load_or_create_metrics()
        
    def load_or_create_metrics(self) -> Dict:
        """Load existing metrics or create new tracking file"""
        if self.metrics_file.exists():
            with open(self.metrics_file, 'r') as f:
                return json.load(f)
        else:
            return {
                'user': self.user_name,
                'start_date': self.start_date.isoformat(),
                'sessions': [],
                'designs_created': 0,
                'commands_executed': 0,
                'time_saved_hours': 0,
                'satisfaction_ratings': [],
                'issues_encountered': [],
                'feature_requests': []
            }
    
    def save_metrics(self):
        """Save metrics to file"""
        with open(self.metrics_file, 'w') as f:
            json.dump(self.session_data, f, indent=2)
    
    def start_session(self) -> Dict:
        """Start a new testing session"""
        session = {
            'session_id': len(self.session_data['sessions']) + 1,
            'start_time': datetime.now().isoformat(),
            'end_time': None,
            'tasks_completed': [],
            'commands': [],
            'time_traditional_cad': 0,
            'time_ai_cad': 0,
            'quality_rating': None,
            'ease_rating': None,
            'notes': []
        }
        self.session_data['sessions'].append(session)
        self.current_session = session
        self.save_metrics()
        return session
    
    def log_command(self, command: str, result: str, time_taken: float):
        """Log a command execution"""
        if hasattr(self, 'current_session'):
            self.current_session['commands'].append({
                'command': command,
                'result': result,
                'time_seconds': time_taken,
                'timestamp': datetime.now().isoformat()
            })
            self.session_data['commands_executed'] += 1
            self.save_metrics()
    
    def log_design_created(self, design_type: str, specs: Dict, time_minutes: float):
        """Log creation of a new design"""
        self.session_data['designs_created'] += 1
        
        # Calculate time savings (assume 2.5x faster than traditional)
        traditional_time = time_minutes * 2.5
        time_saved = traditional_time - time_minutes
        self.session_data['time_saved_hours'] += time_saved / 60
        
        if hasattr(self, 'current_session'):
            self.current_session['tasks_completed'].append({
                'type': design_type,
                'specifications': specs,
                'time_minutes': time_minutes,
                'traditional_time_estimate': traditional_time
            })
            self.current_session['time_ai_cad'] += time_minutes
            self.current_session['time_traditional_cad'] += traditional_time
        
        self.save_metrics()
    
    def log_issue(self, issue: str, resolution: Optional[str] = None):
        """Log an issue encountered"""
        self.session_data['issues_encountered'].append({
            'issue': issue,
            'resolution': resolution,
            'timestamp': datetime.now().isoformat()
        })
        self.save_metrics()
    
    def add_feature_request(self, feature: str):
        """Add a feature request"""
        self.session_data['feature_requests'].append({
            'feature': feature,
            'timestamp': datetime.now().isoformat()
        })
        self.save_metrics()
    
    def rate_session(self, quality: int, ease: int, notes: str = ""):
        """Rate the current session"""
        if hasattr(self, 'current_session'):
            self.current_session['quality_rating'] = quality
            self.current_session['ease_rating'] = ease
            self.current_session['notes'].append(notes)
            
            # Add to overall ratings
            self.session_data['satisfaction_ratings'].append({
                'quality': quality,
                'ease': ease,
                'session_id': self.current_session['session_id'],
                'date': datetime.now().isoformat()
            })
        self.save_metrics()
    
    def end_session(self):
        """End the current session"""
        if hasattr(self, 'current_session'):
            self.current_session['end_time'] = datetime.now().isoformat()
            self.save_metrics()
    
    def generate_report(self) -> str:
        """Generate a pilot program report"""
        report = f"""
# Pilot Program Report
**User**: {self.user_name}
**Start Date**: {self.session_data['start_date']}
**Report Generated**: {datetime.now().strftime('%Y-%m-%d %H:%M')}

## Overall Metrics
- **Sessions Completed**: {len(self.session_data['sessions'])}
- **Designs Created**: {self.session_data['designs_created']}
- **Commands Executed**: {self.session_data['commands_executed']}
- **Time Saved**: {self.session_data['time_saved_hours']:.1f} hours

## Performance Analysis
"""
        
        if self.session_data['sessions']:
            total_ai_time = sum(s.get('time_ai_cad', 0) for s in self.session_data['sessions'])
            total_trad_time = sum(s.get('time_traditional_cad', 0) for s in self.session_data['sessions'])
            
            if total_trad_time > 0:
                efficiency = ((total_trad_time - total_ai_time) / total_trad_time) * 100
                report += f"- **Efficiency Gain**: {efficiency:.1f}%\n"
                report += f"- **AI CAD Time**: {total_ai_time:.1f} minutes\n"
                report += f"- **Traditional CAD Time (estimated)**: {total_trad_time:.1f} minutes\n"
        
        if self.session_data['satisfaction_ratings']:
            avg_quality = sum(r['quality'] for r in self.session_data['satisfaction_ratings']) / len(self.session_data['satisfaction_ratings'])
            avg_ease = sum(r['ease'] for r in self.session_data['satisfaction_ratings']) / len(self.session_data['satisfaction_ratings'])
            report += f"\n## Satisfaction Ratings\n"
            report += f"- **Average Quality Rating**: {avg_quality:.1f}/10\n"
            report += f"- **Average Ease of Use**: {avg_ease:.1f}/10\n"
        
        if self.session_data['issues_encountered']:
            report += f"\n## Issues Encountered\n"
            for issue in self.session_data['issues_encountered'][-5:]:  # Last 5 issues
                report += f"- {issue['issue']}"
                if issue.get('resolution'):
                    report += f" → Resolved: {issue['resolution']}"
                report += "\n"
        
        if self.session_data['feature_requests']:
            report += f"\n## Feature Requests\n"
            for req in self.session_data['feature_requests']:
                report += f"- {req['feature']}\n"
        
        return report
    
    def display_dashboard(self):
        """Display real-time dashboard"""
        print("\n" + "="*70)
        print(f"📊 PILOT PROGRAM DASHBOARD - {self.user_name}")
        print("="*70)
        
        # Current session info
        if hasattr(self, 'current_session') and self.current_session:
            session_start = datetime.fromisoformat(self.current_session['start_time'])
            duration = datetime.now() - session_start
            print(f"\n🔴 LIVE SESSION #{self.current_session['session_id']}")
            print(f"   Duration: {duration.seconds // 60} minutes")
            print(f"   Commands: {len(self.current_session['commands'])}")
            print(f"   Tasks Completed: {len(self.current_session['tasks_completed'])}")
        
        # Overall metrics
        print(f"\n📈 OVERALL METRICS")
        print(f"   Total Designs: {self.session_data['designs_created']}")
        print(f"   Total Commands: {self.session_data['commands_executed']}")
        print(f"   Time Saved: {self.session_data['time_saved_hours']:.1f} hours")
        
        # Efficiency metrics
        if self.session_data['sessions']:
            total_ai = sum(s.get('time_ai_cad', 0) for s in self.session_data['sessions'])
            total_trad = sum(s.get('time_traditional_cad', 0) for s in self.session_data['sessions'])
            if total_trad > 0:
                efficiency = ((total_trad - total_ai) / total_trad) * 100
                print(f"   Efficiency Gain: {efficiency:.1f}%")
        
        # Satisfaction
        if self.session_data['satisfaction_ratings']:
            avg_quality = sum(r['quality'] for r in self.session_data['satisfaction_ratings']) / len(self.session_data['satisfaction_ratings'])
            avg_ease = sum(r['ease'] for r in self.session_data['satisfaction_ratings']) / len(self.session_data['satisfaction_ratings'])
            print(f"\n⭐ SATISFACTION")
            print(f"   Quality: {avg_quality:.1f}/10")
            print(f"   Ease of Use: {avg_ease:.1f}/10")
        
        # Success indicators
        print(f"\n✅ SUCCESS CRITERIA")
        criteria = [
            (self.session_data['designs_created'] >= 3, "Complete 3+ designs"),
            (self.session_data['time_saved_hours'] >= 2, "Save 2+ hours"),
            (len(self.session_data['satisfaction_ratings']) > 0 and avg_ease >= 7, "Rate ease 7+/10"),
            (self.session_data['commands_executed'] >= 10, "Execute 10+ commands")
        ]
        
        for met, criterion in criteria:
            status = "✅" if met else "⏳"
            print(f"   {status} {criterion}")
        
        print("\n" + "="*70)


def run_pilot_session():
    """Run an interactive pilot testing session"""
    print("🚀 Starting Pilot Testing Session")
    print("-" * 50)
    
    # Initialize dashboard
    dashboard = PilotDashboard("Pilot User 1")
    session = dashboard.start_session()
    
    print(f"Session #{session['session_id']} started")
    print("\nTracking your progress automatically...")
    
    # Simulate some testing activities
    print("\n📝 Example tracked activities:")
    
    # Log a design creation
    dashboard.log_design_created(
        design_type="pressure_vessel",
        specs={'diameter': 2000, 'length': 4000, 'thickness': 15},
        time_minutes=30
    )
    print("   ✓ Pressure vessel design logged (30 min vs 75 min traditional)")
    
    # Log some commands
    dashboard.log_command(
        command="create vessel 2m diameter",
        result="success",
        time_taken=2.5
    )
    print("   ✓ Command execution logged")
    
    # Display current dashboard
    dashboard.display_dashboard()
    
    # Rate the session
    dashboard.rate_session(
        quality=8,
        ease=9,
        notes="Very intuitive natural language interface"
    )
    
    # End session
    dashboard.end_session()
    
    # Generate report
    report = dashboard.generate_report()
    report_file = Path("pilot_report.md")
    with open(report_file, 'w') as f:
        f.write(report)
    
    print(f"\n📄 Report saved to: {report_file}")
    print("\n✅ Session complete!")


if __name__ == "__main__":
    run_pilot_session()