"""Interactive mode for Agent OS CLI.

Provides an interactive command-line interface with enhanced user experience.
"""

import sys
from typing import Optional, List, Dict, Any


class InteractiveMode:
    """Interactive command line interface for Agent OS."""
    
    def __init__(self):
        """Initialize interactive mode."""
        self.history: List[str] = []
        self.max_history = 100
        
    def get_command(self) -> str:
        """Get command from user with enhanced input.
        
        Returns:
            User command string
        """
        try:
            # Use a simple prompt for now
            # In a full implementation, this could use readline for history/completion
            command = input("agent-os> ").strip()
            
            # Add to history
            if command and command not in ['', 'help', 'quit', 'exit']:
                self.add_to_history(command)
            
            return command
            
        except EOFError:
            return 'quit'
        except KeyboardInterrupt:
            print()  # New line after ^C
            return ''
    
    def add_to_history(self, command: str) -> None:
        """Add command to history.
        
        Args:
            command: Command to add
        """
        # Remove duplicates and add to end
        if command in self.history:
            self.history.remove(command)
        
        self.history.append(command)
        
        # Keep only last max_history entries
        if len(self.history) > self.max_history:
            self.history = self.history[-self.max_history:]
    
    def get_history(self) -> List[str]:
        """Get command history.
        
        Returns:
            List of historical commands
        """
        return self.history.copy()
    
    def show_history(self, limit: Optional[int] = 10) -> None:
        """Show command history.
        
        Args:
            limit: Maximum number of entries to show
        """
        if not self.history:
            print("No command history available.")
            return
        
        print("Recent commands:")
        recent_history = self.history[-limit:] if limit else self.history
        
        for i, command in enumerate(recent_history, 1):
            print(f"  {i:2d}: {command}")
    
    def clear_history(self) -> None:
        """Clear command history."""
        self.history.clear()
        print("Command history cleared.")
    
    def get_input_with_prompt(self, prompt: str, default: Optional[str] = None) -> str:
        """Get input with a custom prompt.
        
        Args:
            prompt: Prompt to display
            default: Default value if user presses enter
            
        Returns:
            User input string
        """
        if default:
            prompt_text = f"{prompt} [{default}]: "
        else:
            prompt_text = f"{prompt}: "
        
        try:
            user_input = input(prompt_text).strip()
            return user_input if user_input else (default or '')
        except (EOFError, KeyboardInterrupt):
            return default or ''
    
    def get_yes_no(self, prompt: str, default: bool = True) -> bool:
        """Get yes/no input from user.
        
        Args:
            prompt: Prompt to display
            default: Default value
            
        Returns:
            True for yes, False for no
        """
        default_text = "Y/n" if default else "y/N"
        response = self.get_input_with_prompt(f"{prompt} ({default_text})")
        
        if not response:
            return default
        
        return response.lower() in ['y', 'yes', 'true', '1']
    
    def select_from_list(self, prompt: str, options: List[str], 
                        default: Optional[int] = None) -> Optional[str]:
        """Get selection from a list of options.
        
        Args:
            prompt: Prompt to display
            options: List of options
            default: Default option index
            
        Returns:
            Selected option or None
        """
        if not options:
            return None
        
        print(f"\n{prompt}")
        for i, option in enumerate(options, 1):
            marker = " (default)" if default is not None and i - 1 == default else ""
            print(f"  {i}: {option}{marker}")
        
        while True:
            try:
                response = self.get_input_with_prompt("Select option (number)")
                
                if not response and default is not None:
                    return options[default]
                
                if not response:
                    return None
                
                index = int(response) - 1
                if 0 <= index < len(options):
                    return options[index]
                else:
                    print(f"Invalid selection. Please enter 1-{len(options)}.")
                    
            except ValueError:
                print("Invalid input. Please enter a number.")
            except (EOFError, KeyboardInterrupt):
                return None
    
    def get_module_agent_details(self) -> Dict[str, Any]:
        """Interactively get module agent creation details.
        
        Returns:
            Dictionary with agent details
        """
        print("\n🤖 Create Module Agent - Interactive Setup")
        print("=" * 50)
        
        # Get module name
        while True:
            module_name = self.get_input_with_prompt("Module name")
            if module_name:
                break
            print("Module name is required.")
        
        # Get agent type
        agent_types = [
            "general-purpose",
            "infrastructure", 
            "api",
            "data-processing",
            "monitoring",
            "security",
            "custom"
        ]
        
        selected_type = self.select_from_list(
            "Select agent type:",
            agent_types,
            default=0
        )
        
        if selected_type == "custom":
            selected_type = self.get_input_with_prompt("Enter custom agent type")
        
        # Get repositories
        repos = []
        if self.get_yes_no("Include repository references?"):
            print("\nAvailable repositories:")
            available_repos = [
                "assetutilities",
                "pyproject-starter", 
                "worldenergydata",
                "aceengineer-website",
                "frontierdeepwater",
                "OGManufacturing"
            ]
            
            for repo in available_repos:
                if self.get_yes_no(f"Include {repo}?", default=False):
                    repos.append(repo)
            
            # Allow custom repos
            while self.get_yes_no("Add custom repository?", default=False):
                custom_repo = self.get_input_with_prompt("Repository name")
                if custom_repo:
                    repos.append(custom_repo)
        
        # Get templates
        templates = []
        if self.get_yes_no("Use custom templates?", default=False):
            while self.get_yes_no("Add template?", default=False):
                template = self.get_input_with_prompt("Template name")
                if template:
                    templates.append(template)
        
        # Context caching
        context_cache = self.get_yes_no("Enable context caching?", default=True)
        
        return {
            "module_name": module_name,
            "type": selected_type,
            "repos": repos,
            "templates": templates,
            "context_cache": context_cache
        }