"""Main CLI entry point for Agent OS commands.

Provides a unified command line interface for all Agent OS functionality.
"""

import sys
import argparse
from pathlib import Path
from typing import List, Dict, Any, Optional

from agent_os.commands.create_module_agent import CreateModuleAgentCommand
from .interactive import InteractiveMode
from .progress import ProgressIndicator


class AgentOSCLI:
    """Main CLI class for Agent OS commands."""
    
    def __init__(self):
        """Initialize CLI."""
        self.commands = {
            'create-module-agent': CreateModuleAgentCommand,
        }
        self.interactive_mode = InteractiveMode()
        self.progress = ProgressIndicator()
        
    def create_parser(self) -> argparse.ArgumentParser:
        """Create argument parser.
        
        Returns:
            Configured argument parser
        """
        parser = argparse.ArgumentParser(
            prog='agent-os',
            description='Agent OS Command Line Interface',
            formatter_class=argparse.RawDescriptionHelpFormatter,
            epilog="""
Examples:
  agent-os create-module-agent my-module
  agent-os create-module-agent my-module --type infrastructure --repos repo1,repo2
  agent-os --interactive
            """
        )
        
        # Global options
        parser.add_argument(
            '--interactive', '-i',
            action='store_true',
            help='Run in interactive mode'
        )
        parser.add_argument(
            '--verbose', '-v',
            action='store_true',
            help='Verbose output'
        )
        parser.add_argument(
            '--quiet', '-q',
            action='store_true',
            help='Quiet output (errors only)'
        )
        
        # Subcommands
        subparsers = parser.add_subparsers(
            dest='command',
            title='Available commands',
            help='Agent OS commands'
        )
        
        # create-module-agent subcommand
        create_agent_parser = subparsers.add_parser(
            'create-module-agent',
            help='Create a new module agent',
            description='Create a specialized AI agent for a module with optimized context and documentation integration'
        )
        create_agent_parser.add_argument(
            'module_name',
            help='Name of the module agent to create'
        )
        create_agent_parser.add_argument(
            '--type',
            default='general-purpose',
            help='Type of agent (default: general-purpose)'
        )
        create_agent_parser.add_argument(
            '--repos',
            help='Comma-separated list of repositories to include'
        )
        create_agent_parser.add_argument(
            '--context-cache',
            type=bool,
            default=True,
            help='Enable context caching (default: True)'
        )
        create_agent_parser.add_argument(
            '--templates',
            help='Comma-separated list of templates to use'
        )
        create_agent_parser.add_argument(
            '--output-dir',
            type=Path,
            default=Path.cwd(),
            help='Output directory for agent (default: current directory)'
        )
        
        return parser
    
    def run_interactive(self) -> int:
        """Run in interactive mode.
        
        Returns:
            Exit code
        """
        print("🤖 Agent OS Interactive Mode")
        print("Type 'help' for available commands or 'quit' to exit.\n")
        
        while True:
            try:
                command = self.interactive_mode.get_command()
                
                if command.lower() in ['quit', 'exit', 'q']:
                    print("👋 Goodbye!")
                    return 0
                elif command.lower() in ['help', 'h', '?']:
                    self.show_help()
                    continue
                elif command.strip() == '':
                    continue
                
                # Parse and execute command
                result = self.execute_command_string(command)
                
                if result != 0:
                    print(f"❌ Command failed with exit code {result}")
                else:
                    print("✅ Command completed successfully")
                    
            except KeyboardInterrupt:
                print("\n👋 Goodbye!")
                return 0
            except Exception as e:
                print(f"❌ Error: {e}")
                
    def show_help(self) -> None:
        """Show interactive help."""
        print("""
Available commands:
  create-module-agent <name>              Create a new module agent
  create-module-agent <name> --help       Show detailed help for create-module-agent
  help                                     Show this help
  quit                                     Exit interactive mode

Examples:
  create-module-agent finance-analytics
  create-module-agent devops --type infrastructure --repos assetutilities,pyproject-starter
        """)
    
    def execute_command_string(self, command_string: str) -> int:
        """Execute a command from string.
        
        Args:
            command_string: Command string to execute
            
        Returns:
            Exit code
        """
        # Split command string into arguments
        args = command_string.strip().split()
        
        if not args:
            return 0
        
        # Handle slash commands (convert /command to command)
        if args[0].startswith('/'):
            args[0] = args[0][1:]
        
        # Map common aliases
        if args[0] == 'create-module-agent':
            return self.execute_create_module_agent(args[1:])
        else:
            print(f"❌ Unknown command: {args[0]}")
            print("Type 'help' for available commands.")
            return 1
    
    def execute_create_module_agent(self, args: List[str]) -> int:
        """Execute create-module-agent command.
        
        Args:
            args: Command arguments
            
        Returns:
            Exit code
        """
        if not args:
            print("❌ Module name is required")
            print("Usage: create-module-agent <module_name> [options]")
            return 1
        
        # Handle help request
        if args[0] in ['--help', '-h', 'help']:
            print("""
Create Module Agent Command

Usage:
  create-module-agent <module_name> [options]

Arguments:
  module_name                    Name of the module agent to create

Options:
  --type <type>                  Agent type (default: general-purpose)
  --repos <repo1,repo2>          Comma-separated list of repositories
  --context-cache <true|false>   Enable context caching (default: true)
  --templates <template1,template2>  Comma-separated list of templates

Examples:
  create-module-agent finance-analytics
  create-module-agent devops --type infrastructure
  create-module-agent api --repos assetutilities,pyproject-starter
            """)
            return 0
        
        # Build command arguments for CreateModuleAgentCommand
        command_args = ['/create-module-agent'] + args
        
        # Execute command with progress indication
        try:
            self.progress.start("Creating module agent")
            
            command = CreateModuleAgentCommand()
            result = command.execute(command_args)
            
            self.progress.stop()
            
            if result.success:
                print(f"✅ {result.message}")
                return 0
            else:
                print(f"❌ {result.message}")
                return 1
                
        except Exception as e:
            self.progress.stop()
            print(f"❌ Error executing command: {e}")
            return 1
    
    def run(self, args: Optional[List[str]] = None) -> int:
        """Run CLI with given arguments.
        
        Args:
            args: Command line arguments (defaults to sys.argv)
            
        Returns:
            Exit code
        """
        if args is None:
            args = sys.argv[1:]
        
        parser = self.create_parser()
        
        # Handle no arguments - show help
        if not args:
            parser.print_help()
            return 0
        
        # Parse arguments
        try:
            parsed_args = parser.parse_args(args)
        except SystemExit as e:
            return e.code if e.code is not None else 1
        
        # Handle interactive mode
        if parsed_args.interactive:
            return self.run_interactive()
        
        # Handle specific commands
        if parsed_args.command == 'create-module-agent':
            return self.execute_create_module_agent_from_args(parsed_args)
        else:
            parser.print_help()
            return 1
    
    def execute_create_module_agent_from_args(self, args: argparse.Namespace) -> int:
        """Execute create-module-agent from parsed arguments.
        
        Args:
            args: Parsed command arguments
            
        Returns:
            Exit code
        """
        # Build command arguments
        command_args = ['/create-module-agent', args.module_name]
        
        if args.type != 'general-purpose':
            command_args.extend(['--type', args.type])
        
        if args.repos:
            command_args.extend(['--repos', args.repos])
        
        if not args.context_cache:
            command_args.extend(['--context-cache', 'false'])
        
        if args.templates:
            command_args.extend(['--templates', args.templates])
        
        # Execute command
        try:
            self.progress.start("Creating module agent")
            
            command = CreateModuleAgentCommand(base_dir=args.output_dir)
            result = command.execute(command_args)
            
            self.progress.stop()
            
            if result.success:
                print(f"✅ {result.message}")
                return 0
            else:
                print(f"❌ {result.message}")
                return 1
                
        except Exception as e:
            self.progress.stop()
            print(f"❌ Error executing command: {e}")
            return 1


def main_cli() -> int:
    """Main CLI entry point.
    
    Returns:
        Exit code
    """
    cli = AgentOSCLI()
    return cli.run()


if __name__ == '__main__':
    sys.exit(main_cli())