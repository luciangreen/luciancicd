#!/usr/bin/env python3
"""
Lucian CI/CD v2 - Command Line Interface
=======================================

Enhanced command-line interface with additional utilities.
"""

import argparse
import sys
import json
from pathlib import Path
from luciancicd_v2 import LucianCICD


def cmd_status(cicd: LucianCICD):
    """Show status of the CI/CD system"""
    print("Lucian CI/CD v2 Status")
    print("=" * 50)
    
    # Repository information
    cicd.discover_repositories()
    print(f"Repositories discovered: {len(cicd.repositories)}")
    
    for name, repo in cicd.repositories.items():
        print(f"  • {name}")
        print(f"    Path: {repo.path}")
        print(f"    Main files: {len(repo.main_files)}")
        print(f"    Test commands: {len(repo.test_commands)}")
    
    # Dependency information
    cicd.build_dependency_graph()
    total_deps = sum(len(deps) for deps in cicd.dependency_graph.values())
    print(f"\nDependencies: {total_deps} total")
    
    for repo, deps in cicd.dependency_graph.items():
        if deps:
            print(f"  {repo} depends on: {', '.join(deps)}")
    
    # Change tracking information  
    data_path = Path(cicd.config["data_path"])
    tracker_file = data_path / "file_tracker.json"
    
    if tracker_file.exists():
        with open(tracker_file, 'r') as f:
            tracker_data = json.load(f)
        print(f"\nTracked files: {len(tracker_data)}")
    else:
        print(f"\nNo file tracking data found")
        
    # Recent logs
    log_path = Path(cicd.config["log_path"])
    if log_path.exists():
        log_files = list(log_path.glob("*.log"))
        result_files = list(log_path.glob("test_results_*.json"))
        print(f"\nLog files: {len(log_files)}")
        print(f"Test result files: {len(result_files)}")
        
        if result_files:
            latest_results = max(result_files, key=lambda x: x.stat().st_mtime)
            with open(latest_results, 'r') as f:
                results = json.load(f)
            print(f"\nLatest test run:")
            print(f"  Success: {results['success']}")
            print(f"  Repositories tested: {results['repositories_tested']}")
            print(f"  Total tests: {results['total_tests']}")
            print(f"  Duration: {results['duration']:.2f}s")


def cmd_clean(cicd: LucianCICD):
    """Clean up temporary files and data"""
    import shutil
    
    paths_to_clean = [
        Path(cicd.config["testing_path"]),
        Path(cicd.config["data_path"]) / "file_tracker.json"
    ]
    
    for path in paths_to_clean:
        try:
            if path.exists():
                if path.is_dir():
                    shutil.rmtree(path)
                    print(f"✓ Cleaned directory: {path}")
                else:
                    path.unlink()
                    print(f"✓ Cleaned file: {path}")
            else:
                print(f"• Path does not exist: {path}")
        except Exception as e:
            print(f"✗ Could not clean {path}: {e}")
            
    print("Clean up complete")


def cmd_validate_config(config_path: str):
    """Validate configuration file"""
    try:
        cicd = LucianCICD(config_path)
        print("✓ Configuration is valid")
        
        # Check paths
        for key, path_str in [
            ("repositories_path", cicd.config["repositories_path"]),
            ("output_path", cicd.config["output_path"]),
            ("testing_path", cicd.config["testing_path"]),
            ("data_path", cicd.config["data_path"]),
            ("log_path", cicd.config["log_path"])
        ]:
            path = Path(path_str)
            if path.exists():
                print(f"✓ {key}: {path} (exists)")
            else:
                print(f"⚠ {key}: {path} (does not exist)")
                
    except Exception as e:
        print(f"✗ Configuration error: {e}")
        return False
        
    return True


def cmd_analyze_repo(cicd: LucianCICD, repo_name: str):
    """Analyze a specific repository in detail"""
    cicd.discover_repositories()
    
    if repo_name not in cicd.repositories:
        print(f"✗ Repository '{repo_name}' not found")
        available = ', '.join(cicd.repositories.keys())
        print(f"Available repositories: {available}")
        return
        
    repo = cicd.repositories[repo_name]
    print(f"Repository Analysis: {repo_name}")
    print("=" * 50)
    
    print(f"Path: {repo.path}")
    print(f"Main files: {repo.main_files}")
    print(f"Test commands: {repo.test_commands}")
    
    # File analysis
    all_files = list(repo.path.rglob("*"))
    tracked_files = [f for f in all_files if f.is_file() and cicd._should_include_file(f)]
    
    print(f"\nFiles:")
    print(f"  Total files: {len([f for f in all_files if f.is_file()])}")
    print(f"  Tracked files: {len(tracked_files)}")
    
    # File types
    extensions = {}
    for file_path in tracked_files:
        ext = file_path.suffix or "(no extension)"
        extensions[ext] = extensions.get(ext, 0) + 1
        
    print(f"  File types: {dict(extensions)}")
    
    # Dependencies
    cicd.build_dependency_graph()
    deps = cicd.dependency_graph.get(repo_name, set())
    if deps:
        print(f"\nDependencies: {', '.join(deps)}")
    else:
        print(f"\nNo dependencies found")
        
    # Dependents (repositories that depend on this one)
    dependents = [r for r, d in cicd.dependency_graph.items() if repo_name in d]
    if dependents:
        print(f"Dependent repositories: {', '.join(dependents)}")
    else:
        print(f"No repositories depend on this one")


def main():
    """Enhanced command-line interface"""
    parser = argparse.ArgumentParser(
        description="Lucian CI/CD v2 - Enhanced Modern CI/CD System",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python3 luciancicd_cli.py --setup              # Initial setup
  python3 luciancicd_cli.py --run               # Run CI/CD cycle  
  python3 luciancicd_cli.py --scan-only         # Scan for changes only
  python3 luciancicd_cli.py --status            # Show system status
  python3 luciancicd_cli.py --clean             # Clean up temporary files
  python3 luciancicd_cli.py --analyze test_repo # Analyze specific repository
  python3 luciancicd_cli.py --validate-config   # Validate configuration
        """
    )
    
    parser.add_argument("--config", default="luciancicd_config.json",
                       help="Configuration file path")
    
    # Main operations
    parser.add_argument("--setup", action="store_true",
                       help="Set up initial CI/CD state")
    parser.add_argument("--run", action="store_true", 
                       help="Run CI/CD cycle")
    parser.add_argument("--scan-only", action="store_true",
                       help="Only scan for changes, don't run tests")
    
    # Utility operations
    parser.add_argument("--status", action="store_true",
                       help="Show system status")
    parser.add_argument("--clean", action="store_true",
                       help="Clean up temporary files and data")
    parser.add_argument("--validate-config", action="store_true",
                       help="Validate configuration file")
    parser.add_argument("--analyze", metavar="REPO",
                       help="Analyze specific repository in detail")
    
    args = parser.parse_args()
    
    # Handle utility operations that don't need full CI/CD setup
    if args.validate_config:
        success = cmd_validate_config(args.config)
        sys.exit(0 if success else 1)
        
    # Create CI/CD instance for other operations
    try:
        cicd = LucianCICD(args.config)
    except Exception as e:
        print(f"✗ Failed to initialize CI/CD system: {e}")
        sys.exit(1)
    
    if args.status:
        cmd_status(cicd)
        
    elif args.clean:
        cmd_clean(cicd)
        
    elif args.analyze:
        cmd_analyze_repo(cicd, args.analyze)
        
    elif args.setup:
        cicd.setup_initial_state()
        print("✓ Initial setup complete")
        
    elif args.scan_only:
        cicd.discover_repositories()
        changed_repos = cicd.scan_for_changes()
        if changed_repos:
            print(f"Changed repositories: {', '.join(changed_repos)}")
        else:
            print("No changes detected")
            
    elif args.run:
        success = cicd.run_ci_cd_cycle()
        sys.exit(0 if success else 1)
        
    else:
        parser.print_help()
        

if __name__ == "__main__":
    main()