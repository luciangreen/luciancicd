#!/usr/bin/env python3
"""
Lucian CI/CD v2 - Complete Rewrite
==================================

A modern, stable, and fast continuous integration and continuous deployment 
system rewritten from the ground up to replace the complex Prolog-based system.

Key improvements:
- Cross-platform compatibility
- Lower memory usage
- Better performance
- Simplified configuration
- Modern architecture
"""

import os
import sys
import json
import time
import hashlib
import logging
import argparse
import threading
from pathlib import Path
from typing import Dict, List, Set, Optional, Tuple, Any
from dataclasses import dataclass
from concurrent.futures import ThreadPoolExecutor, as_completed
import subprocess
import fnmatch
from datetime import datetime

# Import enhanced dependency management
from dependency_manager import DependencyResolver, AdvancedTestRunner


@dataclass
class Repository:
    """Represents a repository with its metadata"""
    name: str
    path: Path
    main_files: List[str]
    test_commands: List[str]
    dependencies: List[str] = None
    
    def __post_init__(self):
        if self.dependencies is None:
            self.dependencies = []


@dataclass  
class FileChangeInfo:
    """Information about a file change"""
    path: Path
    last_modified: float
    content_hash: str
    

class LucianCICD:
    """Main CI/CD engine - simplified and modernized"""
    
    def __init__(self, config_path: str = "luciancicd_config.json"):
        self.config_path = Path(config_path)
        self.config = self._load_config()
        self.logger = self._setup_logging()
        self.repositories: Dict[str, Repository] = {}
        self.file_tracker: Dict[str, FileChangeInfo] = {}
        self.dependency_graph: Dict[str, Set[str]] = {}
        
        # Initialize enhanced dependency management
        self.dependency_resolver = DependencyResolver(self.config)
        self.advanced_test_runner = AdvancedTestRunner(self.config, self.dependency_resolver)
        
    def _load_config(self) -> Dict[str, Any]:
        """Load configuration with sensible defaults"""
        default_config = {
            "repositories_path": "../../GitHub2/",
            "output_path": "../../GitHub2o/", 
            "testing_path": "../luciancicd-testing/",
            "data_path": "../luciancicd-data/",
            "log_path": "../lc_logs/",
            "omit_paths": ["private2"],
            "max_parallel_tests": 4,
            "test_timeout": 60,
            "max_changes_threshold": 20,
            "file_extensions": [".pl", ".py", ".txt", ".md"],
            "user": "luciangreen"
        }
        
        if self.config_path.exists():
            try:
                with open(self.config_path, 'r') as f:
                    user_config = json.load(f)
                    default_config.update(user_config)
            except Exception as e:
                print(f"Warning: Could not load config file {self.config_path}: {e}")
                print("Using default configuration")
        else:
            # Create default config file
            with open(self.config_path, 'w') as f:
                json.dump(default_config, f, indent=2)
            print(f"Created default configuration file: {self.config_path}")
            
        return default_config
        
    def _setup_logging(self) -> logging.Logger:
        """Set up comprehensive logging"""
        log_dir = Path(self.config["log_path"])
        log_dir.mkdir(parents=True, exist_ok=True)
        
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        log_file = log_dir / f"luciancicd_v2_{timestamp}.log"
        
        logger = logging.getLogger('luciancicd_v2')
        logger.setLevel(logging.INFO)
        
        # File handler
        file_handler = logging.FileHandler(log_file)
        file_handler.setLevel(logging.DEBUG)
        
        # Console handler  
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.DEBUG)  # Enable debug output
        
        # Formatter
        formatter = logging.Formatter(
            '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
        )
        file_handler.setFormatter(formatter)
        console_handler.setFormatter(formatter)
        
        logger.addHandler(file_handler)
        logger.addHandler(console_handler)
        
        return logger
        
    def _get_file_hash(self, file_path: Path) -> str:
        """Get content hash of a file for change detection"""
        try:
            with open(file_path, 'rb') as f:
                content = f.read()
                return hashlib.sha256(content).hexdigest()
        except Exception:
            return ""
            
    def _should_include_file(self, file_path: Path) -> bool:
        """Check if file should be included in tracking"""
        # Skip hidden files and directories (but not relative path parts like "..")
        if any(part.startswith('.') and part not in ['.', '..'] for part in file_path.parts):
            return False
            
        # Check omit paths
        str_path = str(file_path)
        for omit_path in self.config["omit_paths"]:
            if omit_path in str_path:
                return False
                
        # Check file extensions
        if file_path.suffix and file_path.suffix in self.config["file_extensions"]:
            return True
            
        # Include main_file.txt and other configuration files
        if file_path.name in ["main_file.txt", "lppm_registry.txt"]:
            return True
            
        return False
        
    def discover_repositories(self) -> None:
        """Discover repositories and their configurations"""
        self.logger.info("Discovering repositories...")
        
        repo_base_path = Path(self.config["repositories_path"])
        if not repo_base_path.exists():
            self.logger.error(f"Repository path does not exist: {repo_base_path}")
            return
            
        for item in repo_base_path.iterdir():
            if item.is_dir() and not item.name.startswith('.'):
                try:
                    repo = self._load_repository_config(item)
                    if repo:
                        self.repositories[repo.name] = repo
                        self.logger.info(f"Discovered repository: {repo.name}")
                except Exception as e:
                    self.logger.warning(f"Could not load repository {item.name}: {e}")
                    
        self.logger.info(f"Discovered {len(self.repositories)} repositories")
        
    def _load_repository_config(self, repo_path: Path) -> Optional[Repository]:
        """Load repository configuration from main_file.txt"""
        main_file_path = repo_path / "main_file.txt"
        
        if not main_file_path.exists():
            # Create empty repository entry for directories without main_file.txt
            return Repository(
                name=repo_path.name,
                path=repo_path,
                main_files=[],
                test_commands=[]
            )
            
        try:
            # Parse the main_file.txt format from the original system
            with open(main_file_path, 'r') as f:
                content = f.read().strip()
                
            if not content or content == "[]":
                return Repository(
                    name=repo_path.name,
                    path=repo_path,
                    main_files=[],
                    test_commands=[]
                )
                
            # Simple parsing - expect JSON-like format
            # [[file.pl, [[pred,arity], ...]], ...]
            import ast
            try:
                config_data = ast.literal_eval(content)
                main_files = []
                test_commands = []
                
                for file_config in config_data:
                    if isinstance(file_config, list) and len(file_config) >= 1:
                        filename = file_config[0]
                        main_files.append(filename)
                        
                        # Generate basic test commands
                        if len(file_config) > 1 and isinstance(file_config[1], list):
                            for pred_info in file_config[1]:
                                if isinstance(pred_info, list) and len(pred_info) >= 2:
                                    pred_name, arity = pred_info[0], pred_info[1]
                                    test_cmd = f"{pred_name}({', '.join(['_'] * arity)})"
                                    test_commands.append(test_cmd)
                                    
                return Repository(
                    name=repo_path.name,
                    path=repo_path,
                    main_files=main_files,
                    test_commands=test_commands
                )
                
            except (ValueError, SyntaxError):
                self.logger.warning(f"Could not parse main_file.txt for {repo_path.name}")
                return Repository(
                    name=repo_path.name,
                    path=repo_path,
                    main_files=[],
                    test_commands=[]
                )
                
        except Exception as e:
            self.logger.warning(f"Error reading main_file.txt for {repo_path.name}: {e}")
            return None
            
    def scan_for_changes(self) -> List[str]:
        """Scan for file changes and return list of changed repositories"""
        self.logger.info("Scanning for file changes...")
        
        data_path = Path(self.config["data_path"])
        data_path.mkdir(parents=True, exist_ok=True)
        
        tracker_file = data_path / "file_tracker.json"
        
        # Load previous tracking data
        if tracker_file.exists():
            try:
                with open(tracker_file, 'r') as f:
                    old_tracker_data = json.load(f)
                    self.file_tracker = {
                        path: FileChangeInfo(
                            path=Path(path),
                            last_modified=info["last_modified"],
                            content_hash=info["content_hash"]
                        )
                        for path, info in old_tracker_data.items()
                    }
            except Exception as e:
                self.logger.warning(f"Could not load previous tracker data: {e}")
                self.file_tracker = {}
        
        changed_repos = set()
        current_tracker = {}
        
        # Scan all repositories for changes
        for repo_name, repo in self.repositories.items():
            repo_changed = False
            
            for file_path in repo.path.rglob("*"):
                if file_path.is_file() and self._should_include_file(file_path):
                    str_path = str(file_path)
                    
                    try:
                        current_mtime = file_path.stat().st_mtime
                        current_hash = self._get_file_hash(file_path)
                        
                        current_info = FileChangeInfo(
                            path=file_path,
                            last_modified=current_mtime,
                            content_hash=current_hash
                        )
                        
                        current_tracker[str_path] = {
                            "last_modified": current_mtime,
                            "content_hash": current_hash
                        }
                        
                        # Check if file changed
                        if str_path not in self.file_tracker:
                            self.logger.info(f"New file detected: {file_path}")
                            repo_changed = True
                        else:
                            old_info = self.file_tracker[str_path]
                            if (current_hash != old_info.content_hash or 
                                current_mtime != old_info.last_modified):
                                self.logger.info(f"File changed: {file_path}")
                                repo_changed = True
                                
                    except Exception as e:
                        self.logger.warning(f"Could not check file {file_path}: {e}")
                        
            if repo_changed:
                changed_repos.add(repo_name)
                
        # Save updated tracking data
        with open(tracker_file, 'w') as f:
            json.dump(current_tracker, f, indent=2)
            
        changed_list = list(changed_repos)
        self.logger.info(f"Changed repositories: {changed_list}")
        
        return changed_list
        
    def build_dependency_graph(self) -> None:
        """Build dependency graph between repositories"""
        self.logger.info("Building dependency graph...")
        
        # Load LPPM registry if available
        self.dependency_resolver.load_lppm_registry()
        
        # Build comprehensive dependency graph
        self.dependency_graph = self.dependency_resolver.build_dependency_graph(self.repositories)
        
        self.logger.info("Enhanced dependency graph built")
        
    def get_affected_repositories(self, changed_repos: List[str]) -> List[str]:
        """Get all repositories affected by changes (including dependents)"""
        return self.dependency_resolver.get_affected_repositories(changed_repos, self.repositories)
        
    def run_tests_for_repository(self, repo_name: str) -> Dict[str, Any]:
        """Run tests for a specific repository"""
        repo = self.repositories[repo_name]
        self.logger.info(f"Running tests for repository: {repo_name}")
        
        results = {
            "repository": repo_name,
            "success": True,
            "tests_run": 0,
            "tests_passed": 0,
            "tests_failed": 0,
            "errors": [],
            "duration": 0.0
        }
        
        start_time = time.time()
        
        try:
            # Create testing directory
            test_path = Path(self.config["testing_path"]) / repo_name
            test_path.mkdir(parents=True, exist_ok=True)
            
            # Copy repository files to testing directory
            import shutil
            for file_path in repo.path.rglob("*"):
                if file_path.is_file() and self._should_include_file(file_path):
                    rel_path = file_path.relative_to(repo.path)
                    dest_path = test_path / rel_path
                    dest_path.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copy2(file_path, dest_path)
                    
            # Run test commands
            if repo.test_commands:
                for test_cmd in repo.test_commands:
                    results["tests_run"] += 1
                    
                    try:
                        # Create a simple test script
                        test_script = test_path / "test_runner.pl"
                        with open(test_script, 'w') as f:
                            f.write(f"""
% Auto-generated test script
:- set_prolog_flag(stack_limit, 1000000000).

main :-
    catch(({test_cmd}), Error, (write('Test failed: '), write(Error), nl, halt(1))),
    write('Test passed: {test_cmd}'), nl,
    halt(0).

main :- 
    write('Test setup failed'), nl,
    halt(1).

% Load the repository files
""")
                            # Add include statements for all .pl files
                            for pl_file in test_path.rglob("*.pl"):
                                if pl_file != test_script:
                                    rel_path = pl_file.relative_to(test_path)
                                    f.write(f":- include('{rel_path}').\n")
                                    
                        # Make sure the script exists before running
                        if not test_script.exists():
                            raise Exception(f"Test script was not created: {test_script}")
                        
                        # Debug: Print the command being executed
                        abs_test_script = test_script.resolve()
                        self.logger.debug(f"Running: swipl -g main -t halt {abs_test_script} in {test_path}")
                        
                        # Run the test with timeout
                        result = subprocess.run([
                            'swipl', '-g', 'main', '-t', 'halt', str(abs_test_script)
                        ], 
                        capture_output=True, 
                        text=True,
                        timeout=self.config["test_timeout"],
                        cwd=test_path
                        )
                        
                        if result.returncode == 0:
                            results["tests_passed"] += 1
                            self.logger.debug(f"Test passed: {test_cmd}")
                        else:
                            results["tests_failed"] += 1
                            results["success"] = False
                            error_msg = f"Test failed: {test_cmd} - {result.stderr}"
                            results["errors"].append(error_msg)
                            self.logger.warning(error_msg)
                            
                    except subprocess.TimeoutExpired:
                        results["tests_failed"] += 1
                        results["success"] = False
                        error_msg = f"Test timeout: {test_cmd}"
                        results["errors"].append(error_msg)
                        self.logger.warning(error_msg)
                        
                    except Exception as e:
                        results["tests_failed"] += 1
                        results["success"] = False
                        error_msg = f"Test error: {test_cmd} - {str(e)}"
                        results["errors"].append(error_msg)
                        self.logger.warning(error_msg)
            else:
                self.logger.info(f"No test commands defined for {repo_name}")
                
        except Exception as e:
            results["success"] = False
            error_msg = f"Repository test setup error: {str(e)}"
            results["errors"].append(error_msg)
            self.logger.error(error_msg)
            
        results["duration"] = time.time() - start_time
        return results
        
    def run_tests_for_repository_enhanced(self, repo_name: str) -> Dict[str, Any]:
        """Run enhanced tests for a specific repository with dependency support"""
        repo = self.repositories[repo_name]
        self.logger.info(f"Running enhanced tests for repository: {repo_name}")
        
        results = {
            "repository": repo_name,
            "success": True,
            "tests_run": 0,
            "tests_passed": 0,
            "tests_failed": 0,
            "errors": [],
            "duration": 0.0,
            "dependencies": list(self.dependency_resolver.get_all_dependencies(repo_name))
        }
        
        start_time = time.time()
        
        try:
            # Create comprehensive test environment with dependencies
            test_path = self.advanced_test_runner.create_test_environment(repo_name, self.repositories)
            
            # Run test commands if available
            if repo.test_commands:
                # Use comprehensive test script
                test_script = self.advanced_test_runner.generate_comprehensive_test(repo_name, repo, test_path)
                
                results["tests_run"] = 1  # One comprehensive test
                
                try:
                    # Run the comprehensive test
                    abs_test_script = test_script.resolve()
                    self.logger.debug(f"Running comprehensive test: {abs_test_script}")
                    
                    result = subprocess.run([
                        'swipl', '-g', 'main', '-t', 'halt', str(abs_test_script)
                    ], 
                    capture_output=True, 
                    text=True,
                    timeout=self.config["test_timeout"] * 2,  # Longer timeout for comprehensive tests
                    cwd=test_path
                    )
                    
                    if result.returncode == 0:
                        results["tests_passed"] = 1
                        self.logger.debug(f"Comprehensive test passed for {repo_name}")
                        self.logger.debug(f"Test output: {result.stdout}")
                    else:
                        results["tests_failed"] = 1
                        results["success"] = False
                        error_msg = f"Comprehensive test failed for {repo_name}: {result.stderr}"
                        results["errors"].append(error_msg)
                        self.logger.warning(error_msg)
                        
                except subprocess.TimeoutExpired:
                    results["tests_failed"] = 1
                    results["success"] = False
                    error_msg = f"Comprehensive test timeout for {repo_name}"
                    results["errors"].append(error_msg)
                    self.logger.warning(error_msg)
                    
                except Exception as e:
                    results["tests_failed"] = 1
                    results["success"] = False
                    error_msg = f"Comprehensive test error for {repo_name}: {str(e)}"
                    results["errors"].append(error_msg)
                    self.logger.warning(error_msg)
            else:
                # Fall back to basic test for repositories without test commands
                results = self.run_tests_for_repository(repo_name)
                
        except Exception as e:
            results["success"] = False
            error_msg = f"Enhanced test setup error for {repo_name}: {str(e)}"
            results["errors"].append(error_msg)
            self.logger.error(error_msg)
            
        results["duration"] = time.time() - start_time
        return results
        
    def run_tests(self, repositories: List[str]) -> Dict[str, Any]:
        """Run tests for multiple repositories in parallel"""
        self.logger.info(f"Running tests for {len(repositories)} repositories")
        
        # Get optimal test order (dependencies first)
        test_order = self.dependency_resolver.get_test_order(repositories)
        self.logger.info(f"Test order: {' -> '.join(test_order)}")
        
        overall_results = {
            "success": True,
            "repositories_tested": len(repositories),
            "repositories_passed": 0,
            "repositories_failed": 0,
            "total_tests": 0,
            "total_passed": 0,
            "total_failed": 0,
            "duration": 0.0,
            "test_order": test_order,
            "results": {}
        }
        
        start_time = time.time()
        
        # Run tests in parallel (respecting dependency order when possible)
        max_workers = min(self.config["max_parallel_tests"], len(repositories))
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            future_to_repo = {
                executor.submit(self.run_tests_for_repository_enhanced, repo_name): repo_name
                for repo_name in test_order
            }
            
            for future in as_completed(future_to_repo):
                repo_name = future_to_repo[future]
                try:
                    result = future.result()
                    overall_results["results"][repo_name] = result
                    
                    # Update overall stats
                    overall_results["total_tests"] += result["tests_run"]
                    overall_results["total_passed"] += result["tests_passed"]
                    overall_results["total_failed"] += result["tests_failed"]
                    
                    if result["success"]:
                        overall_results["repositories_passed"] += 1
                        self.logger.info(f"✓ Tests passed for {repo_name}")
                    else:
                        overall_results["repositories_failed"] += 1
                        overall_results["success"] = False
                        self.logger.error(f"✗ Tests failed for {repo_name}")
                        
                except Exception as e:
                    self.logger.error(f"Error testing repository {repo_name}: {e}")
                    overall_results["repositories_failed"] += 1
                    overall_results["success"] = False
                    
        overall_results["duration"] = time.time() - start_time
        
        # Save results
        self._save_test_results(overall_results)
        
        return overall_results
        
    def _save_test_results(self, results: Dict[str, Any]) -> None:
        """Save test results to file"""
        log_path = Path(self.config["log_path"])
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        results_file = log_path / f"test_results_{timestamp}.json"
        
        with open(results_file, 'w') as f:
            json.dump(results, f, indent=2, default=str)
            
        self.logger.info(f"Test results saved to: {results_file}")
        
    def setup_initial_state(self) -> None:
        """Set up initial state for CI/CD tracking"""
        self.logger.info("Setting up initial CI/CD state...")
        
        # Discover repositories
        self.discover_repositories()
        
        # Build dependency graph
        self.build_dependency_graph()
        
        # Initialize file tracking (without reporting changes)
        self.scan_for_changes()
        
        self.logger.info("Initial setup complete")
        
    def run_ci_cd_cycle(self) -> bool:
        """Run a complete CI/CD cycle"""
        self.logger.info("Starting CI/CD cycle...")
        
        try:
            # Discover repositories
            self.discover_repositories()
            
            # Build dependency graph 
            self.build_dependency_graph()
            
            # Scan for changes
            changed_repos = self.scan_for_changes()
            
            if not changed_repos:
                self.logger.info("No changes detected - CI/CD cycle complete")
                return True
                
            # Get all affected repositories
            affected_repos = self.get_affected_repositories(changed_repos)
            
            # Perform change impact analysis
            changed_file_paths = []
            for repo_name in changed_repos:
                repo = self.repositories[repo_name]
                for file_path in repo.path.rglob("*"):
                    if file_path.is_file() and self._should_include_file(file_path):
                        str_path = str(file_path)
                        if str_path in self.file_tracker:
                            old_info = self.file_tracker[str_path]
                            current_hash = self._get_file_hash(file_path)
                            if current_hash != old_info.content_hash:
                                changed_file_paths.append(file_path)
                                
            impact_analysis = self.dependency_resolver.analyze_change_impact(changed_file_paths, self.repositories)
            
            self.logger.info(f"Change impact analysis:")
            self.logger.info(f"  Risk level: {impact_analysis['risk_level']}")
            self.logger.info(f"  Changed repositories: {impact_analysis['changed_repositories']}")
            self.logger.info(f"  Affected repositories: {impact_analysis['affected_repositories']}")
            if impact_analysis['recommendations']:
                self.logger.info(f"  Recommendations: {'; '.join(impact_analysis['recommendations'])}")
            
            self.logger.info(f"Running tests for affected repositories: {affected_repos}")
            
            # Run tests
            results = self.run_tests(affected_repos)
            
            # Report results
            if results["success"]:
                self.logger.info("✓ All tests passed - CI/CD cycle successful")
                return True
            else:
                self.logger.error("✗ Some tests failed - CI/CD cycle failed")
                return False
                
        except Exception as e:
            self.logger.error(f"CI/CD cycle failed with error: {e}")
            return False


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="Lucian CI/CD v2 - Modern CI/CD System")
    parser.add_argument("--config", default="luciancicd_config.json",
                       help="Configuration file path")
    parser.add_argument("--setup", action="store_true",
                       help="Set up initial CI/CD state")
    parser.add_argument("--run", action="store_true", 
                       help="Run CI/CD cycle")
    parser.add_argument("--scan-only", action="store_true",
                       help="Only scan for changes, don't run tests")
    
    args = parser.parse_args()
    
    # Create CI/CD instance
    cicd = LucianCICD(args.config)
    
    if args.setup:
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