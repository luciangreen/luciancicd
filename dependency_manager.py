#!/usr/bin/env python3
"""
Enhanced Dependency Management for Lucian CI/CD v2
=================================================

This module handles dependency resolution using LPPM registry
and provides advanced dependency tracking capabilities.
"""

import json
import ast
from pathlib import Path
from typing import Dict, List, Set, Optional, Tuple
import logging


class DependencyResolver:
    """Advanced dependency resolution system"""
    
    def __init__(self, config: dict):
        self.config = config
        self.logger = logging.getLogger('luciancicd_v2.dependency')
        self.registry: Dict[str, dict] = {}
        self.dependency_graph: Dict[str, Set[str]] = {}
        
    def load_lppm_registry(self) -> bool:
        """Load LPPM registry for dependency information"""
        registry_path = Path("../List-Prolog-Package-Manager/lppm_registry.txt")
        
        if not registry_path.exists():
            self.logger.warning(f"LPPM registry not found at {registry_path}")
            return False
            
        try:
            with open(registry_path, 'r') as f:
                content = f.read().strip()
                
            # Parse the Prolog-style registry format
            # Expected format: [[User,Repository,Description,Dependencies], ...]
            registry_data = ast.literal_eval(content)
            
            for entry in registry_data:
                if isinstance(entry, list) and len(entry) >= 4:
                    user, repo, description, deps = entry[0], entry[1], entry[2], entry[3]
                    
                    self.registry[repo] = {
                        'user': user,
                        'description': description,
                        'dependencies': deps
                    }
                    
            self.logger.info(f"Loaded {len(self.registry)} entries from LPPM registry")
            return True
            
        except Exception as e:
            self.logger.error(f"Failed to load LPPM registry: {e}")
            return False
            
    def build_dependency_graph(self, repositories: Dict[str, any]) -> Dict[str, Set[str]]:
        """Build comprehensive dependency graph"""
        self.dependency_graph = {}
        
        # Initialize graph
        for repo_name in repositories:
            self.dependency_graph[repo_name] = set()
            
        # Add dependencies from LPPM registry
        for repo_name in repositories:
            if repo_name in self.registry:
                deps = self.registry[repo_name].get('dependencies', [])
                for dep in deps:
                    if isinstance(dep, list) and len(dep) >= 2:
                        dep_user, dep_repo = dep[0], dep[1]
                        if dep_repo in repositories:
                            self.dependency_graph[repo_name].add(dep_repo)
                            
        # Add implicit dependencies based on file includes
        self._detect_implicit_dependencies(repositories)
        
        self.logger.info(f"Built dependency graph with {len(self.dependency_graph)} nodes")
        return self.dependency_graph
        
    def _detect_implicit_dependencies(self, repositories: Dict[str, any]) -> None:
        """Detect implicit dependencies from file includes"""
        for repo_name, repo in repositories.items():
            try:
                # Scan Prolog files for include statements
                for file_path in repo.path.rglob("*.pl"):
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                        
                    # Look for include statements
                    import re
                    include_patterns = [
                        r":- include\(['\"]([^'\"]+)['\"]",
                        r":- include\('([^']+)'\)",
                        r':-include\("([^"]+)"\)'
                    ]
                    
                    for pattern in include_patterns:
                        matches = re.findall(pattern, content)
                        for match in matches:
                            # Try to determine which repository this include belongs to
                            include_path = match
                            potential_dep = self._resolve_include_to_repo(include_path, repositories)
                            if potential_dep and potential_dep != repo_name:
                                self.dependency_graph[repo_name].add(potential_dep)
                                
            except Exception as e:
                self.logger.debug(f"Could not scan {repo_name} for implicit dependencies: {e}")
                
    def _resolve_include_to_repo(self, include_path: str, repositories: Dict[str, any]) -> Optional[str]:
        """Try to resolve an include path to a repository name"""
        # Simple heuristic: look for repository names in the path
        include_lower = include_path.lower()
        
        for repo_name in repositories:
            if repo_name.lower() in include_lower:
                return repo_name
                
        # Look for common patterns
        if 'listprolog' in include_lower:
            return 'listprologinterpreter'
        elif 'lppm' in include_lower:
            return 'List-Prolog-Package-Manager'
            
        return None
        
    def get_all_dependencies(self, repo_name: str) -> Set[str]:
        """Get all dependencies (direct and transitive) for a repository"""
        visited = set()
        dependencies = set()
        
        def _collect_deps(current_repo):
            if current_repo in visited:
                return
            visited.add(current_repo)
            
            for dep in self.dependency_graph.get(current_repo, set()):
                if dep not in dependencies:
                    dependencies.add(dep)
                    _collect_deps(dep)
                    
        _collect_deps(repo_name)
        return dependencies
        
    def get_affected_repositories(self, changed_repos: List[str], all_repos: Dict[str, any]) -> List[str]:
        """Get all repositories affected by changes (including dependents)"""
        affected = set(changed_repos)
        
        # Find repositories that depend on changed ones
        for repo_name in all_repos:
            repo_deps = self.get_all_dependencies(repo_name)
            if repo_deps.intersection(set(changed_repos)):
                affected.add(repo_name)
                
        return list(affected)
        
    def get_test_order(self, repositories: List[str]) -> List[str]:
        """Get optimal order for testing repositories (dependencies first)"""
        # Topological sort of dependency graph
        in_degree = {repo: 0 for repo in repositories}
        
        # Calculate in-degrees
        for repo in repositories:
            for dep in self.dependency_graph.get(repo, set()):
                if dep in in_degree:
                    in_degree[repo] += 1
                    
        # Kahn's algorithm for topological sort
        queue = [repo for repo in repositories if in_degree[repo] == 0]
        result = []
        
        while queue:
            current = queue.pop(0)
            result.append(current)
            
            # Update in-degrees of dependents
            for repo in repositories:
                if current in self.dependency_graph.get(repo, set()):
                    in_degree[repo] -= 1
                    if in_degree[repo] == 0:
                        queue.append(repo)
                        
        # Add any remaining repositories (circular dependencies)
        remaining = set(repositories) - set(result)
        result.extend(list(remaining))
        
        return result
        
    def analyze_change_impact(self, changed_files: List[Path], all_repos: Dict[str, any]) -> Dict[str, any]:
        """Analyze the impact of file changes"""
        impact_analysis = {
            'changed_repositories': [],
            'affected_repositories': [],
            'risk_level': 'low',
            'recommendations': []
        }
        
        # Determine which repositories contain changed files
        changed_repos = []
        for file_path in changed_files:
            for repo_name, repo in all_repos.items():
                try:
                    file_path.relative_to(repo.path)
                    if repo_name not in changed_repos:
                        changed_repos.append(repo_name)
                    break
                except ValueError:
                    continue
                    
        impact_analysis['changed_repositories'] = changed_repos
        
        # Get affected repositories
        affected_repos = self.get_affected_repositories(changed_repos, all_repos)
        impact_analysis['affected_repositories'] = affected_repos
        
        # Assess risk level
        if len(affected_repos) > len(changed_repos) * 3:
            impact_analysis['risk_level'] = 'high'
            impact_analysis['recommendations'].append(
                "High impact change detected - consider running full test suite"
            )
        elif len(affected_repos) > len(changed_repos):
            impact_analysis['risk_level'] = 'medium'
            impact_analysis['recommendations'].append(
                "Medium impact change - review dependent repositories carefully"
            )
            
        # Add specific recommendations
        if len(changed_repos) > 5:
            impact_analysis['recommendations'].append(
                "Large change detected - consider breaking into smaller commits"
            )
            
        return impact_analysis


class AdvancedTestRunner:
    """Enhanced test runner with dependency-aware features"""
    
    def __init__(self, config: dict, dependency_resolver: DependencyResolver):
        self.config = config
        self.dependency_resolver = dependency_resolver
        self.logger = logging.getLogger('luciancicd_v2.test_runner')
        
    def create_test_environment(self, repo_name: str, repositories: Dict[str, any]) -> Path:
        """Create isolated test environment with dependencies"""
        test_path = Path(self.config["testing_path"]) / repo_name
        test_path.mkdir(parents=True, exist_ok=True)
        
        # Clear existing environment
        import shutil
        if test_path.exists():
            shutil.rmtree(test_path)
        test_path.mkdir(parents=True, exist_ok=True)
        
        # Copy main repository
        repo = repositories[repo_name]
        self._copy_repository_files(repo.path, test_path, repo_name)
        
        # Copy dependencies
        dependencies = self.dependency_resolver.get_all_dependencies(repo_name)
        for dep_name in dependencies:
            if dep_name in repositories:
                dep_repo = repositories[dep_name]
                dep_path = test_path / dep_name
                self._copy_repository_files(dep_repo.path, dep_path, dep_name)
                
        return test_path
        
    def _copy_repository_files(self, source_path: Path, dest_path: Path, repo_name: str) -> None:
        """Copy repository files to test environment"""
        import shutil
        
        for file_path in source_path.rglob("*"):
            if file_path.is_file():
                # Check if file should be included
                rel_path = file_path.relative_to(source_path)
                dest_file = dest_path / rel_path
                
                # Create destination directory
                dest_file.parent.mkdir(parents=True, exist_ok=True)
                
                # Copy file
                shutil.copy2(file_path, dest_file)
                
    def generate_comprehensive_test(self, repo_name: str, repo: any, test_path: Path) -> Path:
        """Generate comprehensive test script with dependency loading"""
        test_script = test_path / "comprehensive_test.pl"
        
        with open(test_script, 'w') as f:
            f.write("""
% Comprehensive test script generated by Lucian CI/CD v2
:- set_prolog_flag(stack_limit, 1000000000).

% Initialize test environment
test_init :-
    write('Initializing test environment for """ + repo_name + """'), nl.

% Load dependencies
load_dependencies :-
    write('Loading dependencies...'), nl.

""")
            
            # Add dependency includes
            dependencies = self.dependency_resolver.get_all_dependencies(repo_name)
            for dep in dependencies:
                dep_path = test_path / dep
                if dep_path.exists():
                    for pl_file in dep_path.rglob("*.pl"):
                        rel_path = pl_file.relative_to(test_path)
                        f.write(f":- catch(consult('{rel_path}'), _, true).\n")
                        
            # Add main repository includes
            for pl_file in test_path.rglob("*.pl"):
                if pl_file != test_script and pl_file.name != "comprehensive_test.pl":
                    try:
                        rel_path = pl_file.relative_to(test_path)
                        # Skip dependency files (already included above)
                        skip = False
                        for dep in dependencies:
                            if str(rel_path).startswith(dep + "/"):
                                skip = True
                                break
                        if not skip:
                            f.write(f":- catch(consult('{rel_path}'), _, true).\n")
                    except ValueError:
                        continue
                        
            # Add test execution logic
            f.write(f"""

% Main test execution
main :-
    test_init,
    load_dependencies,
    run_tests,
    halt(0).

% Alternative main clause for error handling  
main :- 
    write('Test execution failed'), nl,
    halt(1).

% Run all tests for {repo_name}
run_tests :-
    write('Running tests for {repo_name}'), nl,
    test_predicates,
    write('All tests completed successfully'), nl.

% Test individual predicates
test_predicates :-
""")
            
            # Add individual predicate tests
            for test_cmd in repo.test_commands:
                f.write(f"""    catch(({test_cmd}), Error, (
        write('Test failed for {test_cmd}: '), write(Error), nl, halt(1)
    )),
    write('✓ Test passed: {test_cmd}'), nl,
""")
                
            f.write("""    true.
""")
        
        return test_script