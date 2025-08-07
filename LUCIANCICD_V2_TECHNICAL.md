# Lucian CI/CD v2 - Technical Reference

## System Architecture

Lucian CI/CD v2 is implemented as a pure Prolog system with the following architectural components:

### Core Modules

```prolog
luciancicd_v2.pl              % Main CI/CD engine (650+ lines)
luciancicd_v2_settings.pl     % Configuration management  
main_v2.pl                    % Entry points
```

### Key Data Structures

#### Repository Representation
```prolog
repo(Name, Path, MainFiles, Dependencies)
```
- `Name`: Atom representing repository name
- `Path`: Full path to repository directory
- `MainFiles`: List of files containing testable predicates
- `Dependencies`: List of dependent repository names

#### File Change Information
```prolog
file_info(RelativePath, ContentHash)
```
- `RelativePath`: Path relative to repository root
- `ContentHash`: Content-based hash for change detection

#### Version Information
```prolog
version(Files, Metadata, Timestamp)
```
- `Files`: List of file_info/2 structures
- `Metadata`: Repository metadata
- `Timestamp`: Version creation timestamp

#### Change Records
```prolog
added(File, Hash)         % New file
deleted(File, Hash)       % Removed file  
modified(File, OldHash, NewHash)  % Changed file
```

## Core Algorithms

### Repository Discovery Algorithm
```prolog
discover_repositories_v2(Repos) :-
    repositories_paths1([RepoPath]),
    findall(repo(Name, Path, MainFiles, Dependencies), 
            discover_single_repository(RepoPath, Name, Path, MainFiles, Dependencies), 
            Repos).
```

**Process:**
1. Get base repository path from configuration
2. Expand directory pattern to find all subdirectories
3. Filter out directories that should be omitted
4. Load main_file.txt configuration for each repository
5. Parse repository dependencies (simplified in v2)
6. Return structured list of repository objects

### Snapshot Creation Algorithm
```prolog
save_repository_snapshots(Repos) :-
    get_time(Timestamp),
    format_time(atom(TimeStr), '%Y%m%d_%H%M%S', Timestamp),
    atom_concat('../../luciancicd-snapshots/', TimeStr, SnapshotDir),
    make_directory_path(SnapshotDir),
    forall(member(Repo, Repos), save_single_repository_snapshot(Repo, SnapshotDir)),
    save_snapshot_metadata(SnapshotDir, Repos, Timestamp).
```

**Process:**
1. Generate timestamp-based snapshot directory
2. Create directory structure recursively
3. For each repository:
   - Copy all relevant files (recursive directory traversal)
   - Generate content hashes for change detection
   - Save repository metadata (main files, dependencies, timestamp)
4. Save snapshot-level metadata

### Change Detection Algorithm
```prolog
detect_changes_v2(Repos, ChangedRepos) :-
    get_previous_snapshot_dir(PreviousSnapshotDir),
    compare_with_previous_snapshot(Repos, PreviousSnapshotDir, ChangedRepos).
```

**Process:**
1. Find most recent snapshot directory (sorted by name/timestamp)
2. For each current repository:
   - Compare with corresponding snapshot repository
   - Use `has_file_changes/2` for file-by-file comparison
   - Hash-based content comparison for efficiency
3. Return list of repositories with detected changes

### Version Combination Analysis
```prolog
find_combinations_between_versions(ChangedRepos, Combinations) :-
    get_previous_snapshot_dir(PreviousSnapshotDir),
    findall(combination(Repo, OldVersion, NewVersion, Changes),
            find_repo_combination(Repo, PreviousSnapshotDir, OldVersion, NewVersion, Changes)),
            Combinations).
```

**Process:**
1. For each changed repository:
   - Extract current version info (files, hashes, metadata)
   - Extract previous version info from snapshot
   - Calculate differences using `calculate_version_changes/3`
   - Classify changes as added/deleted/modified
2. Generate combination records linking old and new versions
3. Provide detailed change analysis for each file

## File Processing Pipeline

### File Filtering Logic
```prolog
should_copy_file(File) :-
    \+ exists_directory(File),
    file_name_extension(_, Ext, File),
    member(Ext, [pl, py, txt, md, json]),
    file_base_name(File, Name),
    \+ sub_atom(Name, 0, 1, _, '.').
```

**Criteria:**
- Must be a regular file (not directory)
- Extension must be in allowed list
- Filename must not start with '.' (hidden files)
- Standard exclusions applied (e.g., __pycache__, .git)

### Content Hashing
```prolog
get_file_hash(File, Hash) :-
    open(File, read, Stream, [type(binary)]),
    read_stream_to_codes(Stream, Codes),
    close(Stream),
    term_hash(Codes, Hash).
```

Uses Prolog's built-in `term_hash/2` for consistent content-based hashing.

### Recursive File Copying
```prolog
copy_files_recursive(CurrentDir, TargetDir, SourceRoot) :-
    directory_files(CurrentDir, Files),
    forall(
        (member(File, Files), File \= '.', File \= '..'),
        process_file_or_directory(FullPath, TargetDir, SourceRoot)
    ).
```

**Features:**
- Maintains directory structure in target
- Preserves relative paths
- Handles nested directories recursively
- Applies file filtering at each level

## Testing Framework

### Test Execution Engine
```prolog
run_repository_tests(RepoName, Success, Errors) :-
    repositories_paths1([RepoPath]),
    atom_concat(RepoPath, RepoName, FullRepoPath),
    get_repository_main_files(FullRepoPath, MainFiles),
    run_tests_for_main_files(FullRepoPath, MainFiles, Success, Errors).
```

**Process:**
1. Create isolated testing environment
2. Copy repository files to testing directory
3. Generate test script with proper includes
4. Execute tests with configurable timeout
5. Capture results and error information
6. Clean up testing environment

### Test Script Generation
```prolog
test_script :-
    set_prolog_flag(stack_limit, 1000000000),
    catch((test_predicate), Error, handle_error(Error)),
    halt(0).
```

**Features:**
- Proper Prolog stack configuration
- Error handling with graceful failure
- Timeout protection
- Result capture and reporting

## Configuration System

### Hierarchical Configuration
1. **Base Configuration** (`settings.pl`)
   - Repository paths
   - Output directories  
   - Basic omit patterns

2. **V2 Extensions** (`luciancicd_v2_settings.pl`)
   - Snapshot configuration
   - Advanced testing options
   - Performance tuning
   - Feature toggles

3. **Repository Overrides**
   - Per-repository custom settings
   - Special handling for critical repos
   - Testing parameter adjustments

### Configuration Access
```prolog
get_v2_config(Key, Value) :-
    (   Key = snapshots_path -> snapshots_path_v2(Value)
    ;   Key = test_timeout -> test_timeout_v2(Value)
    ;   % ... additional mappings
    ).
```

## Error Handling Strategy

### Graceful Degradation
- **File Access Errors**: Continue with available files
- **Test Failures**: Isolate failures to specific repositories
- **Directory Creation**: Create missing directories automatically
- **Parser Errors**: Fall back to empty configurations

### Error Recovery Patterns
```prolog
safe_operation(Goal, Fallback) :-
    catch(Goal, Error, (log_error(Error), Fallback)).
```

Used throughout the system for robust error handling.

## Performance Optimizations

### Efficient Change Detection
- Content-based hashing avoids unnecessary file reads
- Early termination when changes detected
- Minimal memory usage during comparison

### Lazy Evaluation
- Repository discovery only when needed
- File processing on-demand
- Snapshot creation only for changed repositories

### Resource Management  
- Stream cleanup in all file operations
- Temporary directory management
- Memory-conscious data structures

## Integration Points

### Backward Compatibility
- Maintains original `settings.pl` format
- Preserves `main_file.txt` structure
- Compatible with existing repository layouts
- Supports original workflow patterns

### Extension Mechanisms
- Plugin architecture for custom processors
- Configurable file type handlers
- Extensible change analysis
- Modular test execution

## Diagnostic and Debugging

### Logging System
```prolog
debug_log(Level, Message) :-
    get_v2_config(enable_detailed_logging, true),
    format('~w: ~w~n', [Level, Message]).
```

### Status Reporting
```prolog
luciancicd_v2_status :-
    discover_repositories_v2(Repos),
    detect_changes_v2(Repos, Changes),
    report_system_status(Repos, Changes).
```

### Validation Framework
```prolog
validate_v2_config :-
    validate_paths_exist,
    validate_numeric_settings,
    validate_file_extensions.
```

## Data Flow Architecture

```
Repository Discovery → Snapshot Creation → Change Detection →
Version Combination → Change Generation → Test Execution →
Result Integration → Status Reporting
```

Each stage is designed for:
- **Independence**: Can be run separately for debugging
- **Idempotency**: Safe to run multiple times
- **Incrementality**: Only processes what has changed
- **Auditability**: Full logging of all operations

## Memory and Storage Patterns

### Memory Usage
- **Repository Objects**: Lightweight structures with lazy loading
- **File Hashes**: Computed on-demand, not cached
- **Test Results**: Streamed to disk, minimal in-memory storage

### Storage Layout
```
luciancicd-snapshots/
├── 20250807_120000/     # Timestamp-based snapshots
│   ├── repo1/           # Repository copies
│   ├── repo2/
│   └── snapshot_metadata.pl
luciancicd-changes/
├── 20250807_120030/     # Change records
│   ├── repo1/
│   │   ├── change_summary.pl
│   │   └── changes.txt
luciancicd-results/
├── test_results_20250807_120045.pl
```

This architecture provides a robust, scalable, and maintainable foundation for the Lucian CI/CD v2 system.