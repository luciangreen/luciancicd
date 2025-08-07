# Lucian CI/CD v2 (Prolog Implementation) - User Guide

## Overview

Lucian CI/CD v2 is a complete rewrite of the CI/CD system in Prolog, featuring advanced repository versioning, change combination analysis, and enhanced testing capabilities. This implementation provides the core functionality requested: saving previous repository versions, finding combinations between old and new repositories, and producing/saving changes.

## Key Features

### ✅ Repository Versioning
- **Automatic Snapshots**: Creates timestamped snapshots of all repository states
- **Content Hashing**: Uses SHA-256-like hashing for reliable change detection  
- **Metadata Preservation**: Saves repository configuration and dependencies with each snapshot
- **Efficient Storage**: Only tracks relevant file types (.pl, .py, .txt, .md, .json)

### ✅ Change Detection & Combination Finding
- **Hash-Based Comparison**: Compares file content hashes between versions
- **Version Combination Analysis**: Finds relationships between old and new repository versions
- **Change Classification**: Categorizes changes as added, deleted, or modified files
- **Impact Analysis**: Identifies which repositories are affected by changes

### ✅ Change Generation & Saving
- **Detailed Change Records**: Creates comprehensive change summaries with timestamps
- **File-Level Diffs**: Tracks specific files that changed between versions
- **Change Persistence**: Saves all changes to the luciancicd-changes directory
- **Audit Trail**: Maintains complete history of all changes processed

### ✅ Enhanced Testing
- **Prolog Test Execution**: Runs tests defined in main_file.txt configurations
- **Timeout Handling**: Configurable timeouts prevent hanging tests
- **Error Recovery**: Graceful handling of test failures and system errors
- **Result Logging**: Comprehensive test results saved with timestamps

## Installation & Setup

### Prerequisites
- SWI-Prolog 9.0+ installed
- Repository structure following Lucian CI/CD conventions
- Access to GitHub2 directory with repositories

### Quick Setup
```prolog
% Load the system
?- consult('luciancicd_v2.pl').

% Initialize the system (creates directories and first snapshot)
?- set_up_luciancicd_v2.

% Check system status
?- luciancicd_v2_status.
```

## Usage

### Main Commands

#### Run Complete CI/CD Cycle
```prolog
?- luciancicd_v2.
```
This will:
1. Discover all repositories
2. Detect changes since last snapshot
3. Find combinations between old/new versions  
4. Generate and save change records
5. Run tests for affected repositories
6. Integrate successful changes

#### Setup Initial State
```prolog
?- set_up_luciancicd_v2.
```
Creates initial snapshots of all repositories.

#### Check System Status
```prolog
?- luciancicd_v2_status.
```
Shows:
- Number of discovered repositories
- Last snapshot timestamp
- Currently changed repositories

#### Run System Tests
```prolog
?- test_luciancicd_v2.
```
Validates all system components.

### Configuration

The system uses two configuration files:

#### settings.pl (Original Settings)
```prolog
repositories_paths1(["../GitHub2/"]).
omit_paths1(["private2"]).
output_path1(["../../GitHub2o/"]).
```

#### luciancicd_v2_settings.pl (V2 Enhancements)
```prolog
snapshots_path_v2("../../luciancicd-snapshots/").
changes_path_v2("../../luciancicd-changes/").  
max_changes_threshold_v2(25).
test_timeout_v2(120).
enable_detailed_logging_v2(true).
```

## Repository Structure Requirements

### main_file.txt Format
Each repository should have a `main_file.txt` file defining testable predicates:

```prolog
[["test.pl", [["test_pred", 1]]]].
```

This tells the system:
- File: `test.pl` contains testable code
- Predicate: `test_pred/1` should be tested

### Directory Structure
```
GitHub2/
├── repo1/
│   ├── main_file.txt
│   ├── code.pl
│   └── ...
├── repo2/
│   ├── main_file.txt
│   ├── code.pl
│   └── ...
```

## Generated Artifacts

### Snapshots
Located in `../../luciancicd-snapshots/TIMESTAMP/`
- Complete file copies of each repository
- Metadata files with repository information
- Timestamp-based organization

### Change Records  
Located in `../../luciancicd-changes/TIMESTAMP/`
- Change summaries for each repository
- Detailed file-level change information
- Version comparison data

### Test Results
Located in `../../luciancicd-results/`
- Complete test execution results
- Performance metrics and timing
- Error logs and debugging information

## Advanced Features

### Version Combinations
The system analyzes relationships between repository versions:

```prolog
combination(RepoName, OldVersion, NewVersion, Changes)
```

Where:
- `OldVersion`: Previous repository state with file hashes
- `NewVersion`: Current repository state  
- `Changes`: List of `added(File,Hash)`, `deleted(File,Hash)`, `modified(File,OldHash,NewHash)`

### Change Impact Analysis
Automatically determines:
- Which files changed between versions
- What type of changes occurred
- Which repositories might be affected
- Risk assessment based on change patterns

### Testing Integration
- Loads repository files into test environment
- Executes predicates defined in main_file.txt
- Captures test output and errors
- Provides detailed success/failure reporting

## Troubleshooting

### Common Issues

#### "No repositories discovered"
- Check `repositories_paths1/1` setting in settings.pl
- Ensure GitHub2 directory exists and contains repositories
- Verify directory permissions

#### "Arguments are not sufficiently instantiated"  
- Usually indicates file path or directory issues
- Check that all referenced directories exist
- Verify file permissions for reading/writing

#### "Test timeout"
- Increase `test_timeout_v2/1` setting
- Check for infinite loops in test predicates
- Verify SWI-Prolog stack limits

### Debug Mode
Enable detailed logging:
```prolog
?- get_v2_config(enable_detailed_logging, true).
```

## Integration with Original System

The v2 system maintains backward compatibility:
- Uses existing `settings.pl` configuration  
- Maintains `main_file.txt` format
- Integrates with existing repository structure
- Preserves original CI/CD workflow concepts

## Example Session

```prolog
% Start SWI-Prolog
$ swipl

% Load v2 system
?- consult('luciancicd_v2.pl').

% Setup initial state
?- set_up_luciancicd_v2.
🔧 Setting up Lucian CI/CD v2 initial state...
📁 Discovered 3 repositories
✓ Initial state saved

% Make some changes to repositories...
% (edit files in GitHub2/)

% Run CI/CD cycle
?- luciancicd_v2.
🚀 Starting Lucian CI/CD v2 (Prolog Implementation)...
📁 Discovered 3 repositories  
🔍 Detecting changes...
📋 Found 2 changed repositories: [repo1, repo2]
⚙️  Processing changes...
🔄 Finding combinations between old and new versions...
✓ Found 2 version combinations
💾 Generating and saving changes...
🧪 Running tests for changed repositories...
✓ repo1 (0.15 seconds)
✓ repo2 (0.08 seconds)
🎯 Integrating successful changes...
✓ All tests passed - CI/CD cycle successful

% Check status
?- luciancicd_v2_status.
📋 Lucian CI/CD v2 Status
========================
Repositories: 3
Last snapshot: ../../luciancicd-snapshots/20250807_020123
Changed repositories: []
========================
```

## Performance Characteristics

- **Memory Usage**: Minimal - only loads necessary files
- **Speed**: Fast change detection using content hashing
- **Storage**: Efficient - only stores changed files in snapshots
- **Scalability**: Handles dozens of repositories efficiently

## Future Enhancements

Planned improvements for future versions:
- Integration with Git for advanced version control
- Email notifications for test failures
- Web interface for monitoring CI/CD status
- Advanced dependency analysis and resolution
- Parallel test execution across repositories