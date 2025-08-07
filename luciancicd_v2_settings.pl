% luciancicd_v2_settings.pl
%
% Lucian CI/CD v2 Settings and Configuration
% ==========================================
%
% Enhanced settings for the Prolog v2 implementation with repository versioning
% and change combination features.
%

% User configuration
user_v2("luciangreen").

% Repository paths for v2 system  
repositories_paths_v2([
    "../../GitHub2/"
]).

% Paths to omit during scanning
omit_paths_v2([
    "private2",
    ".git",
    "__pycache__",
    "node_modules",
    ".DS_Store"
]).

% Output and working directories for v2
output_path_v2("../../GitHub2o/").
testing_path_v2("../luciancicd-testing-v2/").
snapshots_path_v2("../../luciancicd-snapshots/").
changes_path_v2("../../luciancicd-changes/").
results_path_v2("../../luciancicd-results/").

% File extensions to track for changes
tracked_extensions_v2([
    "pl", "py", "txt", "md", "json", "yml", "yaml"
]).

% V2 specific configuration
max_changes_threshold_v2(25).
snapshot_retention_days_v2(30).
enable_detailed_diffs_v2(true).
parallel_testing_v2(true).
max_parallel_tests_v2(4).

% Time limits (in seconds)
test_timeout_v2(120).
repository_scan_timeout_v2(300).

% Advanced features configuration
enable_dependency_analysis_v2(true).
enable_change_impact_analysis_v2(true).  
enable_automatic_rollback_v2(true).
enable_change_verification_v2(true).

% Combination finding parameters
max_combinations_to_analyze_v2(100).
combination_analysis_depth_v2(3).
enable_smart_combination_filtering_v2(true).

% Notification and reporting settings
enable_email_notifications_v2(false).
notification_email_v2("user@example.com").
enable_detailed_logging_v2(true).
log_retention_days_v2(7).

% Integration settings
enable_git_integration_v2(true).
auto_commit_successful_changes_v2(false).
commit_message_template_v2("CI/CD v2 automated changes").

% Performance settings
enable_file_caching_v2(true).
cache_timeout_minutes_v2(30).
enable_incremental_snapshots_v2(true).

% Backward compatibility settings
maintain_v1_compatibility_v2(true).
use_legacy_test_format_v2(true).
migration_mode_v2(false).

% Repository-specific overrides (format: repository_name -> settings)
repository_overrides_v2([
    ["critical-repo", [
        max_changes_threshold(5),
        test_timeout(300),
        enable_automatic_rollback(true)
    ]],
    ["experimental-repo", [
        max_changes_threshold(50),
        enable_change_verification(false)
    ]]
]).

% Testing framework configuration
test_framework_v2("swipl").
test_runner_options_v2([
    stack_limit(1000000000),
    halt_on_error(true),
    capture_output(true)
]).

% Advanced dependency management
dependency_sources_v2([
    lppm_registry,
    local_analysis,
    explicit_declarations
]).

% Change analysis configuration
change_analysis_methods_v2([
    file_hash_comparison,
    content_diff_analysis,
    semantic_analysis,
    dependency_impact_analysis
]).

% Predicate to get v2 configuration value
get_v2_config(Key, Value) :-
    (   Key = user -> user_v2(Value)
    ;   Key = repositories_paths -> repositories_paths_v2(Value) 
    ;   Key = omit_paths -> omit_paths_v2(Value)
    ;   Key = output_path -> output_path_v2(Value)
    ;   Key = testing_path -> testing_path_v2(Value)
    ;   Key = snapshots_path -> snapshots_path_v2(Value)
    ;   Key = changes_path -> changes_path_v2(Value)
    ;   Key = results_path -> results_path_v2(Value)
    ;   Key = tracked_extensions -> tracked_extensions_v2(Value)
    ;   Key = max_changes_threshold -> max_changes_threshold_v2(Value)
    ;   Key = test_timeout -> test_timeout_v2(Value)
    ;   Key = enable_detailed_logging -> enable_detailed_logging_v2(Value)
    ;   format('Warning: Unknown config key: ~w~n', [Key]),
        fail
    ).

% Predicate to get repository-specific override
get_repository_override(RepoName, OverrideKey, Value) :-
    repository_overrides_v2(Overrides),
    member([RepoName, RepoOverrides], Overrides),
    member(Override, RepoOverrides),
    Override =.. [OverrideKey, Value].

% Predicate to validate v2 configuration
validate_v2_config :-
    write('🔧 Validating Lucian CI/CD v2 configuration...'), nl,
    validate_paths_exist,
    validate_numeric_settings,
    validate_file_extensions,
    write('✓ Configuration validation completed'), nl.

% Validate that required paths exist or can be created
validate_paths_exist :-
    repositories_paths_v2([RepoPath]),
    (   exists_directory(RepoPath)
    ->  true
    ;   format('Warning: Repository path does not exist: ~w~n', [RepoPath])
    ),
    
    % Validate and create working directories
    forall(
        member(PathKey, [snapshots_path, changes_path, results_path, testing_path]),
        (   get_v2_config(PathKey, Path),
            (   exists_directory(Path)
            ->  true
            ;   catch(make_directory(Path), _, 
                     format('Created directory: ~w~n', [Path]))
            )
        )
    ).

% Validate numeric settings are within reasonable ranges
validate_numeric_settings :-
    max_changes_threshold_v2(MaxChanges),
    (   MaxChanges > 0, MaxChanges =< 1000
    ->  true
    ;   format('Warning: max_changes_threshold_v2 should be between 1-1000, got ~w~n', [MaxChanges])
    ),
    
    test_timeout_v2(Timeout),
    (   Timeout > 0, Timeout =< 3600
    ->  true  
    ;   format('Warning: test_timeout_v2 should be between 1-3600 seconds, got ~w~n', [Timeout])
    ).

% Validate file extensions format
validate_file_extensions :-
    tracked_extensions_v2(Extensions),
    forall(
        member(Ext, Extensions),
        (   atom(Ext)
        ->  true
        ;   format('Warning: File extension should be atom, got ~w~n', [Ext])
        )
    ).

% Display current v2 configuration
show_v2_config :-
    write('Lucian CI/CD v2 Configuration'), nl,
    write('============================='), nl,
    user_v2(User),
    format('User: ~w~n', [User]),
    repositories_paths_v2([RepoPath]),
    format('Repository Path: ~w~n', [RepoPath]),
    max_changes_threshold_v2(MaxChanges),
    format('Max Changes Threshold: ~w~n', [MaxChanges]),
    test_timeout_v2(Timeout),
    format('Test Timeout: ~w seconds~n', [Timeout]),
    enable_detailed_logging_v2(Logging),
    format('Detailed Logging: ~w~n', [Logging]),
    write('============================='), nl.