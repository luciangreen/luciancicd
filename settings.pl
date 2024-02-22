user("luciangreen").

% Note: the following may or may not end in "/".

repositories_paths1([
"../../GitHub2/"
]).

omit_paths1([
"private2"
%"b" % omits GitHub2/b/
]).

output_path1([
"../../GitHub2o/"
]).

fail_if_greater_than_n_changes1(%7
20
).