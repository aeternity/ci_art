# CI Art - CI Artifact support library

## Introduction

The Aeternity core team went through a number of different CI systems, and finally (?)
settled on CircleCi. As part of the documentation of State Channels in the
[protocol repos](https://github.com/aeternity/protocol), we copied Markdown log files
generated from test suite runs. This library was created to semi-automate that process.

## Usage

This component is designed as a [rebar3 plugin](https://rebar3.readme.io/docs/using-available-plugins).

In the Aeternity protocol repos, the rebar.config file looks like this:

```erlang
{plugins, [{ci_art, "~> 0.1"}]}.
{artifact_roots,
 [#{ root => "node/api/examples/channels/json-rpc"
   , system => circleci
   , project => "gh/aeternity/aeternity"
   , latest_run => #{ <<"branch">> => <<"master">>
                    , <<"status">> => <<"success">>}
   , workflow => "build_test_deploy"
   , job => #{<<"name">> => <<"test_latest">>}
   }
 ]}.
```

The settings:
`artifact_roots` (`list()`), lists a number of file roots in the current repos.
The `root` attribute identifies the directory under which the artifacts are stored.

The `system` attribute identifies the CI system (currently, only CircleCI is supported).

The rest of the attributes are CI-system-specific. In this case:

`project` corresponds to the CircleCI option `"project-slug"`. If an `"org-slug"`
is needed, it will be derived from the project slug.

`latest_run` identifies how to match the test frun from which to fetch artifacts.
A test run is expected to contain one or more jobs. In this case we filter on `branch`
and `status`. Using `jsx` for JSON parsing, attributes and string values are all binaries.

`workflow` names the workflow

`job` names the job from which to fetch artifacts.

`artifacts` provides a filter for selecting artifacts to download. Only artifacts
which match this filter **and** correspond to a filename/-path under the root directory
will be downloaded.


## Using filters

Filters are expressed as maps, where the key corresponds to an attribute in an
object returned from the query, and the value is one of:

* A constant value, which is compared to the corresponding object value
* A map, which is treated as a sub-filter **iff** the object value is an object.
* A fun of arity 1, which is called with the object value and shall return `true`
  for a successful match. Anything else is interpreted as a failed match. If the
  fun calls `throw(cut)`, this indicates a final failed match, aborting further
  filtering of the data set. Whatever has matched until the cut will be returned.

The `ci_art` module exports a number of convenient filter functions:

* `f_date(Op, ErlangDateTime)` returns a fun/1 which compares a datetime with the
  given `ErlangDateTime` value, using the arithmetic operator `Op` (e.g. `'=='`)
* `f_date_cut(Op, ErlangDateTime)` works as `f_date/2` but throws a `cut` if the
  match fails. This can be used to restrict the search to data after a certain date,
  assuming that the CI API (like CircleCI) returns items in descending datetime order.
* `f_re(Regexp)` matches the value against `Regexp` (using `re:run/3`)
