# global-flags
[![Build Status](https://img.shields.io/travis/whitfin/global-flags.svg?label=unix)](https://travis-ci.org/whitfin/global-flags) [![Hex.pm Version](https://img.shields.io/hexpm/v/global_flags.svg)](https://hex.pm/packages/global_flags) [![Documentation](https://img.shields.io/badge/docs-latest-blue.svg)](https://hexdocs.pm/global_flags/)

This library is designed to provide an easy way to initialize global flags in Erlang/Elixir,
without having to be linked to a main application tree.

This is useful for libraries which require a certain state, regardless of the state of the
parent application. It's aimed at use cases where it's considered overhead to jump to an
other process, and it's wasteful to have an ETS table (for example).

This library is tiny, so you can include it with minimal overhead. It uses the atom table
to store flags, and as such it's not possible to unset a flag after it's set.

## Installation

### Rebar

Follow the instructons found [here](https://hex.pm/docs/rebar3_usage) to configure your
Rebar setup to use Hex as a dependency source, then you can grab it directly:

```erlang
{deps,[
  % pulls the latest version
  global_flags,
  % to pull the latest version from github
  {global_flags, {git, "git://github.com/whitfin/global-flags.git"}}
]}.
```

### Mix

To install it for your project, you can pull it directly from Hex. Rather
than use the version shown below, you can use the the latest version from
Hex (shown at the top of this README).

```elixir
def deps do
  [{:global_flags, "~> 1.0"}]
end
```

## Usage

### Erlang

```erlang
% manually set a flag (typically unused)
1> global_flags:set("my-flag")

% will run only the first time it's hit (and then sets the provided flag)
2> global_flags:once("app:init", fun() -> erlang:display("this will run once") end).

% won't run because the call above it set the "app:init" flag
3> global_flags:once("app:init", fun() -> erlang:display("same flag, so will never run") end).

% only runs when the flag has been set
4> global_flags:with("app:init", fun() -> erlang:display("will only run when set") end).

% only runs when the flag has been not been set
5> global_flags:without("app:init", fun() -> erlang:display("will only run when unset") end).
```

### Elixir

```elixir
# manually set a flag (typically unused)
iex(1)> :global_flags.set("my-flag")

# will run only the first time it's hit (and then sets the provided flag)
iex(2)> :global_flags.once("app:init", fn -> IO.puts("this will run once") end)

# won't run because the call above it set the "app:init" flag
iex(3)> :global_flags.once("app:init", fn -> IO.puts("same flag, so will never run") end)

# only runs when the flag has been set
iex(4)> :global_flags.with("app:init", fn -> IO.puts("will only run when set") end)

# only runs when the flag has been not been set
iex(5)> :global_flags.without("app:init", fn -> IO.puts("will only run when unset") end)
```

## Examples

This example is in Elixir, but it should be fairly understandable for those coming from
both languages. It contains a counter that's global to the entire runtime, which creates
itself on the first call. This is useful because all applications using this counter would
increment atomically, even if they're not explicitly linked directly.

```elixir
defmodule GlobalCounter do

  @doc """
  Retrieves the next integer in the global counter.
  """
  def next_int do
    # initializes the Agent only the first time called
    :global_flags.once("global_counter:init", fn ->
      Agent.start(fn -> 1 end, [ name: :global_counter ])
    end)

    # guaranteed to now have a started Agent
    Agent.get_and_update(:global_counter, fn count ->
      {count, count + 1}
    end)
  end
end
```
