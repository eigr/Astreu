defmodule AstreuTest do
  use ExUnit.Case
  doctest Astreu

  test "greets the world" do
    assert Astreu.hello() == :world
  end
end
