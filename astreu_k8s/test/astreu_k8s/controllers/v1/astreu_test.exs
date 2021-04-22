defmodule AstreuK8s.Controller.V1.AstreuTest do
  @moduledoc false
  use ExUnit.Case, async: false
  alias AstreuK8s.Controller.V1.Astreu

  describe "add/1" do
    test "returns :ok" do
      event = %{}
      result = Astreu.add(event)
      assert result == :ok
    end
  end

  describe "modify/1" do
    test "returns :ok" do
      event = %{}
      result = Astreu.modify(event)
      assert result == :ok
    end
  end

  describe "delete/1" do
    test "returns :ok" do
      event = %{}
      result = Astreu.delete(event)
      assert result == :ok
    end
  end

  describe "reconcile/1" do
    test "returns :ok" do
      event = %{}
      result = Astreu.reconcile(event)
      assert result == :ok
    end
  end
end
