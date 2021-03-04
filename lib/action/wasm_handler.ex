defmodule Astreu.WasmHandler do
  @moduledoc """
  Astreu.WasmHandler module.
  """

  @doc """
  Use Wasm in Elixir.

  ## Examples

      iex> bytes = File.read!("test/wasm_test/target/wasm32-unknown-unknown/debug/wasmex_test.wasm")
      iex> {:ok, instance } = Wasmex.start_link(bytes)
      iex> {:ok, memory} = Wasmex.memory(instance, :uint8, 0)

      iex> Astreu.WasmHandler.write_binary(memory, 0, "Hello") # :ok
      iex> Astreu.WasmHandler.read_binary(memory, 0) # "Hello"

      iex> # just a test to see what we actually wrote to memory,
      iex> # we see the first 8 bytes being the size followed by actual content
      iex> Wasmex.Memory.read_binary(memory, 0, 8 + 5)

      iex> # Call function
      iex> in_str_pointer = 0 # you can make this one up. if there is nothing else in memory, `0` is a good value. otherwise be careful not to overwrite existing data in memory.
      iex> in_str = "Hello World"
      iex> :ok = Astreu.WasmHandler.write_binary(memory, in_str_pointer, in_str)
      iex> {:ok, [out_str_pointer]} = Wasmex.call_function(instance, :string_receive_and_result_bytes, [in_str_pointer])
      iex> out_str = Astreu.WasmHandler.read_binary(memory, out_str_pointer)

  """

  alias Wasmex, as: Wasm

  def get_instance(bytes), do: Wasm.start_link(bytes)

  def get_memory(instance, pointer), do: Wasm.memory(instance, :uint8, pointer)

  def write_binary(memory, index, binary) when is_binary(binary) do
    length = byte_size(binary)
    # 8 bytes, big endian
    length_bytes = <<length::64-big>>

    Wasm.Memory.write_binary(memory, index, length_bytes)
    Wasm.Memory.write_binary(memory, index + 8, binary)
  end

  def read_binary(memory, index) do
    length =
      memory
      |> Wasm.Memory.read_binary(index, 8)
      |> :binary.decode_unsigned(:big)

    Wasm.Memory.read_binary(memory, index + 8, length)
  end
end
