defmodule Astreu.PersistenceAdapter do
  @doc """
  Persist messages in some storage.
  """
  @callback persist(String.t()) :: {:ok, term} | {:error, String.t()}

  @doc """
  Retrive messages from some storage.
  """
  @callback retrieve() :: [String.t()]

  @doc """
  Retrive messages from some storage.
  """
  @callback delete(String.t()) :: {:ok, term} | {:error, String.t()}
end
