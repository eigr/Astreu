defmodule Astreu.Protocol.MetaData.PropertiesEntry do
  @moduledoc false
  use Protobuf, map: true, syntax: :proto3

  @type t :: %__MODULE__{
          key: String.t(),
          value: String.t()
        }

  defstruct [:key, :value]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 15, 80, 114, 111, 112, 101, 114, 116, 105, 101, 115, 69, 110, 116, 114, 121, 18, 16,
        10, 3, 107, 101, 121, 24, 1, 32, 1, 40, 9, 82, 3, 107, 101, 121, 18, 20, 10, 5, 118, 97,
        108, 117, 101, 24, 2, 32, 1, 40, 9, 82, 5, 118, 97, 108, 117, 101, 58, 8, 8, 0, 16, 0, 24,
        0, 56, 1>>
    )
  end

  field(:key, 1, type: :string)
  field(:value, 2, type: :string)
end

defmodule Astreu.Protocol.MetaData do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          replyTo: String.t(),
          correlationId: String.t(),
          properties: %{String.t() => String.t()},
          timestamp: Google.Protobuf.Timestamp.t() | nil
        }

  defstruct [:replyTo, :correlationId, :properties, :timestamp]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 8, 77, 101, 116, 97, 68, 97, 116, 97, 18, 24, 10, 7, 114, 101, 112, 108, 121, 84, 111,
        24, 1, 32, 1, 40, 9, 82, 7, 114, 101, 112, 108, 121, 84, 111, 18, 36, 10, 13, 99, 111,
        114, 114, 101, 108, 97, 116, 105, 111, 110, 73, 100, 24, 2, 32, 1, 40, 9, 82, 13, 99, 111,
        114, 114, 101, 108, 97, 116, 105, 111, 110, 73, 100, 18, 73, 10, 10, 112, 114, 111, 112,
        101, 114, 116, 105, 101, 115, 24, 3, 32, 3, 40, 11, 50, 41, 46, 97, 115, 116, 114, 101,
        117, 46, 112, 114, 111, 116, 111, 99, 111, 108, 46, 77, 101, 116, 97, 68, 97, 116, 97, 46,
        80, 114, 111, 112, 101, 114, 116, 105, 101, 115, 69, 110, 116, 114, 121, 82, 10, 112, 114,
        111, 112, 101, 114, 116, 105, 101, 115, 18, 56, 10, 9, 116, 105, 109, 101, 115, 116, 97,
        109, 112, 24, 4, 32, 1, 40, 11, 50, 26, 46, 103, 111, 111, 103, 108, 101, 46, 112, 114,
        111, 116, 111, 98, 117, 102, 46, 84, 105, 109, 101, 115, 116, 97, 109, 112, 82, 9, 116,
        105, 109, 101, 115, 116, 97, 109, 112, 26, 67, 10, 15, 80, 114, 111, 112, 101, 114, 116,
        105, 101, 115, 69, 110, 116, 114, 121, 18, 16, 10, 3, 107, 101, 121, 24, 1, 32, 1, 40, 9,
        82, 3, 107, 101, 121, 18, 20, 10, 5, 118, 97, 108, 117, 101, 24, 2, 32, 1, 40, 9, 82, 5,
        118, 97, 108, 117, 101, 58, 8, 8, 0, 16, 0, 24, 0, 56, 1>>
    )
  end

  field(:replyTo, 1, type: :string)
  field(:correlationId, 2, type: :string)
  field(:properties, 3, repeated: true, type: Astreu.Protocol.MetaData.PropertiesEntry, map: true)
  field(:timestamp, 4, type: Google.Protobuf.Timestamp)
end

defmodule Astreu.Protocol.Message do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t(),
          topic: String.t(),
          data: Google.Protobuf.Any.t() | nil,
          metadata: Astreu.Protocol.MetaData.t() | nil
        }

  defstruct [:id, :topic, :data, :metadata]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 7, 77, 101, 115, 115, 97, 103, 101, 18, 14, 10, 2, 105, 100, 24, 1, 32, 1, 40, 9, 82,
        2, 105, 100, 18, 20, 10, 5, 116, 111, 112, 105, 99, 24, 2, 32, 1, 40, 9, 82, 5, 116, 111,
        112, 105, 99, 18, 40, 10, 4, 100, 97, 116, 97, 24, 3, 32, 1, 40, 11, 50, 20, 46, 103, 111,
        111, 103, 108, 101, 46, 112, 114, 111, 116, 111, 98, 117, 102, 46, 65, 110, 121, 82, 4,
        100, 97, 116, 97, 18, 53, 10, 8, 109, 101, 116, 97, 100, 97, 116, 97, 24, 4, 32, 1, 40,
        11, 50, 25, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108,
        46, 77, 101, 116, 97, 68, 97, 116, 97, 82, 8, 109, 101, 116, 97, 100, 97, 116, 97>>
    )
  end

  field(:id, 1, type: :string)
  field(:topic, 2, type: :string)
  field(:data, 3, type: Google.Protobuf.Any)
  field(:metadata, 4, type: Astreu.Protocol.MetaData)
end

defmodule Astreu.Protocol.AckMessage do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          response: {atom, any}
        }

  defstruct [:response]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 10, 65, 99, 107, 77, 101, 115, 115, 97, 103, 101, 18, 55, 10, 8, 109, 101, 116, 97,
        100, 97, 116, 97, 24, 1, 32, 1, 40, 11, 50, 25, 46, 97, 115, 116, 114, 101, 117, 46, 112,
        114, 111, 116, 111, 99, 111, 108, 46, 77, 101, 116, 97, 68, 97, 116, 97, 72, 0, 82, 8,
        109, 101, 116, 97, 100, 97, 116, 97, 18, 46, 10, 4, 100, 97, 116, 97, 24, 2, 32, 1, 40,
        11, 50, 24, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108,
        46, 77, 101, 115, 115, 97, 103, 101, 72, 0, 82, 4, 100, 97, 116, 97, 66, 10, 10, 8, 114,
        101, 115, 112, 111, 110, 115, 101>>
    )
  end

  oneof(:response, 0)
  field(:metadata, 1, type: Astreu.Protocol.MetaData, oneof: 0)
  field(:data, 2, type: Astreu.Protocol.Message, oneof: 0)
end
