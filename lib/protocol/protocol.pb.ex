defmodule Astreu.Protocol.Failure.Type do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3
  @type t :: integer | :TRANSIENT | :FATAL

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.EnumDescriptorProto.decode(
      <<10, 4, 84, 121, 112, 101, 18, 13, 10, 9, 84, 82, 65, 78, 83, 73, 69, 78, 84, 16, 0, 18, 9,
        10, 5, 70, 65, 84, 65, 76, 16, 1>>
    )
  end

  field(:TRANSIENT, 0)

  field(:FATAL, 1)
end

defmodule Astreu.Protocol.Ack.Reason do
  @moduledoc false
  use Protobuf, enum: true, syntax: :proto3
  @type t :: integer | :ACCEPT | :REJECT | :KNOWLEDGE

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.EnumDescriptorProto.decode(
      <<10, 6, 82, 101, 97, 115, 111, 110, 18, 10, 10, 6, 65, 67, 67, 69, 80, 84, 16, 0, 18, 10,
        10, 6, 82, 69, 74, 69, 67, 84, 16, 1, 18, 13, 10, 9, 75, 78, 79, 87, 76, 69, 68, 71, 69,
        16, 2>>
    )
  end

  field(:ACCEPT, 0)

  field(:REJECT, 1)

  field(:KNOWLEDGE, 2)
end

defmodule Astreu.Protocol.Metadata.PropertiesEntry do
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

defmodule Astreu.Protocol.Metadata do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          replyTo: String.t(),
          correlation: String.t(),
          properties: %{String.t() => String.t()},
          timestamp: Google.Protobuf.Timestamp.t() | nil
        }

  defstruct [:replyTo, :correlation, :properties, :timestamp]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 8, 77, 101, 116, 97, 100, 97, 116, 97, 18, 24, 10, 7, 114, 101, 112, 108, 121, 84,
        111, 24, 1, 32, 1, 40, 9, 82, 7, 114, 101, 112, 108, 121, 84, 111, 18, 32, 10, 11, 99,
        111, 114, 114, 101, 108, 97, 116, 105, 111, 110, 24, 2, 32, 1, 40, 9, 82, 11, 99, 111,
        114, 114, 101, 108, 97, 116, 105, 111, 110, 18, 73, 10, 10, 112, 114, 111, 112, 101, 114,
        116, 105, 101, 115, 24, 3, 32, 3, 40, 11, 50, 41, 46, 97, 115, 116, 114, 101, 117, 46,
        112, 114, 111, 116, 111, 99, 111, 108, 46, 77, 101, 116, 97, 100, 97, 116, 97, 46, 80,
        114, 111, 112, 101, 114, 116, 105, 101, 115, 69, 110, 116, 114, 121, 82, 10, 112, 114,
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
  field(:correlation, 2, type: :string)
  field(:properties, 3, repeated: true, type: Astreu.Protocol.Metadata.PropertiesEntry, map: true)
  field(:timestamp, 4, type: Google.Protobuf.Timestamp)
end

defmodule Astreu.Protocol.Info.PropertiesEntry do
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

defmodule Astreu.Protocol.Info do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          uuid: String.t(),
          properties: %{String.t() => String.t()},
          timestamp: Google.Protobuf.Timestamp.t() | nil
        }

  defstruct [:uuid, :properties, :timestamp]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 4, 73, 110, 102, 111, 18, 18, 10, 4, 117, 117, 105, 100, 24, 1, 32, 1, 40, 9, 82, 4,
        117, 117, 105, 100, 18, 69, 10, 10, 112, 114, 111, 112, 101, 114, 116, 105, 101, 115, 24,
        3, 32, 3, 40, 11, 50, 37, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111,
        99, 111, 108, 46, 73, 110, 102, 111, 46, 80, 114, 111, 112, 101, 114, 116, 105, 101, 115,
        69, 110, 116, 114, 121, 82, 10, 112, 114, 111, 112, 101, 114, 116, 105, 101, 115, 18, 56,
        10, 9, 116, 105, 109, 101, 115, 116, 97, 109, 112, 24, 4, 32, 1, 40, 11, 50, 26, 46, 103,
        111, 111, 103, 108, 101, 46, 112, 114, 111, 116, 111, 98, 117, 102, 46, 84, 105, 109, 101,
        115, 116, 97, 109, 112, 82, 9, 116, 105, 109, 101, 115, 116, 97, 109, 112, 26, 67, 10, 15,
        80, 114, 111, 112, 101, 114, 116, 105, 101, 115, 69, 110, 116, 114, 121, 18, 16, 10, 3,
        107, 101, 121, 24, 1, 32, 1, 40, 9, 82, 3, 107, 101, 121, 18, 20, 10, 5, 118, 97, 108,
        117, 101, 24, 2, 32, 1, 40, 9, 82, 5, 118, 97, 108, 117, 101, 58, 8, 8, 0, 16, 0, 24, 0,
        56, 1>>
    )
  end

  field(:uuid, 1, type: :string)
  field(:properties, 3, repeated: true, type: Astreu.Protocol.Info.PropertiesEntry, map: true)
  field(:timestamp, 4, type: Google.Protobuf.Timestamp)
end

defmodule Astreu.Protocol.Failure do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          uuid: String.t(),
          correlation: String.t(),
          description: String.t(),
          timestamp: Google.Protobuf.Timestamp.t() | nil
        }

  defstruct [:uuid, :correlation, :description, :timestamp]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 7, 70, 97, 105, 108, 117, 114, 101, 18, 18, 10, 4, 117, 117, 105, 100, 24, 1, 32, 1,
        40, 9, 82, 4, 117, 117, 105, 100, 18, 32, 10, 11, 99, 111, 114, 114, 101, 108, 97, 116,
        105, 111, 110, 24, 2, 32, 1, 40, 9, 82, 11, 99, 111, 114, 114, 101, 108, 97, 116, 105,
        111, 110, 18, 32, 10, 11, 100, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110, 24, 3, 32,
        1, 40, 9, 82, 11, 100, 101, 115, 99, 114, 105, 112, 116, 105, 111, 110, 18, 56, 10, 9,
        116, 105, 109, 101, 115, 116, 97, 109, 112, 24, 4, 32, 1, 40, 11, 50, 26, 46, 103, 111,
        111, 103, 108, 101, 46, 112, 114, 111, 116, 111, 98, 117, 102, 46, 84, 105, 109, 101, 115,
        116, 97, 109, 112, 82, 9, 116, 105, 109, 101, 115, 116, 97, 109, 112, 34, 32, 10, 4, 84,
        121, 112, 101, 18, 13, 10, 9, 84, 82, 65, 78, 83, 73, 69, 78, 84, 16, 0, 18, 9, 10, 5, 70,
        65, 84, 65, 76, 16, 1>>
    )
  end

  field(:uuid, 1, type: :string)
  field(:correlation, 2, type: :string)
  field(:description, 3, type: :string)
  field(:timestamp, 4, type: Google.Protobuf.Timestamp)
end

defmodule Astreu.Protocol.Connect.PropertiesEntry do
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

defmodule Astreu.Protocol.Connect do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          uuid: String.t(),
          topic: String.t(),
          subscription: String.t(),
          properties: %{String.t() => String.t()},
          timestamp: Google.Protobuf.Timestamp.t() | nil
        }

  defstruct [:uuid, :topic, :subscription, :properties, :timestamp]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 7, 67, 111, 110, 110, 101, 99, 116, 18, 18, 10, 4, 117, 117, 105, 100, 24, 1, 32, 1,
        40, 9, 82, 4, 117, 117, 105, 100, 18, 20, 10, 5, 116, 111, 112, 105, 99, 24, 2, 32, 1, 40,
        9, 82, 5, 116, 111, 112, 105, 99, 18, 34, 10, 12, 115, 117, 98, 115, 99, 114, 105, 112,
        116, 105, 111, 110, 24, 3, 32, 1, 40, 9, 82, 12, 115, 117, 98, 115, 99, 114, 105, 112,
        116, 105, 111, 110, 18, 72, 10, 10, 112, 114, 111, 112, 101, 114, 116, 105, 101, 115, 24,
        4, 32, 3, 40, 11, 50, 40, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111,
        99, 111, 108, 46, 67, 111, 110, 110, 101, 99, 116, 46, 80, 114, 111, 112, 101, 114, 116,
        105, 101, 115, 69, 110, 116, 114, 121, 82, 10, 112, 114, 111, 112, 101, 114, 116, 105,
        101, 115, 18, 56, 10, 9, 116, 105, 109, 101, 115, 116, 97, 109, 112, 24, 5, 32, 1, 40, 11,
        50, 26, 46, 103, 111, 111, 103, 108, 101, 46, 112, 114, 111, 116, 111, 98, 117, 102, 46,
        84, 105, 109, 101, 115, 116, 97, 109, 112, 82, 9, 116, 105, 109, 101, 115, 116, 97, 109,
        112, 26, 67, 10, 15, 80, 114, 111, 112, 101, 114, 116, 105, 101, 115, 69, 110, 116, 114,
        121, 18, 16, 10, 3, 107, 101, 121, 24, 1, 32, 1, 40, 9, 82, 3, 107, 101, 121, 18, 20, 10,
        5, 118, 97, 108, 117, 101, 24, 2, 32, 1, 40, 9, 82, 5, 118, 97, 108, 117, 101, 58, 8, 8,
        0, 16, 0, 24, 0, 56, 1>>
    )
  end

  field(:uuid, 1, type: :string)
  field(:topic, 2, type: :string)
  field(:subscription, 3, type: :string)
  field(:properties, 4, repeated: true, type: Astreu.Protocol.Connect.PropertiesEntry, map: true)
  field(:timestamp, 5, type: Google.Protobuf.Timestamp)
end

defmodule Astreu.Protocol.Disconnect do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          uuid: String.t(),
          topic: String.t(),
          subscription: String.t(),
          timestamp: Google.Protobuf.Timestamp.t() | nil
        }

  defstruct [:uuid, :topic, :subscription, :timestamp]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 10, 68, 105, 115, 99, 111, 110, 110, 101, 99, 116, 18, 18, 10, 4, 117, 117, 105, 100,
        24, 1, 32, 1, 40, 9, 82, 4, 117, 117, 105, 100, 18, 20, 10, 5, 116, 111, 112, 105, 99, 24,
        2, 32, 1, 40, 9, 82, 5, 116, 111, 112, 105, 99, 18, 34, 10, 12, 115, 117, 98, 115, 99,
        114, 105, 112, 116, 105, 111, 110, 24, 3, 32, 1, 40, 9, 82, 12, 115, 117, 98, 115, 99,
        114, 105, 112, 116, 105, 111, 110, 18, 56, 10, 9, 116, 105, 109, 101, 115, 116, 97, 109,
        112, 24, 4, 32, 1, 40, 11, 50, 26, 46, 103, 111, 111, 103, 108, 101, 46, 112, 114, 111,
        116, 111, 98, 117, 102, 46, 84, 105, 109, 101, 115, 116, 97, 109, 112, 82, 9, 116, 105,
        109, 101, 115, 116, 97, 109, 112>>
    )
  end

  field(:uuid, 1, type: :string)
  field(:topic, 2, type: :string)
  field(:subscription, 3, type: :string)
  field(:timestamp, 4, type: Google.Protobuf.Timestamp)
end

defmodule Astreu.Protocol.System do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          data: {atom, any}
        }

  defstruct [:data]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 6, 83, 121, 115, 116, 101, 109, 18, 43, 10, 4, 105, 110, 102, 111, 24, 1, 32, 1, 40,
        11, 50, 21, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108,
        46, 73, 110, 102, 111, 72, 0, 82, 4, 105, 110, 102, 111, 18, 52, 10, 7, 102, 97, 105, 108,
        117, 114, 101, 24, 2, 32, 1, 40, 11, 50, 24, 46, 97, 115, 116, 114, 101, 117, 46, 112,
        114, 111, 116, 111, 99, 111, 108, 46, 70, 97, 105, 108, 117, 114, 101, 72, 0, 82, 7, 102,
        97, 105, 108, 117, 114, 101, 18, 52, 10, 7, 99, 111, 110, 110, 101, 99, 116, 24, 3, 32, 1,
        40, 11, 50, 24, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111,
        108, 46, 67, 111, 110, 110, 101, 99, 116, 72, 0, 82, 7, 99, 111, 110, 110, 101, 99, 116,
        18, 61, 10, 10, 100, 105, 115, 99, 111, 110, 110, 101, 99, 116, 24, 4, 32, 1, 40, 11, 50,
        27, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108, 46, 68,
        105, 115, 99, 111, 110, 110, 101, 99, 116, 72, 0, 82, 10, 100, 105, 115, 99, 111, 110,
        110, 101, 99, 116, 66, 6, 10, 4, 100, 97, 116, 97>>
    )
  end

  oneof(:data, 0)
  field(:info, 1, type: Astreu.Protocol.Info, oneof: 0)
  field(:failure, 2, type: Astreu.Protocol.Failure, oneof: 0)
  field(:connect, 3, type: Astreu.Protocol.Connect, oneof: 0)
  field(:disconnect, 4, type: Astreu.Protocol.Disconnect, oneof: 0)
end

defmodule Astreu.Protocol.Exchange do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          uuid: String.t(),
          metadata: Astreu.Protocol.Metadata.t() | nil,
          message: Google.Protobuf.Any.t() | nil
        }

  defstruct [:uuid, :metadata, :message]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 8, 69, 120, 99, 104, 97, 110, 103, 101, 18, 18, 10, 4, 117, 117, 105, 100, 24, 1, 32,
        1, 40, 9, 82, 4, 117, 117, 105, 100, 18, 53, 10, 8, 109, 101, 116, 97, 100, 97, 116, 97,
        24, 2, 32, 1, 40, 11, 50, 25, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116,
        111, 99, 111, 108, 46, 77, 101, 116, 97, 100, 97, 116, 97, 82, 8, 109, 101, 116, 97, 100,
        97, 116, 97, 18, 46, 10, 7, 109, 101, 115, 115, 97, 103, 101, 24, 3, 32, 1, 40, 11, 50,
        20, 46, 103, 111, 111, 103, 108, 101, 46, 112, 114, 111, 116, 111, 98, 117, 102, 46, 65,
        110, 121, 82, 7, 109, 101, 115, 115, 97, 103, 101>>
    )
  end

  field(:uuid, 1, type: :string)
  field(:metadata, 2, type: Astreu.Protocol.Metadata)
  field(:message, 3, type: Google.Protobuf.Any)
end

defmodule Astreu.Protocol.Ack do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          uuid: String.t(),
          reason: Astreu.Protocol.Ack.Reason.t(),
          metadata: Astreu.Protocol.Metadata.t() | nil,
          subscription: String.t()
        }

  defstruct [:uuid, :reason, :metadata, :subscription]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 3, 65, 99, 107, 18, 18, 10, 4, 117, 117, 105, 100, 24, 1, 32, 1, 40, 9, 82, 4, 117,
        117, 105, 100, 18, 51, 10, 6, 114, 101, 97, 115, 111, 110, 24, 2, 32, 1, 40, 14, 50, 27,
        46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108, 46, 65, 99,
        107, 46, 82, 101, 97, 115, 111, 110, 82, 6, 114, 101, 97, 115, 111, 110, 18, 53, 10, 8,
        109, 101, 116, 97, 100, 97, 116, 97, 24, 3, 32, 1, 40, 11, 50, 25, 46, 97, 115, 116, 114,
        101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108, 46, 77, 101, 116, 97, 100, 97, 116,
        97, 82, 8, 109, 101, 116, 97, 100, 97, 116, 97, 18, 34, 10, 12, 115, 117, 98, 115, 99,
        114, 105, 112, 116, 105, 111, 110, 24, 4, 32, 1, 40, 9, 82, 12, 115, 117, 98, 115, 99,
        114, 105, 112, 116, 105, 111, 110, 34, 47, 10, 6, 82, 101, 97, 115, 111, 110, 18, 10, 10,
        6, 65, 67, 67, 69, 80, 84, 16, 0, 18, 10, 10, 6, 82, 69, 74, 69, 67, 84, 16, 1, 18, 13,
        10, 9, 75, 78, 79, 87, 76, 69, 68, 71, 69, 16, 2>>
    )
  end

  field(:uuid, 1, type: :string)
  field(:reason, 2, type: Astreu.Protocol.Ack.Reason, enum: true)
  field(:metadata, 3, type: Astreu.Protocol.Metadata)
  field(:subscription, 4, type: :string)
end

defmodule Astreu.Protocol.Message do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          data: {atom, any}
        }

  defstruct [:data]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 7, 77, 101, 115, 115, 97, 103, 101, 18, 49, 10, 6, 115, 121, 115, 116, 101, 109, 24,
        1, 32, 1, 40, 11, 50, 23, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111,
        99, 111, 108, 46, 83, 121, 115, 116, 101, 109, 72, 0, 82, 6, 115, 121, 115, 116, 101, 109,
        18, 55, 10, 8, 101, 120, 99, 104, 97, 110, 103, 101, 24, 2, 32, 1, 40, 11, 50, 25, 46, 97,
        115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108, 46, 69, 120, 99, 104,
        97, 110, 103, 101, 72, 0, 82, 8, 101, 120, 99, 104, 97, 110, 103, 101, 18, 40, 10, 3, 97,
        99, 107, 24, 3, 32, 1, 40, 11, 50, 20, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111,
        116, 111, 99, 111, 108, 46, 65, 99, 107, 72, 0, 82, 3, 97, 99, 107, 66, 6, 10, 4, 100, 97,
        116, 97>>
    )
  end

  oneof(:data, 0)
  field(:system, 1, type: Astreu.Protocol.System, oneof: 0)
  field(:exchange, 2, type: Astreu.Protocol.Exchange, oneof: 0)
  field(:ack, 3, type: Astreu.Protocol.Ack, oneof: 0)
end
