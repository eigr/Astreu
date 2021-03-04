defmodule Astreu.Core.Protocol.Manager.Topic do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          name: String.t()
        }

  defstruct [:name]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 5, 84, 111, 112, 105, 99, 18, 18, 10, 4, 110, 97, 109, 101, 24, 1, 32, 1, 40, 9, 82,
        4, 110, 97, 109, 101>>
    )
  end

  field :name, 1, type: :string
end

defmodule Astreu.Core.Protocol.Manager.TopicInfo.PropertiesEntry do
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

  field :key, 1, type: :string
  field :value, 2, type: :string
end

defmodule Astreu.Core.Protocol.Manager.TopicInfo do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          id: String.t(),
          size: non_neg_integer,
          properties: %{String.t() => String.t()},
          createdAt: Google.Protobuf.Timestamp.t() | nil
        }

  defstruct [:id, :size, :properties, :createdAt]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 9, 84, 111, 112, 105, 99, 73, 110, 102, 111, 18, 14, 10, 2, 105, 100, 24, 1, 32, 1,
        40, 9, 82, 2, 105, 100, 18, 18, 10, 4, 115, 105, 122, 101, 24, 2, 32, 1, 40, 4, 82, 4,
        115, 105, 122, 101, 18, 87, 10, 10, 112, 114, 111, 112, 101, 114, 116, 105, 101, 115, 24,
        3, 32, 3, 40, 11, 50, 55, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112,
        114, 111, 116, 111, 99, 111, 108, 46, 109, 97, 110, 97, 103, 101, 114, 46, 84, 111, 112,
        105, 99, 73, 110, 102, 111, 46, 80, 114, 111, 112, 101, 114, 116, 105, 101, 115, 69, 110,
        116, 114, 121, 82, 10, 112, 114, 111, 112, 101, 114, 116, 105, 101, 115, 18, 56, 10, 9,
        99, 114, 101, 97, 116, 101, 100, 65, 116, 24, 4, 32, 1, 40, 11, 50, 26, 46, 103, 111, 111,
        103, 108, 101, 46, 112, 114, 111, 116, 111, 98, 117, 102, 46, 84, 105, 109, 101, 115, 116,
        97, 109, 112, 82, 9, 99, 114, 101, 97, 116, 101, 100, 65, 116, 26, 67, 10, 15, 80, 114,
        111, 112, 101, 114, 116, 105, 101, 115, 69, 110, 116, 114, 121, 18, 16, 10, 3, 107, 101,
        121, 24, 1, 32, 1, 40, 9, 82, 3, 107, 101, 121, 18, 20, 10, 5, 118, 97, 108, 117, 101, 24,
        2, 32, 1, 40, 9, 82, 5, 118, 97, 108, 117, 101, 58, 8, 8, 0, 16, 0, 24, 0, 56, 1>>
    )
  end

  field :id, 1, type: :string
  field :size, 2, type: :uint64

  field :properties, 3,
    repeated: true,
    type: Astreu.Core.Protocol.Manager.TopicInfo.PropertiesEntry,
    map: true

  field :createdAt, 4, type: Google.Protobuf.Timestamp
end

defmodule Astreu.Core.Protocol.Manager.TopicService.Service do
  @moduledoc false
  use GRPC.Service, name: "astreu.core.protocol.manager.TopicService"

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.ServiceDescriptorProto.decode(
      <<10, 12, 84, 111, 112, 105, 99, 83, 101, 114, 118, 105, 99, 101, 18, 93, 10, 4, 73, 110,
        102, 111, 18, 35, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112, 114,
        111, 116, 111, 99, 111, 108, 46, 109, 97, 110, 97, 103, 101, 114, 46, 84, 111, 112, 105,
        99, 26, 39, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112, 114, 111,
        116, 111, 99, 111, 108, 46, 109, 97, 110, 97, 103, 101, 114, 46, 84, 111, 112, 105, 99,
        73, 110, 102, 111, 34, 3, 136, 2, 0, 40, 0, 48, 0, 18, 76, 10, 4, 68, 114, 111, 112, 18,
        35, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112, 114, 111, 116, 111,
        99, 111, 108, 46, 109, 97, 110, 97, 103, 101, 114, 46, 84, 111, 112, 105, 99, 26, 22, 46,
        103, 111, 111, 103, 108, 101, 46, 112, 114, 111, 116, 111, 98, 117, 102, 46, 69, 109, 112,
        116, 121, 34, 3, 136, 2, 0, 40, 0, 48, 0, 18, 95, 10, 6, 67, 114, 101, 97, 116, 101, 18,
        35, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112, 114, 111, 116, 111,
        99, 111, 108, 46, 109, 97, 110, 97, 103, 101, 114, 46, 84, 111, 112, 105, 99, 26, 39, 46,
        97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112, 114, 111, 116, 111, 99, 111,
        108, 46, 109, 97, 110, 97, 103, 101, 114, 46, 84, 111, 112, 105, 99, 73, 110, 102, 111,
        34, 3, 136, 2, 0, 40, 0, 48, 0>>
    )
  end

  rpc :Info, Astreu.Core.Protocol.Manager.Topic, Astreu.Core.Protocol.Manager.TopicInfo

  rpc :Drop, Astreu.Core.Protocol.Manager.Topic, Google.Protobuf.Empty

  rpc :Create, Astreu.Core.Protocol.Manager.Topic, Astreu.Core.Protocol.Manager.TopicInfo
end

defmodule Astreu.Core.Protocol.Manager.TopicService.Stub do
  @moduledoc false
  use GRPC.Stub, service: Astreu.Core.Protocol.Manager.TopicService.Service
end
