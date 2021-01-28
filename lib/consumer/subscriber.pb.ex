defmodule Astreu.Consumer.Options.PropertiesEntry do
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

defmodule Astreu.Consumer.Options do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          topic: String.t(),
          subscriberId: String.t(),
          properties: %{String.t() => String.t()}
        }

  defstruct [:topic, :subscriberId, :properties]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 7, 79, 112, 116, 105, 111, 110, 115, 18, 20, 10, 5, 116, 111, 112, 105, 99, 24, 1, 32,
        1, 40, 9, 82, 5, 116, 111, 112, 105, 99, 18, 34, 10, 12, 115, 117, 98, 115, 99, 114, 105,
        98, 101, 114, 73, 100, 24, 2, 32, 1, 40, 9, 82, 12, 115, 117, 98, 115, 99, 114, 105, 98,
        101, 114, 73, 100, 18, 72, 10, 10, 112, 114, 111, 112, 101, 114, 116, 105, 101, 115, 24,
        3, 32, 3, 40, 11, 50, 40, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 110, 115, 117,
        109, 101, 114, 46, 79, 112, 116, 105, 111, 110, 115, 46, 80, 114, 111, 112, 101, 114, 116,
        105, 101, 115, 69, 110, 116, 114, 121, 82, 10, 112, 114, 111, 112, 101, 114, 116, 105,
        101, 115, 26, 67, 10, 15, 80, 114, 111, 112, 101, 114, 116, 105, 101, 115, 69, 110, 116,
        114, 121, 18, 16, 10, 3, 107, 101, 121, 24, 1, 32, 1, 40, 9, 82, 3, 107, 101, 121, 18, 20,
        10, 5, 118, 97, 108, 117, 101, 24, 2, 32, 1, 40, 9, 82, 5, 118, 97, 108, 117, 101, 58, 8,
        8, 0, 16, 0, 24, 0, 56, 1>>
    )
  end

  field(:topic, 1, type: :string)
  field(:subscriberId, 2, type: :string)
  field(:properties, 3, repeated: true, type: Astreu.Consumer.Options.PropertiesEntry, map: true)
end

defmodule Astreu.Consumer.Info do
  @moduledoc false
  use Protobuf, syntax: :proto3

  @type t :: %__MODULE__{
          info: {atom, any}
        }

  defstruct [:info]

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.DescriptorProto.decode(
      <<10, 4, 73, 110, 102, 111, 18, 52, 10, 7, 111, 112, 116, 105, 111, 110, 115, 24, 1, 32, 1,
        40, 11, 50, 24, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 110, 115, 117, 109, 101,
        114, 46, 79, 112, 116, 105, 111, 110, 115, 72, 0, 82, 7, 111, 112, 116, 105, 111, 110,
        115, 18, 61, 10, 10, 65, 99, 107, 77, 101, 115, 115, 97, 103, 101, 24, 2, 32, 1, 40, 11,
        50, 27, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108, 46,
        65, 99, 107, 77, 101, 115, 115, 97, 103, 101, 72, 0, 82, 10, 65, 99, 107, 77, 101, 115,
        115, 97, 103, 101, 66, 6, 10, 4, 105, 110, 102, 111>>
    )
  end

  oneof(:info, 0)
  field(:options, 1, type: Astreu.Consumer.Options, oneof: 0)
  field(:AckMessage, 2, type: Astreu.Protocol.AckMessage, oneof: 0)
end

defmodule Astreu.Consumer.Subscriber.Service do
  @moduledoc false
  use GRPC.Service, name: "astreu.consumer.Subscriber"

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.ServiceDescriptorProto.decode(
      <<10, 10, 83, 117, 98, 115, 99, 114, 105, 98, 101, 114, 18, 69, 10, 9, 83, 117, 98, 115, 99,
        114, 105, 98, 101, 18, 21, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 110, 115, 117,
        109, 101, 114, 46, 73, 110, 102, 111, 26, 24, 46, 97, 115, 116, 114, 101, 117, 46, 112,
        114, 111, 116, 111, 99, 111, 108, 46, 77, 101, 115, 115, 97, 103, 101, 34, 3, 136, 2, 0,
        40, 1, 48, 1, 18, 69, 10, 11, 85, 110, 115, 117, 98, 115, 99, 114, 105, 98, 101, 18, 21,
        46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 110, 115, 117, 109, 101, 114, 46, 73, 110,
        102, 111, 26, 22, 46, 103, 111, 111, 103, 108, 101, 46, 112, 114, 111, 116, 111, 98, 117,
        102, 46, 69, 109, 112, 116, 121, 34, 3, 136, 2, 0, 40, 1, 48, 0>>
    )
  end

  rpc(:Subscribe, stream(Astreu.Consumer.Info), stream(Astreu.Protocol.Message))

  rpc(:Unsubscribe, stream(Astreu.Consumer.Info), Google.Protobuf.Empty)
end

defmodule Astreu.Consumer.Subscriber.Stub do
  @moduledoc false
  use GRPC.Stub, service: Astreu.Consumer.Subscriber.Service
end
