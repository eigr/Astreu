defmodule Astreu.Core.Protocol.Consumer.Subscriber.Service do
  @moduledoc false
  use GRPC.Service, name: "astreu.core.protocol.consumer.Subscriber"

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.ServiceDescriptorProto.decode(
      <<10, 10, 83, 117, 98, 115, 99, 114, 105, 98, 101, 114, 18, 82, 10, 9, 83, 117, 98, 115, 99,
        114, 105, 98, 101, 18, 29, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46,
        112, 114, 111, 116, 111, 99, 111, 108, 46, 77, 101, 115, 115, 97, 103, 101, 26, 29, 46,
        97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112, 114, 111, 116, 111, 99, 111,
        108, 46, 77, 101, 115, 115, 97, 103, 101, 34, 3, 136, 2, 0, 40, 1, 48, 1, 18, 77, 10, 11,
        85, 110, 115, 117, 98, 115, 99, 114, 105, 98, 101, 18, 29, 46, 97, 115, 116, 114, 101,
        117, 46, 99, 111, 114, 101, 46, 112, 114, 111, 116, 111, 99, 111, 108, 46, 77, 101, 115,
        115, 97, 103, 101, 26, 22, 46, 103, 111, 111, 103, 108, 101, 46, 112, 114, 111, 116, 111,
        98, 117, 102, 46, 69, 109, 112, 116, 121, 34, 3, 136, 2, 0, 40, 0, 48, 0>>
    )
  end

  rpc(:Subscribe, stream(Astreu.Core.Protocol.Message), stream(Astreu.Core.Protocol.Message))

  rpc(:Unsubscribe, Astreu.Core.Protocol.Message, Google.Protobuf.Empty)
end

defmodule Astreu.Core.Protocol.Consumer.Subscriber.Stub do
  @moduledoc false
  use GRPC.Stub, service: Astreu.Core.Protocol.Consumer.Subscriber.Service
end
