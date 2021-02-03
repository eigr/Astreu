defmodule Astreu.Consumer.Subscriber.Service do
  @moduledoc false
  use GRPC.Service, name: "astreu.consumer.Subscriber"

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.ServiceDescriptorProto.decode(
      <<10, 10, 83, 117, 98, 115, 99, 114, 105, 98, 101, 114, 18, 72, 10, 9, 83, 117, 98, 115, 99,
        114, 105, 98, 101, 18, 24, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111,
        99, 111, 108, 46, 80, 97, 121, 108, 111, 97, 100, 26, 24, 46, 97, 115, 116, 114, 101, 117,
        46, 112, 114, 111, 116, 111, 99, 111, 108, 46, 77, 101, 115, 115, 97, 103, 101, 34, 3,
        136, 2, 0, 40, 1, 48, 1, 18, 72, 10, 11, 85, 110, 115, 117, 98, 115, 99, 114, 105, 98,
        101, 18, 24, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111, 108,
        46, 80, 97, 121, 108, 111, 97, 100, 26, 22, 46, 103, 111, 111, 103, 108, 101, 46, 112,
        114, 111, 116, 111, 98, 117, 102, 46, 69, 109, 112, 116, 121, 34, 3, 136, 2, 0, 40, 0, 48,
        0>>
    )
  end

  rpc(:Subscribe, stream(Astreu.Protocol.Payload), stream(Astreu.Protocol.Message))

  rpc(:Unsubscribe, Astreu.Protocol.Payload, Google.Protobuf.Empty)
end

defmodule Astreu.Consumer.Subscriber.Stub do
  @moduledoc false
  use GRPC.Stub, service: Astreu.Consumer.Subscriber.Service
end
