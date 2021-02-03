defmodule Astreu.Producer.Publisher.Service do
  @moduledoc false
  use GRPC.Service, name: "astreu.producer.Publisher"

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.ServiceDescriptorProto.decode(
      <<10, 9, 80, 117, 98, 108, 105, 115, 104, 101, 114, 18, 70, 10, 7, 80, 117, 98, 108, 105,
        115, 104, 18, 24, 46, 97, 115, 116, 114, 101, 117, 46, 112, 114, 111, 116, 111, 99, 111,
        108, 46, 80, 97, 121, 108, 111, 97, 100, 26, 24, 46, 97, 115, 116, 114, 101, 117, 46, 112,
        114, 111, 116, 111, 99, 111, 108, 46, 80, 97, 121, 108, 111, 97, 100, 34, 3, 136, 2, 0,
        40, 1, 48, 1>>
    )
  end

  rpc(:Publish, stream(Astreu.Protocol.Payload), stream(Astreu.Protocol.Payload))
end

defmodule Astreu.Producer.Publisher.Stub do
  @moduledoc false
  use GRPC.Stub, service: Astreu.Producer.Publisher.Service
end
