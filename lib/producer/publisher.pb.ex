defmodule Astreu.Core.Protocol.Producer.Publisher.Service do
  @moduledoc false
  use GRPC.Service, name: "astreu.core.protocol.producer.Publisher"

  def descriptor do
    # credo:disable-for-next-line
    Elixir.Google.Protobuf.ServiceDescriptorProto.decode(
      <<10, 9, 80, 117, 98, 108, 105, 115, 104, 101, 114, 18, 80, 10, 7, 80, 117, 98, 108, 105,
        115, 104, 18, 29, 46, 97, 115, 116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112, 114,
        111, 116, 111, 99, 111, 108, 46, 77, 101, 115, 115, 97, 103, 101, 26, 29, 46, 97, 115,
        116, 114, 101, 117, 46, 99, 111, 114, 101, 46, 112, 114, 111, 116, 111, 99, 111, 108, 46,
        77, 101, 115, 115, 97, 103, 101, 34, 3, 136, 2, 0, 40, 1, 48, 1>>
    )
  end

  rpc :Publish, stream(Astreu.Core.Protocol.Message), stream(Astreu.Core.Protocol.Message)
end

defmodule Astreu.Core.Protocol.Producer.Publisher.Stub do
  @moduledoc false
  use GRPC.Stub, service: Astreu.Core.Protocol.Producer.Publisher.Service
end
