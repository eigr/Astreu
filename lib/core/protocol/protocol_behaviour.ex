defmodule Astreu.Core.Protocol.ProtocolBehaviour do
  require Logger
  alias GRPC.Server
  alias Astreu.Core.Protocol.Topic.SubscriberManager
  alias Astreu.Core.Protocol.Producer.Dispatcher

  @spec ensure_metadata(Astreu.Protocol.Message.t()) ::
          {:ok, Astreu.Protocol.Message.t()} | {:error, String.t()}
  def ensure_metadata(
        %{stream: stream, message: message, consumer: consumer, producer: producer} = params
      ) do
    case message.data do
      {:system, _} -> ensure_system(message)
      {:exchange, _} -> ensure_exchange(message)
      {:ack, _} -> ensure_ack(message)
      _ -> {:error, "Invalid message: #{inspect(message)}"}
    end
  end

  def handle(%{stream: stream, message: message, consumer: consumer, producer: producer} = params) do
    case message.data do
      {:system, _} -> handle_system(params)
      {:exchange, _} -> handle_exchange(params)
      {:ack, _} -> handle_ack(params)
      _ -> handle_invalid(params)
    end
  end

  defp handle_system(
         %{stream: stream, message: message, consumer: consumer, producer: producer} = params
       ) do
    system = elem(message.data, 1)

    case system.data do
      {:connect, _} -> handle_connect(system.data, params)
      {:disconnect, _} -> handle_disconnect(system.data, params)
      {:info, _} -> handle_info(system.data, params)
      {:failure, _} -> handle_failure(system.data, params)
      _ -> handle_invalid(params)
    end
  end

  defp handle_connect(
         data,
         %{stream: stream, message: message, consumer: consumer, producer: producer} = params
       ) do
    # TODO lookup before register new process
    Logger.debug("Connect subscriber #{inspect(data)}")
    connection = elem(data, 1)

    name =
      if consumer do
        "#{connection.topic}:#{connection.subscription}"
      else
        "replies.#{connection.topic}.#{connection.uuid}:#{connection.uuid}"
      end

    topic_name =
      if consumer do
        connection.topic
      else
        "replies.#{connection.topic}.#{connection.uuid}"
      end

    subscription =
      if consumer do
        connection.subscription
      else
        connection.uuid
      end

    state = %{
      name: name,
      stream: stream,
      topic: topic_name,
      uuid: connection.uuid,
      timestamp: connection.timestamp,
      properties: connection.properties,
      subscriber: subscription
    }

    Horde.DynamicSupervisor.start_child(
      Astreu.SubscriberSupervisor,
      {Astreu.Core.Protocol.Topic.SubscriberManager, state}
    )

    SubscriberManager.subscribe(state[:name])
  end

  def handle_invalid(
        %{stream: stream, message: message, consumer: consumer, producer: producer} = params
      ) do
    Server.send_reply(
      stream,
      Astreu.Protocol.Message.new(data: {:exchange, Astreu.Protocol.Exchange.new()})
    )
  end

  def handle_invalid(
        reason,
        %{stream: stream, message: message, consumer: consumer, producer: producer} = params
      ) do
    Server.send_reply(
      stream,
      Astreu.Protocol.Message.new(data: {:exchange, Astreu.Protocol.Exchange.new()})
    )
  end

  defp handle_exchange(
         %{stream: stream, message: message, consumer: consumer, producer: producer} = params
       ) do
    msg = elem(message.data, 1)

    if producer do
      Dispatcher.dispatch(msg.metadata.topic, message)
      # case Dispatcher.dispatch(msg.metadata.topic, msg) do
      # ACK with success
      #  :ok -> Server.send_reply(stream, Astreu.Protocol.Message.new())
      # ACK without success
      # _ -> Server.send_reply(stream, Astreu.Protocol.Message.new())
      # end
    end

    if consumer do
      topic = "replies.#{msg.metadata.topic}.#{msg.metadata.producerId}"
      Dispatcher.dispatch(topic, message)

      # case Dispatcher.dispatch(topic, msg) do
      # ACK with success
      #  :ok -> Server.send_reply(stream, Astreu.Protocol.Message.new())
      # ACK without success
      #  _ -> Server.send_reply(stream, Astreu.Protocol.Message.new())
      # end
    end

    params
  end

  defp handle_ack(
         %{stream: stream, message: message, consumer: consumer, producer: producer} = params
       ) do
    msg = elem(message.data, 1)

    if consumer do
      topic = "replies.#{msg.metadata.topic}.#{msg.metadata.producerId}"
      Logger.debug("Send Ack response to topic #{topic}")
      Dispatcher.dispatch(topic, message)
    end

    params
  end

  # Validations
  defp ensure_system(message) do
    {:ok, message}
  end

  defp ensure_exchange(message) do
    {:ok, message}
  end

  defp ensure_ack(message) do
    {:ok, message}
  end

  defp handle_disconnect(data, params) do
    data
  end

  defp handle_info(data, params) do
    data
  end

  defp handle_failure(data, params) do
    data
  end
end
