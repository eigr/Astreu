# Astreu

**High-performance Messaging System based on gRPC protocol write on Elixir** (***this is a WIP no production ready***)

![Astreu CI](https://github.com/eigr/Astreu/workflows/Astreu%20CI/badge.svg) [![Astreu Release](https://github.com/eigr/Astreu/actions/workflows/release.yml/badge.svg)](https://github.com/eigr/Astreu/actions/workflows/release.yml)

## Architecture Overview

```

                                     +-------------------------------+
                                     |           Astreu              |
                                     |                               |
                                     | +---------------------------+ |
    +-------------+                  | |     Management API        | |                   +-------------+
    | Subscribers |                  | +---------------------------+ |                   |  Producers  |
  +-------------+ |  Bi-directional  | +---------------------------+ |  Bi-directional   | +-------------+
+---------------| +----------------->+ |     PubSub Adapters       | +------------------>+ | +-------------+
| | |          || |    Streams       | +---------------------------+ |    Streams        | | |         | | |
| | |          || +<-----------------+ +---------------------------+ +<------------------+ | |         | | |
| | +-------------+                  | |      Core Protocol        | |                   +-------------+ | |
+-+-------------+                    | +---------------------------+ |                     +-------------+ |
                                     | +---------------------------+ |                       +-------------+
                                     | |        gRpc Server        | |
                                     | +---------------------------+ |
                                     +-------------------------------+


```

## Usage and Installation

* Run:

```
# docker run --rm --net=host -e RELEASE_NODE=unique_name_peer_node eigr/astreu:0.1.2
```

* Connect as a Producer to the server from an SDK (Java SDK example below):

```java
public static void main(final String[] args) {

    final Producer producer =
            Astreu.at("127.0.0.1", 9980)
            .asPub("test", UUID.randomUUID().toString().toLowerCase());

    final Publisher<ReplyMessage> publisher = producer.bind(); 

    Flux.from(publisher).subscribe(replyMessage -> {
        replyMessage.logger().info("Reply Message -> {}", replyMessage);
    });

    IntStream.range(0, 10).parallel().forEach(i -> {
        producer.publish(
                String.valueOf(i), 
                Any.newBuilder()
                        .setTypeUrl("io.astreu.custom/Text")
                        .setValue(ByteString.copyFrom(String.format("Hello World Astreu %s", i).getBytes()))
                        .build()
        );
    });
}
```

* Connect as a Subscriber to the server from an SDK (Java SDK example below):

```java
public static void main(final String[] args) {
    final Publisher<MessageWithContext> publisher =
            Astreu.at("127.0.0.1", 9980)
                    .asSub("test", "unique-subscription")
                    .receiveOnly(MessageType.EXCHANGE)
                    .bind(); 

    Flux.from(publisher).subscribe(messageWithContext -> {
        final AcknowledgeContext context = messageWithContext.getContext();

        context.logger().debug("Message type is -> {}", messageWithContext.getType());
        final Exchange message = messageWithContext.getMessage();

        context.logger().info("Incoming Message {}", message);
        context.accept();
    });
}
```

## Client SDK's

* [Astreu4j](https://github.com/eigr/astreu4j)
* [GO Lang Astreu](https://github.com/eigr/astreu-go)
