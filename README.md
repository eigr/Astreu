# Astreu

**High-performance Messaging System basead on gRPC protocol**

## Architecture Overview

```

                                     +-------------------------------+
                                     |    Astreu Message Broker      |
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

```
# docker run --rm --net=host -e RELEASE_NODE=unique_name_peer_node eigr/astreu:0.1.0
```

