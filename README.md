# Astreu

**High-performance Messaging System based on gRPC protocol** (***this is a WIP no production ready***)

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

```
# docker run --rm --net=host -e RELEASE_NODE=unique_name_peer_node eigr/astreu:0.1.2
```

## Client SDK's

* [Astreu4j](https://github.com/eigr/astreu4j)
* [GO Lang Astreu](https://github.com/eigr/astreu-go)