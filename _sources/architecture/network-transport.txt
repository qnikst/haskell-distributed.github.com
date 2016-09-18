Network Transport Abstraction Layer
===================================

Cloud Haskell's generic [network-transport][network-transport] API is entirely independent of
the concurrency and messaging passing capabilities of the *process layer*.
Cloud Haskell applications are built using the primitives provided by the
*process layer* (i.e., [distributed-process][distributed-process]), which provides abstractions
such as nodes and processes. Applications must also depend on a Cloud Haskell
backend, which provides functions to allow the initialisation of the transport
layer using whatever topology might be appropriate to the application.

`Network.Transport` is a network abstraction layer geared towards specific
classes of applications, offering the following high level concepts:

* Nodes in the network are represented by `EndPoint`\s. These are heavyweight stateful objects.
* Each `EndPoint` has an `EndPointAddress`.
* Connections can be established from one `EndPoint` to another using the `EndPointAddress` of the remote end.
* The `EndPointAddress` can be serialised and sent over the network, whereas `EndPoint`s and connections cannot.
* Connections between `EndPoint`s are unidirectional and lightweight.
* Outgoing messages are sent via a `Connection` object that represents the sending end of the connection.
* Incoming messages for **all** of the incoming connections on an `EndPoint` are collected via a shared receive queue.
* In addition to incoming messages, `EndPoint`\s are notified of other `Event`\s such as new connections or broken connections.

This design was heavily influenced by the design of the Common Communication Interface `[CCI] <http://www.olcf.ornl.gov/center-projects/common-communication-interface/>`. Important design goals are:

* Connections should be lightweight: it should be no problem to create thousands of connections between endpoints.
* Error handling is explicit: every function declares as part of its type which errors it can return (no exceptions are thrown)
* Error handling is "abstract": errors that originate from implementation specific problems (such as "no more sockets" in the TCP implementation) get mapped to generic errors ("insufficient resources") at the Transport level.

For the purposes of most Cloud Haskell applications, it is sufficient to know
enough about the ``Network.Transport`` API to instantiate a backend with the
required configuration and pass the returned opaque handle to the ``Node`` API
in order to establish a new, connected, running node. More involved setups are,
of course, possible; The simplest use of the API is thus

.. code-block:: haskell

  main :: IO
  main = do
    Right transport <- createTransport "127.0.0.1" "10080" defaultTCPParameters
    node1 <- newLocalNode transport initRemoteTable

Here we can see that the application depends explicitly on the
`defaultTCPParameters` and `createTransport` functions from
`Network.Transport.TCP`, but little else. The application *can* make use
of other `Network.Transport` APIs if required, but for the most part this
is irrelevant and the application will interact with Cloud Haskell through
the *Process Layer* and *Platform*.

For more details about `Network.Transport` please see the [wiki page](/wiki/networktransport.html).
