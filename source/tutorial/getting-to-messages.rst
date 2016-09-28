Getting to messaging
====================

What is Serializable
--------------------

Processes can send data if the type implements the |Serializable| typeclass,
which is done indirectly by implementing ``Binary`` and deriving ``Typeable``.
Implementations are already provided for primitives and some commonly used
data structures. As programmers, we see the messages in nice high-level form
(e.g., ``Int``, ``String``, ``Ping``, ``Pong``, etc), however these data have to be
encoded in order to be sent over a communications channel.

Not all types are |Serializable|, for example concurrency primitives such as
``MVar`` and ``TVar`` are meaningless outside the context of threads with a shared
memory. Cloud Haskell programs remain free to use these constructs within
processes or within processes on the same machine though. If you want to
pass data between processes using *ordinary* concurrency primitives such as
``STM`` then you're free to do so. Processes spawned locally can share
types such as ``TMVar`` just as normal Haskell threads would.

Message Ordering
----------------

We have already met the :api-dp:`send` primitive, used to deliver messages from
one process to another. Here's a review of what we've learned about
:api-dp:`send` thus far:

1. sending is asynchronous (i.e., it does not block the caller)
2. sending **never** fails, regardless of the state of the recipient process
3. even if a message is received, there is **no** guarantee **when** it will arrive
4. there are **no** guarantees that the message will be received at all

Asynchronous sending buys us several benefits. Improved concurrency is
possible, because processes need not block or wait for acknowledgements,
nor does error handling need to be implemented each time a message is sent.
Consider a stream of messages sent from one process to another. If the
stream consists of messages ``a``, ``b``, ``c`` and we have seen ``c``, then we know for
certain that we will have already seen ``a``, ``b`` (in that order), so long as the
messages were sent to us by the same peer process.

When two concurrent process exchange messages, Cloud Haskell guarantees that
messages will be delivered in FIFO order, if at all. No such guarantee exists
between $N$ processes where $N > 1$, so if processes *A* and *B* are both
communicating (concurrently) with process *C*, the ordering guarantee will
only hold for each pair of interactions, i.e., between *A* and *C* and/or
*B* and *C* the ordering will be guaranteed, but not between *A* and *B*
with regards messages sent to *C*.

Because the mailbox contains messages of varying types, when we :api-dp:`expect`
a message, we eschew the ordering because we're searching for a message
whose contents can be decoded to a specific type. Of course, we may **want**
to process messages in the precise order which they arrived. To achieve
this, we must defer the type checking that would normally cause a traversal
of the mailbox and extract the *raw* message ourselves. This can be achieved
using |receive| and :api-dp:`matchAny`, as we will demonstrate later.

Selective Receive
------------------

Processes dequeue messages (from their mailbox) using the |expect|
and |receive| family of primitives. Both take an optional timeout,
allowing the expression to evaluate to ``Nothing`` if no matching input
is found.

The |expect| primitive blocks until a message matching the expected type
(of the expression) is found in the process' mailbox. If a match is found by
scanning the mailbox, it is dequeued and returned, otherwise the caller
(i.e., the calling thread/process) is blocked until a message of the expected
type is delivered to the mailbox. Let's take a look at this in action:

.. code-block:: haskell

  demo :: Process ()
  demo = do
      listener <- spawnLocal listen
      send listener "hello"
      getSelfPid >>= send listener
      () <- expect
    where
      listen = do
        third <- expect :: Process ProcessId
        first <- expect :: Process String
        second <- expectTimeout 100000 :: Process String
        mapM_ (say . show) [first, second, third]
        send third ()

This program will print ``"hello"``, then ``Nothing`` and finally ``pid://...``.
The first |expect| - labelled "third" because of the order in which we
know it will arrive in our mailbox - **will** succeed, since the parent process
sends its |ProcessId| after the string "hello", yet the listener blocks until it
can dequeue the |ProcessId| before "expecting" a string. The second |expect|
(labelled "first") also succeeds, demonstrating that the listener has selectively
removed messages from its mailbox based on their type rather than the order in
which they arrived. The third |expect| will timeout and evaluate to ``Nothing``,
because only one string is ever sent to the listener and that has already been
removed from the mailbox. The removal of messages from the process' mailbox based
on type is what makes this program viable - without this "selective receiving",
the program would block and never complete.

By contrast, the |receive| family of primitives take a list of :api-dp:`Match`
objects, each derived from evaluating a |match| style primitive. This
subject was covered briefly in the first tutorial. Matching on messages allows
us to separate the type(s) of messages we can handle from the type that the
whole |receive| expression evaluates to.

Consider the following snippet:

.. code-block:: haskell

  usingReceive = do
    () <- receiveWait [
        match (\(s :: String) -> say s)
      , match (\(i :: Int)    -> say $ show i)
      ]

Note that each of the matches in the list must evaluate to the same type,
as the type signature indicates: ``receiveWait :: [Match b] -> Process b``.

The behaviour of :api-dp:`receiveWait` differs from :api-dp:`receiveTimeout`
in that it blocks forever (until a match is found in the process' mailbox),
whereas the variant taking a timeout will return ``Nothing`` unless a match is
found within the specified time interval. Note that as with ``System.Timeout``,
the only guarantee we have about a timeout based function is that it will not
expire *before* the given interval. Both functions scan the mailbox in FIFO
order, evaluating the list of |match| expressions in declarative
(i.e., insertion) order until one of the matches succeeds or the operation
times out.

Advanced Mailbox Processing
---------------------------

There are times when it is desirable to take a message from our mailbox without
explicitly specifying its type. Not only is this a useful capability, it is the
*only* way to process messages in the precise order they were received.

To see how this works in practise, let's consider the |relay| primitive that
ships with distributed-process. This utility function starts a process that
simply dequeues *any* messages it receives and forwards them to some other process.
In order to dequeue messages regardless of their type, this code relies on the
|matchAny| primitive, which has the following type:

.. code-block:: haskell

  matchAny :: forall b. (Message -> Process b) -> Match b

Since forwarding *raw messages* (without decoding them first) is a common pattern
in Cloud Haskell programs, there is also a primitive to do that for us:

.. code-block:: haskell

  forward :: Message -> ProcessId -> Process ()

Given these types, we can see that in order to combine |matchAny| with :api-dp:`forward`
we need to either ``flip forward`` and apply the |ProcessId| (leaving us with
the required type ``Message -> Process b``) or use a lambda - the actual implementation
does the latter and looks like this:

.. code-block:: haskell

  relay :: ProcessId -> Process ()
  relay !pid = forever' $ receiveWait [ matchAny (\m -> forward m pid) ]

This is pretty useful, but since |matchAny| operates on the raw |Message| type,
we're limited in what we can do with the messages we receive. In order to delve
*inside* a message, we have to know its type. If we have an expression that operates
on a specific type, we can *attempt* to decode the message to that type and examine
the result to see whether the decoding succeeds or not. There are two primitives
we can use to that effect: |unwrapMessage| and |handleMessage|. Their types look like
this:

.. code-block:: haskell

  unwrapMessage :: forall m a. (Monad m, Serializable a) => Message -> m (Maybe a)

  handleMessage :: forall m a b. (Monad m, Serializable a) => Message -> (a -> m b) -> m (Maybe b)

Of the two, |unwrapMessage| is the simpler, taking a raw |Message| and evaluating to
``Maybe a`` before returning that value in the monad ``m``. If the type of the raw |Message|
does not match our expectation, the result will be ``Nothing``, otherwise ``Just a``.

The approach |handleMessage| takes is a bit more flexible, taking a function
from ``a -> m b`` and returning ``Just b`` if the underlying message is of type
``a`` (hence the operation can be executed and evaluate to ``Maybe b``) or
``Nothing`` if the message's type is incompatible with the handler function.

Let's look at |handleMessage| in action. Earlier on we looked at |relay| from
distributed-process and now we'll consider its sibling |proxy| - this takes a predicate,
evaluates some input of type ``a`` and returns ``Process Bool``, allowing us to run arbitrary
|Process| code in order to decide whether or not the ``a`` is eligible to be forwarded to
the relay |ProcessId|. The type of |proxy| is thus:

.. code-block:: haskell

   proxy :: Serializable a => ProcessId -> (a -> Process Bool) -> Process ()

Since |matchAny| operates on ``(Message -> Process b)`` and |handleMessage| operates on
``a -> Process b`` we can compose these to make our proxy server. We must not forward 
messages for which the predicate function evaluates to ``Just False``, nor can we sensibly
forward messages which the predicate function is unable to evaluate due to type 
incompatibility. This leaves us with the definition found in distributed-process:

.. code-block:: haskell

  proxy pid proc = do
    receiveWait [
        matchAny (\m -> do
                     next <- handleMessage m proc
                     case next of
                       Just True  -> forward m pid
                       Just False -> return ()  -- explicitly ignored
                       Nothing    -> return ()) -- un-routable / cannot decode
      ]
    proxy pid proc

Beyond simple relays and proxies, the raw message handling capabilities available in
distributed-process can be utilised to develop highly generic message processing code.
All the richness of the distributed-process-platform APIs (such as :doc:`/managed-process`) which
will be discussed in later tutorials are, in fact, built upon these families of primitives.

Typed Channels
--------------

While being able to send and receive any |Serializable| datum is very
powerful, the burden of decoding types correctly at runtime is levied on the
programmer and there are runtime overheads to be aware of (which will be covered
in later tutorials). Fortunately, distributed-provides provides a type safe
alternative to |send| and |receive|, in the form of *Typed Channels*.
Channels work like a distributed equivalent of Haskell's
``Control.Concurrent.Chan``, however they have distinct ends: a single
receiving port and a corollary send port. Represented by distinct ends, a
``SendPort a`` (which is |Serializable|) and ``ReceivePort a`` (which is not),
channels are a lightweight and useful abstraction that provides a type safe
interface for interacting with processes separately from their primary mailbox.

Channels are created with ``newChan :: Process (SendPort a, ReceivePort a)``, with
messages sent via ``sendChan :: SendPort a -> a -> Process ()``. The
|ReceivePort| can be passed directly to :api-dp:`receiveChan`, or used in a
:api-dp:`receiveWait` or :api-dp:`receiveTimeout` call via the :api-dp:`matchChan`
primitive, so as to combine mailbox scans with channel reads.

.. code-block:: haskell

  channelsDemo :: Process ()
  channelsDemo = do
      (sp, rp) <- newChan :: Process (SendPort String, ReceivePort String)

      -- send on a channel
      spawnLocal $ sendChan sp "hello!"

      -- receive on a channel
      m <- receiveChan rp
      say $ show m

Channels are particularly useful when you are sending a message that needs a
response, because we know exactly where to look for the reply.

Channels can also allow message types to be simplified, as passing a
|ProcessId| for the reply isn't required. Channels aren't so useful when we
need to spawn a process and send a bunch a messages to it, then wait for
replies however; we can’t send a |ReceivePort| since it is not |Serializable|.

|ReceivePort|\s can be merged, so we can listen on several simultaneously. In
the latest version of :hackage-pkg:`distributed-process`, we can listen
for *regular* messages and multiple channels at the same time, using
:api-dp:`matchChan` in the list of allowed matches passed :api-dp:`receiveWait`
and :api-dp:`receiveTimeout`.

.. |receive| replace:: :distributed-process:`Control.Distributed.Process.send`
.. |send| replace:: :distributed-process:`Control.Distributed.Process.send`
.. |expect| replace:: :distributed-process:`Control.Distributed.Process.expect`
.. |match| replace:: :distributed-process:`Control.Distributed.Process.match`
.. |matchAny| replace:: :distributed-process:`Control.Distributed.Process.matchAny`
.. |relay| replace:: :distributed-process:`Control.Distributed.Process.relay`
.. |proxy| replace:: :distributed-process:`Control.Distributed.Process.proxy`
.. |ProcessId| replace:: :distributed-process:`Control.Distributed.Process.ProcessId`
.. |ReceivePort| replace:: :distributed-process:`Control.Distributed.Process.ReceivePort`
.. |Process| replace:: :distributed-process:`Control.Distributed.Process.Process`
.. |Message| replace:: :distributed-process:`Control.Distributed.Process.Message`
.. |handleMessage| replace:: :distributed-process:`Control.Distributed.Process.handleMessage`
.. |unwrapMessage| replace:: :distributed-process:`Control.Distributed.Process.unwrapMessage`
.. |Serializable| replace:: :distributed-process:`Control.Distributed.Process.Serializable.Serializable`

