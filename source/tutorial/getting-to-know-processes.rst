.. default-role:: distributed-process

Getting to know Processes
=========================

Concurrency and Distribution
----------------------------

The *Process Layer* is where Cloud Haskell's support for concurrency and
distributed programming are exposed to application developers. This layer
deals explicitly with



The core of Cloud Haskell's concurrency and distribution support resides in the
:hackage-pkg:`distributed-process` library. As well as the APIs necessary for
starting nodes and forking processes on them, we find all the basic primitives
required to

* spawn processes locally and remotely
* send and receive messages, optionally using typed channels
* monitor and/or link to processes, channels and other nodes

Most of this is easy enough to follow in the haddock documentation and the
various tutorials. Here we focus on the essential *concepts* behind the
process layer.


A concurrent process is somewhat like a Haskell thread - in fact it is a
|forkIO| thread - but with few differences.

  1. Process run `Process` code instead of ``IO``. This allow process to use
     communication function specific to the distributed-process.
  2. In addition to `ThreadId` each process have it's own `ProcessId`
     that can be used as a label then sending messages both locally and remotely.
  3. `Process` lifetime is bound to the lifetime of the `Node` it's running
     on. When node will be closed, all processes that are running on that node
     are also closed.
  4. *Node Controller* bookkeep information about thread lifetime, and notify
     interested processes in `Process` exit and *the exit reason*.
  5. Each `Process` have mailbox associated with that `Process` that can be
     used to read incomming messages.


Process that can send and receive messages through its
*process mailbox*. Each process can send messages asynchronously to other
processes, and can receive messages synchronously from its own mailbox.
The conceptual difference between threads and processes is that the latter
do not share state, but communicate only via message passing.

Code that is executed in this manner must run in the `Process` monad. Our
process will look like any other monad code, plus we provide and instance
of ``MonadIO`` for `Process`, so you can ``liftIO`` to make IO actions
available.

Processes reside on nodes, which in our implementation map directly to the
:dp:`Control.Distributed.Processes.Node.` module. Given a configured
:n-t:`Network.Transport.` backend, starting a new node is fairly simple:

.. code-block:: haskell

  newLocalNode :: Transport -> IO LocalNode

Once this function returns, the node will be *up and running* and able to
interact with other nodes and host processes. It is possible to start more
than one node in the same running program, though if you do this they will
continue to send messages to one another using the supplied
:n-t:`Network.Transport` backend.

Given a new node, there are two primitives for starting a new process.

.. code-block:: haskell

  forkProcess :: LocalNode -> Process () -> IO ProcessId
  runProcess  :: LocalNode -> Process () -> IO ()

Once we've spawned some processes, they can communicate with one another
using the messaging primitives provided by [distributed-processes][distributed-processes],
which are well documented in the haddocks.


Process Lifetime
----------------

A process will continue executing until it has evaluated to some value, or is abruptly
terminated either by crashing (with an un-handled exception) or being instructed to
stop executing. Deliberate stop instructions take one of two forms: a `ProcessExitException`
or `ProcessKillException`. As the names suggest, these *signals* are delivered in the form
of asynchronous exceptions, however you should not to rely on that fact! After all,
we cannot throw an exception to a thread that is executing in some other operating
system process or on a remote host! Instead, you should use the `exit` and `kill`
primitives from :hackage-pkg:`distributed-process`, which not only ensure that remote target processes
are handled seamlessly, but also maintain a guarantee that if you send a message and
*then* an exit signal, the message will be delivered to the destination process (via its
local node controller) before the exception is thrown - note that this does not guarantee
that the destination process will have time to *do anything* with the message before it
is terminated.

The `ProcessExitException` signal is sent from one process to another, indicating that the
receiver is being asked to terminate. A process can choose to tell itself to exit, and the
`die` primitive simplifies doing so without worrying about the expected type for the
action. In fact, `die` has slightly different semantics from `exit`, since the latter involves
sending an internal signal to the local node controller. A direct consequence
of this is that the *exit signal* may not arrive immediately, since the *Node Controller* could
be busy processing other events. On the other hand, the `die` primitive throws a
`ProcessExitException` directly in the calling thread, thus terminating it without delay.
In practise, this means the following two functions could behave quite differently at
runtime:


.. code-block:: haskell

  -- this will never print anything...
  demo1 = die "Boom" >> expect >>= say

  -- this /might/ print something before it exits
  demo2 = do
    self <- getSelfPid
    exit self "Boom"
    expect >>= say

The `ProcessExitException` type holds a *reason* field, which is serialised as a raw `Message`.
This exception type is exported, so it is possible to catch these *exit signals* and decide how
to respond to them. Catching *exit signals* is done via a set of primitives in
distributed-process, and the use of them forms a key component of the various fault tolerance
strategies provided by distributed-process-platform.

A `ProcessKillException` is intended to be an **untrappable** exit signal, so its type is
not exported and therefore you can **only** handle it by catching all exceptions, which
as we all know is very bad practise. The `kill` primitive is intended to be a
**brutal** means for terminating process - e.g., it is used to terminate supervised child
processes that haven't shutdown on request, or to terminate processes that don't require
any special cleanup code to run when exiting - although it does behave like `exit`
in so much as it is dispatched (to the target process) via the *Node Controller*.


.. TODO

   notes about handling normal exception

Exit signals in Cloud Haskell then, are unlike asynchronous exceptions in other
haskell code. Whilst a process *can* use asynchronous exceptions - there's
nothing stoping this since the `Process` monad is an instance of ``MonadIO`` -
as we've seen, exceptions thrown are not bound by the same ordering guarantees
as messages delivered to a process. Link failures and exit signals **might** work
via asynchronous exceptions - that is the case in the current implementation - but
these are implemented in such a fashion that if you send a message and **then** an
exit signal, the message is guaranteed to arrive first.

Because processes are implemented with |forkIO| we might be tempted to stop
them by throwing an asynchronous exception to the process, but this is almost
certainly the wrong thing to do. Firstly, processes might reside on a remote
node, in which case throwing an exception is impossible. Secondly, if we send
some messages to a process' mailbox and then dispatch an exception to kill it,
there is no guarantee that the subject will receive our message before being
terminated by the asynchronous exception.

Monitoring and Linking
----------------------

Processes can be linked to other processes (or nodes or channels). A link, which is
unidirectional, guarantees that once any object we have linked to *exits*, we will also
be terminated. A simple way to test this is to spawn a child process, link to it and then
terminate it, noting that we will subsequently die ourselves. Here's a simple example,
in which we link to a child process and then cause it to terminate (by sending it a message
of the type it is waiting for). Even though the child terminates "normally", our process
is also terminated since `link` will *link the lifetime of two processes together*
regardless of exit reasons.

.. code-block:: haskell

  demo = do
    pid <- spawnLocal $ receive >>= return
    link pid
    send pid ()
    () <- receive

The medium that link failures uses to signal exit conditions is the same as exit and kill
signals - asynchronous exceptions. Once again, it is a bad idea to rely on this (not least
because it might change in some future release) and the exception type (`ProcessLinkException`)
is not exported so as to prevent developers from abusing exception handling code in this
special case. Since link exit signals cannot be caught directly, if you find yourself wanting
to **trap** a link failure, you probably want to use a monitor instead.

Whilst the built-in `link` primitive terminates the link-ee regardless of exit reason,
:hackage-pkg:`distributed-process-extras` provides an alternate function
:dp-extras:`Control.Distributed.Process.Extras.linkOnFailure`,
which only dispatches the `ProcessLinkException` if the link-ed process dies abnormally
(i.e., with some `DiedReason` other than `v:DiedNormal`).


.. TODO:: describe monitors better and move ``linkOnFailure`` to the cookbook.

Monitors on the other hand, do not cause the **listening** process to exit at all, instead
putting a `ProcessMonitorNotification` into the process' mailbox. This signal and its
constituent fields can be introspected in order to decide what action (if any) the receiver
can/should take in response to the monitored process' death. Let's take a look at how
monitors can be used to determine both when and *how* a process has terminated. Tucked
away in :hackage-pkg:`distributed-process-extras`, the
:dp-extras:`Control.Distributed.Process.Extras.linkOnFailure`, primitive works in exactly this
way, only terminating the caller if the subject terminates abnormally. Let's take a look...

.. code-block:: haskell

  linkOnFailure them = do
    us <- getSelfPid
    tid <- liftIO $ myThreadId
    void $ spawnLocal $ do
      callerRef <- P.monitor us
      calleeRef <- P.monitor them
      reason <- receiveWait [
               matchIf (\(ProcessMonitorNotification mRef _ _) ->
                         mRef == callerRef) -- nothing left to do
                       (\_ -> return DiedNormal)
             , matchIf (\(ProcessMonitorNotification mRef' _ _) ->
                         mRef' == calleeRef)
                       (\(ProcessMonitorNotification _ _ r') -> return r')
           ]
      case reason of
        DiedNormal -> return ()
        _ -> liftIO $ throwTo tid (ProcessLinkException us reason)

As we can see, this code makes use of monitors to track both processes involved in the
link. In order to track *both* processes and react to changes in their status, it is
necessary to spawn a third process which will do the monitoring. This doesn't happen
with the built-in link primitive, but is necessary in this case since the link handling
code resides outside the *Node Controller*.

The two matches passed to `receiveWait` both handle a `ProcessMonitorNotification`, and
the predicate passed to `matchIf` is used to determine whether the notification we're
receiving is for the process that called us, or the *linked to* process. If the former
dies, we've nothing more to do, since links are unidirectional. If the latter dies
however, we must examine the `DiedReason` the `ProcessMonitorNotification` provides us
with, to determine whether the subject exited normally (i.e., with `v:DiedNormal`).
If the exit was *abnormal*, we throw a `ProcessLinkException` to the original caller,
which is exactly how an ordinary link would behave.

Linking and monitoring are foundational tools for **supervising** processes, where a top level
process manages a set of children, starting, stopping and restarting them as necessary.

Getting Process Info
--------------------

The `getProcessInfo` function provides a means for us to obtain information about a running
process. The `ProcessInfo` type it returns contains the local node id and a list of
registered names, monitors and links for the process. The call returns ``Nothing`` if the
process in question is not alive.

In order to get `ProcessId` and `NodeId` of the currently running process one
could `getProcessId` and `getNodeId` respectively.

Monad Transformer Stacks
------------------------

It is not generally necessary, but it may be convenient in your application to use a
custom monad transformer stack with the Process monad at the bottom. For example,
you may have decided that in various places in your application you will make calls to
a network database. You may create a data access module, and it will need configuration information available to it in
order to connect to the database server. A ReaderT can be a nice way to make
configuration data available throughout an application without
schlepping it around by hand.

This example is a bit contrived and over-simplified but
illustrates the concept. Consider the ``fetchUser`` function below, it runs in the
``AppProcess`` monad which provides the configuration settings required to connect
to the database::

  import Data.ByteString (ByteString)
  import Control.Monad.Reader

  -- imagine we have some database library
  import Database.Imaginary as DB

  data AppConfig = AppConfig {dbHost :: String, dbUser :: String}

  type AppProcess = ReaderT AppConfig Process

  data User = User {userEmail :: String}

  -- Perform a user lookup using our custom app context
  fetchUser :: String -> AppProcess (Maybe User)
  fetchUser email = do
    db <- openDB
    user <- liftIO $ DB.query db email
    closeDB db
    return user

  openDB :: AppProcess DB.Connection
  openDB = do
    AppConfig host user <- ask
    liftIO $ DB.connect host user

  closeDB :: DB.Connection -> AppProcess ()
  closeDB db = liftIO (DB.close db)

So this would mostly work but it is not complete. What happens if an exception
is thrown by the `query` function? Your open database handle may not be
closed. Typically we manage this with the |bracket| function.

In the base library, |bracket| is defined in :base:`Control.Exception.` with
this signature::

  bracket :: IO a        -- ^ computation to run first ("acquire resource")
          -> (a -> IO b) -- ^ computation to run last ("release resource")
          -> (a -> IO c) -- ^ computation to run in-between
          -> IO c

Great! We pass an IO action that acquires a resource; |bracket| passes that
resource to a function which takes the resource and runs another action.
We also provide a release function which |bracket| is guaranteed to run
even if the primary action raises an exception.


Unfortunately, we cannot directly use |bracket| in our
``fetchUser`` function: ``openDB`` (resource acquisition) runs in the ``AppProcess``
monad. If our functions ran in IO, we could lift the entire bracket computation into
our monad transformer stack with liftIO; but we cannot do that for the computations
*passed* to bracket.

It is perfectly possible to write our own bracket; :hackage-pkg:`distributed-process` does this
for the `Process` monad (which is itself a newtyped ReaderT stack).
Here is how that is done::

  -- | Lift 'Control.Exception.bracket'
  bracket :: Process a -> (a -> Process b) -> (a -> Process c) -> Process c
  bracket before after thing =
    mask $ \restore -> do
      a <- before
      r <- restore (thing a) `onException` after a
      _ <- after a
      return r

  mask :: ((forall a. Process a -> Process a) -> Process b) -> Process b
  mask p = do
      lproc <- ask
      liftIO $ Ex.mask $ \restore ->
        runLocalProcess lproc (p (liftRestore restore))
    where
      liftRestore :: (forall a. IO a -> IO a)
                  -> (forall a. Process a -> Process a)
      liftRestore restoreIO = \p2 -> do
        ourLocalProc <- ask
        liftIO $ restoreIO $ runLocalProcess ourLocalProc p2

  -- | Lift 'Control.Exception.onException'
  onException :: Process a -> Process b -> Process a
  onException p what = p `catch` \e -> do _ <- what
                                          liftIO $ throwIO (e :: SomeException)

:hackage-pkg:`distributed-process` needs to do this sort of thing to keep its dependency
list small, but do we really want to write this for every transformer stack
we use in our own applications? No! And we do not have to, thanks to
the :hackage-pkg:`monad-control` and :hackage-pkg:`lifted-base` libraries.

:hackage-pkg:`monad-control` provides several typeclasses and helper functions
that make it possible to fully generalize the wrapping/unwrapping required
to keep transformer effects stashed away while actions run in the base monad. Of
most concern to end users of this library are the typeclass ``MonadBase`` and
``MonadBaseControl``.

How it works is beyond the scope of this tutorial, but there is an excellent and thorough
explanation written by Michael Snoyman which is available [here][mctrlt].

:hackage-pkg:`lifted-base` takes advantage of these typeclasses to provide
lifted versions of many functions in the Haskell base library. For example,
:lifted-base:`Control.Exception.Lifted.bracket` has a definition of bracket that
looks like this::

  bracket :: MonadBaseControl IO m
          => m a         -- ^ computation to run first ("acquire resource")
          -> (a -> m b)  -- ^ computation to run last ("release resource")
          -> (a -> m c)  -- ^ computation to run in-between
          -> m c

It is just the same as the version found in base, except it is generalized to work
with actions in any monad that implements ``MonadBaseControl IO``.
:hackage-pkg:`monad-control` defines instances for the standard transformers,
but that instance requires the base monad (in this case, `Process`) to also have an
instance of these classes.

To address this the [distributed-process-monad-control][dpmc] package
provides orphan instances of the `Process` type for both ``MonadBase IO`` and
``MonadBaseControl IO``.
After importing these, we can rewrite our ``fetchUser`` function to use the
instance of bracket provided by :hackage-pkg:`lifted-base`.


.. code-block:: haskell

  -- ...
  import Control.Monad.Catch as Catch

  -- ...

  fetchUser :: String -> AppProcess (Maybe User)
  fetchUser email =
    Catch.bracket openDB
                  closeDB
            $ \db -> liftIO $ DB.query db email

:hackage-pkg:`lifted-base` also provides conveniences like ``MVar`` and other
concurrency primitives that operate in ``MonadBase IO``. One benefit here is
that your code is not sprinkled with liftIO; but ``MonadBaseControl IO`` also
makes things like a lifted ``withMVar`` possible - which is really just a
specialization of |bracket|. You will also find lots of other libraries on
hackage which use these instances - at present count there are more than 150
`packages using it <http://packdeps.haskellers.com/reverse/lifted-base>`__.

.. warning::

   This instance can enable use of functions such as |forkIO| (or, 
   :lifted-base:`Control.Concurrent.Lifted.fork` from :hackage-pkg:`lifted-base`)
   which compromise invariants in the `Process` monad and can lead to confusing
   and subtle issues. Always use the *Cloud Haskell* functions such as
   `spawnLocal` instead.

.. |bracket| replace:: :base:`Control.Exception.bracket`
.. |forkIO| replace:: :base:`Control.Concurrent.forkIO`
,. |fork| replace:: :lifted-base:`Control.Concurrent.Lifted.fork`
