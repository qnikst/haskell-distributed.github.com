.. Haskell Distributed documentation master file, created by
   sphinx-quickstart on Sun Sep 18 21:46:47 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Haskell Distributed's documentation!
===============================================

Cloud Haskell: Erlang-style concurrent and distributed programming in Haskell.
The Cloud Haskell Platform consists of a generic :hackage-pkg:`network transport API <network-transport>`,
:hackage-pkg:`libraries <distributed-static>` for sending static closures to remote nodes,
a rich :hackage-pkg:`API <distributed-process>` for distributed programming and a set of
Platform Libraries, modelled after Erlang’s Open Telecom Platform.

Generic network transport backends have been developed for TCP and in-memory messaging,
and several other implementations are available including a transport for Windows Azure.

Contents:

.. toctree::
   :maxdepth: 3

   overview


.. toctree::
   :caption: Tutorial
   :numbered:

   tutorial/getting-started
   tutorial/getting-to-know-processes
   tutorial/getting-to-messages
   tutorial/debugging
   managing-topologies

.. toctree::
   :caption: Details
   :numbered:

   architecture/network-transport

.. toctree::
   :caption: Cookbook
   :numbered:

   managed-process
   5ch
   6ch
   tutorial-NT2



Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

