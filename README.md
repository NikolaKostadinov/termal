# **TPLS**: Thermal Process Lattice Simulator

## The Idea:

The **TPLS** is an physical simulator runing on top of Erlang. It is based on discretizing the **heat equation**:

$$ \frac{\partial T}{\partial t} = \alpha \nabla^2 T $$

The **TPLS** uses a **lattice** of erlang **processes** (*shortly: nodes*) which exchange messages. All nodes are supervized by *"Big Brother"* who prevents premature crashes. 

## The Scheme:

In the schemes every node is represented by a circle. The lines are logical connections. This is how the lattice looks like:

<img src="./assets/lattice.svg">

Every node knows its neighbors.

<img src="./assets/node.svg">

## The Setup

The **TPLS** is build for Linux. It can be used on other OS, but you have to configure it manualy. Firstly, pull the source code. Then run the ``compiler.sh`` script:

```console
$ chmod +x ./compiler.sh
$ ./compiler.sh
```

This script compiles every erlang module in the repository. Now, you can open the erlang shell with the ``erl`` command and use the ``node`` and ``bigbrother`` modules.
