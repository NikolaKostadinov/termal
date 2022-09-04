# **TPLS**: Thermal Process Lattice Simulator

## The Idea:

The **TPLS** is an physical simulator runing on top of Erlang. It is based on discretizing the **heat equation**:

$$ \frac{\partial T}{\partial t} = \alpha \nabla^2 T $$

The **TPLS** uses a **lattice** of erlang **processes** (*shortly: nodes*) which exchange messages. All nodes are supervized by *"Big Brother"* who prevents premature crashes. 

## The Scheme:

In the schemes every node is represented by a circle. The lines are logical connections. This is how the lattice looks like:

<img src="./assets/lattice.svg">

Every node knows its temperature and neighbors:

<img src="./assets/node.svg">

For time evolution, the node has to request the temperatures of its neighbors, the thermal diffusivity of the supervisor, and calcutate the next temperature via the **heat equation**:

$$ \Delta T = \alpha \nabla^2 T \Delta t $$

The **Laplacian** operator can be calculated like this:

$$ \nabla^2 T = \frac{1}{\Delta x^2} \left( \sum{\partial T} - 4T \right) $$

where $\sum{\partial T}$ means *the sum of its neighbors*.

>> Note: This is not a generalized formula for the Laplacian. For the general case let $n \left(\partial T \right)$ be the number of neighbors. Then the general Laplacian is:
>>
>> $$ \nabla^2 T = \frac{1}{\Delta x^2} \left( \sum{\partial T} - n \left( \partial T \right) T \right) $$

## The Setup

The **TPLS** is build for Linux. It can be used on other OS, but you have to configure it manualy. Firstly, pull the source code. Then run the ``compiler.sh`` script:

```console
$ chmod +x ./compiler.sh
$ ./compiler.sh
```

This script compiles every erlang module in the repository. Now, you can open the erlang shell with the ``erl`` command and use the ``node`` and ``bigbrother`` modules.
