# **TPLS**: Thermal Process Lattice Simulator

## The Idea

The **TPLS** is an physical simulator runing on top of Erlang. It is based on discretizing the **heat equation**:

$$ \frac{\partial T}{\partial t} = \alpha \nabla^2 T $$

The **TPLS** uses a **lattice** of erlang **processes** (*shortly: nodes*) which exchange messages. All nodes are supervized by *"Big Brother"* who prevents premature crashes. 

## The Scheme

In the schemes every node is represented by a circle. The lines are logical connections. This is how the lattice looks like:

<img src="./assets/lattice.svg">

Every node knows its temperature and neighbors:

<img src="./assets/node.svg">

For time evolution, the node has to request the temperatures of its neighbors, the thermal diffusivity of the supervisor, and calcutate the next temperature via the **heat equation**:

$$ \Delta T = \alpha \nabla^2 T \Delta t $$

The **Laplacian** operator can be calculated like this:

$$ \nabla^2 T = \frac{1}{\Delta x^2} \left( \sum{\partial T} - 4T \right) $$

where $\sum{\partial T}$ means *the sum of its neighbors*.

> Note: This is not a generalized formula for the Laplacian. For the general case let $n \left(\partial T \right)$ be the number of neighbors. Then the general Laplacian is:
>
> $$ \nabla^2 T = \frac{1}{\Delta x^2} \left( \sum{\partial T} - n \left( \partial T \right) T \right) $$

## The Setup

The **TPLS** is build for Linux. It can be used on other OS, but you have to configure it manualy. Firstly, pull the source code. Then run the ``compiler.sh`` script:

```console
$ chmod +x ./compiler.sh
$ ./compiler.sh
```

This script compiles every erlang module in the repository. Now, you can open the erlang shell with the ``erl`` command and use the ``node`` and ``bigbrother`` modules.

## The Simulator

Let's build a simulation. Firstly, we will initialize an "empty" ``bigbrother`` process:

```erlang
BB = bigbrother:start().
```

By default the material of the system is set to iron. You can change it by passing the name of the material as an argument to the ``bigbrother:start/1`` function:

```erlang
Material = gold,
BB = bigbrother:start(Material).
```

For simplisity we will simulate how the temperature of a beam evolves. Let's create a basis. It will begin in $x = -1$ and it will end in $x = 1$. We will use $\Delta x = 0.1$:

```erlang
Start = -1,
End = 1,
DX = 0.1,

X = therm:basis(Start, DX, End).
```

Let's use $T \left(x\right) = 20 e^{-x^2} + 300$. To get the temperatures of the beam, we will use ``therm:beam/2`` function:

```erlang
F = fun (X) -> 20 * math:exp( - X * X ) + 300 end,
T = therm:beam(F, X).
```

Now let's create the processes. We will message the ``bigbrother`` process as developers:

```erlang
BB ! { dev, { start, { beam, T } } }.
```

Now we are ready to simulate the system. Simply message the supervisor like this:

```erlang
DT = 0.1,
BB ! { dev, { evolve, DT } }.
```

This will evolve the state with $\Delta t = 0.1$ seconds.
