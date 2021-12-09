# gateaas

A single gateaas instance is a single boolean logic gate micro service.

A cluster of gateaas instances can represent extremely complex machines. 

Theoretically you can emulate a processor if you try hard enough.

# Usage

First build the docker container

```sh
docker build -t gateaas:1.0.0 .
```

Next, invoke the `gateaas-compiler` to output a docker-compose file.

The following invocation will "compile" `my-boolean-logic-script.gas` and generate `docker-compose.yml`.

```
gateaas-compiler my-boolean-logic-script.gas my-boolean-logic-cluster.yml
```

Next, launch the gate cluster

```
docker-compose -f my-boolean-logic-cluster.yml
```

From here, you can look inside the `my-boolean-logic-cluster.yml` to determine where to access your gates.

A gate may be probed via `curl http://my-gate/probe`

A gate accepts input via `curl http://my-gate/in/$inputNumber -d $bool` 
* `$inputNumber` represents the pin being set.  Unary gates (NOOP, NOT) will always use input 0. Binary gates will use 0 or 1.
* `$n` represents the value the pin is being set to.  Accepts 0, 1, true, false

See examples for what is possible

# Installation

Only source installation is supported right now.  Get your local haskell env set up using [ghcup](https://www.haskell.org/ghcup/) and then run `cabal install gateaas-compiler`

# Gateaas Language

Declarations
  * `INPUT a` declares that there will be some program input `a`
  * `LET foo = bar` declares that there will be some expression `bar` assigned to value `foo`
  * `OUTPUT foo = bar` declares that there will be some expression `bar` assigned to `foo`, and that nothing is allowed to reference foo.
    In future versions, outputs and input declarations will be leveraged to demonstrate the bits moving.

Functions
  * `a AND b`
  * `a NAND b`
  * `a OR b`
  * `a NOR b`
  * `a XOR b`
  * `a XNOR b`
  * `NOT a`

Parens may be used to explicitly define order of operations.
What is the default order of operations? I dunno, and it will likely change. Use parens
