\# Metaphors

(Heads up: mostly just playing with ideas and types; no real use cases in sight.)

We begin, as usual, with a few language extensions, imports, and the
van Laarhoven encoding of a Lens:

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE TypeOperators #-}
> module Metaphor where
> import Control.Applicative (Const(Const, getConst))
> import qualified Data.Functor.Identity as Identity
> import qualified Unsafe.Coerce as Unsafe
> import Data.Type.Equality ((:~:)(Refl))

> type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
> type SimpleLens s a = Lens s s a a
>
> lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
> lens getter setter run s = setter s <$> run (getter s)
>
> get :: Lens s t a b -> s -> a
> get l s = getConst $ l Const s
>
> set :: Lens s t a b -> s -> b -> t
> set l s b = Identity.runIdentity $ l (const $ Identity.Identity b) s

A Lens allows us to view and update a structure `s`/`t` in terms of an aspect
`a`/`b`.

If two different structures can be viewed in terms of the same aspects `a`/`b`,
we can model a partial mapping between them. In other words, we can take an
`s`/`t` structure and alter it with contributions from a `u`/`v` structure, OR
we can take the `u`/`v` structure and alter it with contributions from the
`s`/`t`.

This looks like the kind of metaphorical thought found in sentences like this:

    we can think of the economy as a cake,
    and if we add yeast to a cake, it grows bigger,
    so we can add yeast to the economy in the form of...

> -- metaphor :: SimpleLens s a -> SimpleLens u a -> (s -> u -> s, s -> u -> u)
> metaphorPair ::
>   Lens s t a b ->
>   Lens u v b a ->
>   (s -> u -> t, s -> u -> v)
> metaphorPair ls lu =
>   ( \s u -> set ls s (get lu u)
>   , \s u -> set lu u (get ls s)
>   )

We can wrap the pair of lenses in a data type and use them as needed:

> data Metaphor s u t v = forall a b. MkMetaphor (Lens s t a b) (Lens u v b a)
> type SimpleMetaphor s u = Metaphor s u s u
>
> metaphor :: Lens s t a b -> Lens u v b a -> Metaphor s u t v
> metaphor = MkMetaphor
>
> forward :: Metaphor s u t v -> s -> u -> t
> forward (MkMetaphor ls lu) s u = set ls s (get lu u)
>
> backward :: Metaphor s u t v -> s -> u -> v
> backward (MkMetaphor ls lu) s u = set lu u (get ls s)

\## Coin metaphor

If we define a Metaphor in terms of its two operations `forward` and `backward`
(as in `metaphorPair`) we can see that:
```(s -> u -> t, s -> u -> v)```
is isomorphic to
```((s, u) -> t, (s, u) -> v)```
which is the same as
```(a ~ (s, u)) => (a -> t, a -> v)```

Thus, a more general type can be found:

> -- | t and v are two sides of the same Coin a
> data Coin c t v = MkCoin {heads :: c -> t, tails :: c -> v}

Thus, if we just look at the types, `Metaphor s u t v ~ Coin (s, u) t v`.
Or the simplified version `SimpleMetaphor s u ~ Coin (s, u) s u`.

So for any story involving characters `s` and `u`, a Metaphor lets us look at
how each protagonist sees its own ending.

However, the `Coin` type drops the idea of intermixing information from two
types. The `c` type parameter can be completely unrelated to the `t` and `v`
parameters.

In `Metaphor`, the type parameters derive from the types of inner `Lens`es: `s`
and `t` come from one side, `u` and `v` come from the other. And although this
is not apparent in the type system, the members of each pair cannot be
completely unrelated. As Edward Kmett explains in the
**Why is it a Lens Family?** section of
[http://comonad.com/reader/2012/mirrored-lenses/](this blog post):

    In order for the lens laws to hold, the 4 types parameterizing our lens
    family must be interrelated.

    In particular you need to be able to put back (with `^=`) what you get out
    of the lens (with `^.`) and put multiple times.

    This effectively constrains the space of possible legal lens families to
    those where there exists an index kind `i`, and two type families
    `outer :: i -> *`, and `inner :: i -> *`. If this were a viable type
    signature, then each lens family would actually have 2 parameters, yielding
    something like:

    ```
    -- pseudo-Haskell
    -- type LensFamily outer inner =
    --    forall a b. LensFamily (outer a) (outer b) (inner a) (inner b)
    ```

\## Pair-to-pair metaphor

`Coin a t v`, holds a pair of function from `a`. That is isomorphic to a single
function returning a pair:

> coinToPairFn :: Coin a t v -> (a -> (t, v))
> coinToPairFn (MkCoin heads tails) = \a -> (heads a, tails a)
>
> pairFnAsCoin :: (a -> (t, v)) -> Coin a t v
> pairFnAsCoin toPair = MkCoin (fst . toPair) (snd . toPair)

Thus, `Metaphor s u t v ~ Coin (s, u) t v ~ (s, u) -> (t, v)`.
Or the simplified version `SimpleMetaphor s u ~ Coin (s, u) s u ~ (s, u) -> (s, u)`.

Notice: The intuition behind `SimpleMetaphor s u` is that we can take aspects of
`s` to alter `u` and vice-versa, and this is mediated by a pair of `Lens`es.
This intuition was already lost at `Coin`, but with `(s, u) -> (t, v)` it
becomes easier to instantiate terms that can never be implemented as `Metaphor`s.

We can write terms like this:

> id :: (s, u) -> (s, u)
> id (s, u) = (s, u)

Where `s` and `u` do not mix at all. This does not look like a `Metaphor`!

In `id`, `s` and `u` do not exchange any information. Well, we have a type
that expresses no information: `()`. So we can, in fact, express this function
as a pair of `Lens`es with `()` as they focus:

> unitLens :: SimpleLens a ()
> unitLens = lens (const ()) const
>
> emptyMetaphor :: SimpleMetaphor s u
> emptyMetaphor = metaphor unitLens unitLens

However, try to find a solution for this one:

> swap :: (s, u) -> (u, s)
> swap (s, u) = (u, s)
>
> swapMetaphor :: forall s u. Metaphor s u u s
> swapMetaphor = metaphor ls lu
>  where
>   ls :: Lens s u u s
>   ls = undefined -- ???
>   lu :: Lens u s s u
>   lu = undefined -- ???

To build a generic `swapMetaphor` we would need a pair of `Lens`es that can turn
an `s` into an `u` and vice-versa - for any `s` and `u`! There's no way to build
a `Lens` like that.

Although... since the focus of each lens never leaves the metaphor, maybe we can
cheat!?

> swapMetaphorViaUnsafe :: forall s u. Metaphor s u u s
> swapMetaphorViaUnsafe = metaphor cheat cheat
>  where
>   cheat :: forall a b. Lens a b b a
>   cheat = lens Unsafe.unsafeCoerce Unsafe.unsafeCoerce

GHCi gives the following:

```
λ> forward swapMetaphorViaUnsafe 'a' (1 :: Int)
4611686018427387904

λ> backward swapMetaphorViaUnsafe 'a' (1 :: Int)
'\5764607523034234880'

λ> backward swapMetaphorViaUnsafe ('a', 'b') (1 :: Int)
([1]    428363 segmentation fault  ghci src/Metaphor.lhs
```

So no, the cheat does not work ¯\_(ツ)_/¯

Intuitively, the reason we could make `emptyMetaphor` fully generic is that it
conveyed _zero_ information between `s` and `u`. `SimpleMetaphor s u` encodes a
two-way path of information between `s` and `u`. It looks like the width of the
path we can build is limited by the amount of information we have about each
of its endpoints. If we let `s` and `u` be unconstrained, `emptyMetaphor` is the
best we can do.

\## Extending metaphors

We can get new metaphors out of existing metaphors:

    * To improve an economy we need to increase productivity
    * To bake a cake we need to add heat
    * Increasing productivity is like adding heat in that both accelerate the
      speed of components in the system (inner metaphor)
    * Thus improving an economy is like baking a cake (outer metaphor)

This suggests that we can use a `Metaphor` to bridge the gap between two
`Lens`es with different focuses, to build a new `Metaphor`! Of course, to do
that we will need a different representation:

> data Metaphor2 s u t v where
>   MkMetaphor2 ::
>     forall s u t v a b c d.
>     Lens s t a b ->
>     -- Either the two Lenses are directly compatible
>     -- or we have an inner Metaphor
>     Either (a :~: d, c :~: b) (Metaphor2 a c b d) ->
>     Lens u v c d ->
>     Metaphor2 s u t v
> type SimpleMetaphor2 s u = Metaphor2 s u s u
>
> metaphor2 :: Lens s t a b -> Lens u v b a -> Metaphor2 s u t v
> metaphor2 ls lu = MkMetaphor2 ls (Left (Refl, Refl)) lu
>
> wrapM2 :: Lens s t a b -> Metaphor2 a c b d -> Lens u v c d -> Metaphor2 s u t v
> wrapM2 ls inner lu = MkMetaphor2 ls (Right inner) lu
>
> emptyM2 :: Metaphor2 t v t v
> emptyM2 = metaphor2 unitLens unitLens
>
> forwardM2 :: Metaphor2 s u t v -> s -> u -> t
> forwardM2 (MkMetaphor2 ls inner lu) s u =
>   case inner of
>     Left (Refl, Refl) -> set ls s (get lu u)
>     Right inner -> set ls s (forwardM2 inner (get ls s) (get lu u))
>
> backwardM2 :: Metaphor2 s u t v -> s -> u -> v
> backwardM2 (MkMetaphor2 ls inner lu) s u =
>   case inner of
>     Left (Refl, Refl) -> set lu u (get ls s)
>     Right inner -> set lu u (backwardM2 inner (get ls s) (get lu u))
>

Thus, our example typechecks!

> data Economy
> data Productivity
> data Cake
> data Heat
>
> economyIsLikeACake :: SimpleMetaphor2 Economy Cake
> economyIsLikeACake =
>   wrapM2 economyHasProductivity productivityIsLikeHeat cakeHasHeat
>  where
>   economyHasProductivity :: SimpleLens Economy Productivity
>   economyHasProductivity = undefined
>   cakeHasHeat :: SimpleLens Cake Heat
>   cakeHasHeat = undefined
>   productivityIsLikeHeat :: SimpleMetaphor2 Productivity Heat
>   productivityIsLikeHeat = emptyM2

\## Composing Metaphors

Besides growing metaphors, we would like to compose them in sequence. Here are
two examples:

    A horse is like a motorcycle (people can ride them)
    and a car is like an electronic circuit (both are human-made machines).
    Therefore, a horse is like an electronic circuit.

    Doughnuts are like cake (in that they are baked foods)
    and cake is like candles (in that both are used in birthday parties).
    Therefore, dougnuts are like candles.

In both cases, we would expect the the final metaphors has a much _weaker_
connection that each of the original metaphors.

The `Metaphor Doughnut Candle` will only let a doughnut and a candle
influence each other if there are some aspects that they both share with a
`Cake`. Since the original metaphors talked aboud food and birthday parties,
we could imagine that an information such as `Color` could be transmitted:
this concern is relevant both to foods _and_ to birthday party decorations.

On the other hand, it's very hard to see what information could be shared in the
first example, `Metaphor Horse ElectronicCircuit`. This would likely be
equivalent to `emptyM2`.

In any case, how can we implement this composition?

I could not find a way to implmenet it with the primitives we have so far
(although I have not proven it's impossible!), so let's represnet composition
explicitly for now:

> data Metaphor3 s u t v where
>   -- same as Metaphor2
>   MkMetaphor3 ::
>     forall s u t v a b c d.
>     Lens s t a b ->
>     Either (a :~: d, c :~: b) (Metaphor3 a c b d) ->
>     Lens u v c d ->
>     Metaphor3 s u t v
>   ComposeMetaphor3 ::
>     forall s u t v a b.
>     Metaphor3 s u t v ->
>     Metaphor3 t v a b ->
>     Metaphor3 s u a b
> type SimpleMetaphor3 s u = Metaphor3 s u s u
>
> metaphor3 :: Lens s t a b -> Lens u v b a -> Metaphor3 s u t v
> metaphor3 ls lu = MkMetaphor3 ls (Left (Refl, Refl)) lu
>
> wrapM3 :: Lens s t a b -> Metaphor3 a c b d -> Lens u v c d -> Metaphor3 s u t v
> wrapM3 ls inner lu = MkMetaphor3 ls (Right inner) lu
>
> emptyM3 :: Metaphor3 t v t v
> emptyM3 = metaphor3 unitLens unitLens
>
> forwardM3 :: Metaphor3 s u t v -> s -> u -> t
> forwardM3 (MkMetaphor3 ls inner lu) s u =
>   case inner of
>     Left (Refl, Refl) -> set ls s (get lu u)
>     Right inner -> set ls s (forwardM3 inner (get ls s) (get lu u))
> forwardM3 (ComposeMetaphor3 msutv mtvab) s u =
>   forwardM3 mtvab (forwardM3 msutv s u) (backwardM3 msutv s u)
>
> backwardM3 :: Metaphor3 s u t v -> s -> u -> v
> backwardM3 (MkMetaphor3 ls inner lu) s u =
>   case inner of
>     Left (Refl, Refl) -> set lu u (get ls s)
>     Right inner -> set lu u (backwardM3 inner (get ls s) (get lu u))
> backwardM3 (ComposeMetaphor3 msutv mtvab) s u =
>   backwardM3 mtvab (forwardM3 msutv s u) (backwardM3 msutv s u)
