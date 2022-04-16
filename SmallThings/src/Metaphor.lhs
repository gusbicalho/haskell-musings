\# Metaphors

(Heads up: mostly just playing with ideas and types; no real use cases in sight.)

We begin, as usual, with a few language extensions, imports, and the
van Laarhoven encoding of a Lens:

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Metaphor where
>
> import Control.Applicative (Const(Const, getConst))
> import qualified Data.Functor.Identity as Identity
>
> type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)
> type SimpleLens s a = Lens s s a a
>
> lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
> lens getter setter run s = setter s <$> run (getter s)
>
> get :: Lens s t a b -> s -> a
> get l = getConst . l Const
>
> update :: Lens s t a b -> (a -> b) -> s -> t
> update l f = Identity.runIdentity . l (Identity.Identity . f)
>
> set :: Lens s t a b -> b -> s -> t
> set l b = update l (const b)

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
>   ( \s u -> set ls (get lu u) s
>   , \s u -> set lu (get ls s) u
>   )

We can wrap the pair of lenses in a data type and use them as needed:

> data Metaphor s u t v = forall a b. MkMetaphor (Lens s t a b) (Lens u v b a)
> type SimpleMetaphor s u = Metaphor s u s u
>
> metaphor :: Lens s t a b -> Lens u v b a -> Metaphor s u t v
> metaphor = MkMetaphor
>
> oneHand :: Metaphor s u t v -> s -> u -> t
> oneHand (MkMetaphor ls lu) s u = set ls (get lu u) s
>
> otherHand :: Metaphor s u t v -> s -> u -> v
> otherHand (MkMetaphor ls lu) s u = set lu (get ls s) u
>
> project :: Metaphor s u t v -> s -> u -> (t, v)
> project metaphor s u = (oneHand metaphor s u, otherHand metaphor s u)

\## Coin metaphor

If we define a Metaphor in terms of its two operations `oneHand` and `otherHand`
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

`Coin a t v`, holds a pair of functions from `a`. That is isomorphic to a single
function returning a pair:

> coinToPairFn :: Coin a t v -> (a -> (t, v))
> coinToPairFn (MkCoin heads tails) = \a -> (heads a, tails a)
>
> pairFnAsCoin :: (a -> (t, v)) -> Coin a t v
> pairFnAsCoin toPair = MkCoin (fst . toPair) (snd . toPair)

For `Coin`s representing metaphors, `a` will always be a pair `(s, u)`, and
the representation of a Metaphor as a function between pairs is given by
`uncurry . project`.

Thus, `Metaphor s u t v ~ Coin (s, u) t v ~ (s, u) -> (t, v)`.
Or the simplified version `SimpleMetaphor s u ~ Coin (s, u) s u ~ (s, u) -> (s, u)`.

The intuition behind `SimpleMetaphor s u` is that we can take aspects of
`s` to alter `u` and vice-versa, and this is mediated by a pair of `Lens`es.
This intuition was already lost at `Coin`, but with `(s, u) -> (t, v)` it
looks like we might be able to write terms that can never be implemented as
`Metaphor`s.

However, as the isomorphism below shows, every function from pairs to pairs
can, indeed, be expressed as a Metaphor!

> metaphorToPairFn :: Metaphor s u t v -> (s, u) -> (t, v)
> metaphorToPairFn = uncurry . project
>
> pairFnToMetaphor :: forall s u t v. ((s, u) -> (t, v)) -> Metaphor s u t v
> pairFnToMetaphor pairFn = metaphor ls lu
>  where
>   ls :: Lens s t (u -> (t, v))  (s -> (t, v))
>   ls = lens (curry pairFn) (\s stv -> fst $ stv s)
>   lu :: Lens u v (s -> (t, v)) (u -> (t, v))
>   lu = lens (flip $ curry pairFn) (\u utv -> snd $ utv u)

I'm not sure those lenses are _lawful_, though, or whether they fit the
criteria explained by Kmett in his blog post. Future work, I guess?

Let's see some interesting examples of conversion to Metaphor. We know we could
use `pairFnToMetaphor` above to do this automatically, but hand-crafting the
code can give us some insight.

Take `id`, for example:

> idPair :: (s, u) -> (s, u)
> idPair (s, u) = (s, u)

In `idPair`, `s` and `u` `s` and `u` do not mix at all; they do not exchange any
information. Well, we have a type that expresses no information: `()`. So we
should be able to express this function as a pair of `Lens`es with `()` as their
focus:

> -- | Empty focus - no reading nor writing any interesting information
> unitLens :: SimpleLens a ()
> unitLens = lens (const ()) const
>
> emptyMetaphor :: SimpleMetaphor s u
> emptyMetaphor = metaphor unitLens unitLens

Another interesting one is `swap`:

> swap :: (s, u) -> (u, s)
> swap (s, u) = (u, s)

Here `s` and `u` switch places, which mean the information from one side of the
Metaphor completely replaces the other (and vice-versa). So we need to let _all_
information about a type go through the Metaphor connection, which means that
the lenses should focus on the entire structures. Such a lens would have this
type:

> -- | Full focus - allow reading and replacing and entire structure
> fullLens :: Lens s t s t

Which translates to `forall f. Functor f => (s -> f t) -> (s -> f t)`. We could
implement this with `lens id (\_ u -> u)`, but there's an even better
alternative:

> fullLens = id
>
> swapMetaphor :: forall s u. Metaphor s u u s
> swapMetaphor = metaphor fullLens fullLens

Now for one last example:

> dropSnd :: (s, u) -> (s, s)
> dropSnd (s, _) = (s, s)
>
> -- | Allows looking at an entire structure, but no modification
> shieldLens :: Lens s s s ()
> shieldLens = lens id const
>
> -- | Allows replacing an entire structure, but no reading its contents
> replaceLens :: Lens s t () t
> replaceLens = lens (const ()) (\_ s -> s)
>
> dropSndMetaphor :: Metaphor s u s s
> dropSndMetaphor = metaphor shieldLens replaceLens

\## Extending metaphors

We can get new metaphors out of existing metaphors:

    * To improve an economy we need to increase productivity
    * To bake a cake we need to add heat
    * Increasing productivity is like adding heat in that both accelerate the
      speed of components in the system (inner metaphor)
    * Thus improving an economy is like baking a cake (outer metaphor)

This suggests that we can use a `Metaphor` to bridge the gap between two
`Lens`es with different focuses, to build a new `Metaphor`!

> wrap :: Lens p q s t -> Metaphor s u t v -> Lens w x u v -> Metaphor p w q x
> wrap lpqst (MkMetaphor lstab luvba) lwxuv = metaphor (lpqst . lstab) (lwxuv . luvba)

Thus, our example typechecks!

> data Economy
> data Productivity
> data Cake
> data Heat
>
> economyIsLikeACake :: SimpleMetaphor Economy Cake
> economyIsLikeACake =
>   wrap economyHasProductivity productivityIsLikeHeat cakeHasHeat
>  where
>   economyHasProductivity :: SimpleLens Economy Productivity
>   economyHasProductivity = undefined
>   cakeHasHeat :: SimpleLens Cake Heat
>   cakeHasHeat = undefined
>   productivityIsLikeHeat :: SimpleMetaphor Productivity Heat
>   productivityIsLikeHeat = emptyMetaphor

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

> composeMetaphors :: forall s t u v w x. Metaphor t v w x -> Metaphor s u t v -> Metaphor s u w x
> composeMetaphors mtvwx msutv = metaphor lsw lux
>  where
>   pairFn (s, u) = let (t, v) = project msutv s u
>                    in project mtvwx t v
>   -- I've copied the code from pairFnToMetaphor over here
>   lsw :: Lens s w (u -> (w, x))  (s -> (w, x))
>   lsw = lens (curry pairFn) (\s swx -> fst $ swx s)
>   lux :: Lens u x (s -> (w, x)) (u -> (w, x))
>   lux = lens (flip $ curry pairFn) (\u uwx -> snd $ uwx u)

We simply project one Metaphor after the other! Then we decompose the resulting
pair-to-pair function into a pair of `Lens`es that each focus on one partial
result. I think there's some duplicated computation going on here, so possible
space for improvement, but this works.

\## Better representation?

`Lens`es have an explicit representation as a product of a getter and setter.
However, usually we go for the van Laarhoven representation because its more
efficient. Besides, making `Lens` a type alias mean that libraries can export
`Lens`es without depending on a lens library.

So far we've implemented `Metaphor` as a product of two lenses. Is there a
better way?

> type ScottMetaphor s u t v = forall r. (forall a b. Lens s t a b -> Lens u v b a -> r) -> r
>
> scottMetaphor :: Lens s t a b -> Lens u v b a -> ScottMetaphor s u t v
> scottMetaphor ls lu = \run -> run ls lu

This is a Scott-encoding of a product of two lenses. It's a promise: give me a
function that accepts two lenses, and I will call it with two lenses. The
inner `forall a b` represents the existential quantification of the focus of
the lenses"you can't know anything about them, except that they match correctly.

This has essentially the same semantics as the actual product encoding we used
above. GHC may be able to inline it a little better, but we still working with
a pair of lenses, we can still "pattern match" to access them individually if
we want (which we did above to implement `wrap`).

Another alternative is to rely on the isomorphism we proved above, and just
use a function of pairs:

> type ProjectionMetaphor s u t v = (s, u) -> (t, v)
>
> -- Almost the same as `metaphorPair` above
> projectionMetaphor :: Lens s t a b -> Lens u v b a -> ProjectionMetaphor s u t v
> projectionMetaphor ls lu = \(s, u) -> (set ls (get lu u) s, set lu (get ls s) u)

With this representation, `composeMetaphors` becomes simple function
composition!

> composeProjectionMetaphors ::
>   ProjectionMetaphor t v w x ->
>   ProjectionMetaphor s u t v ->
>   ProjectionMetaphor s u w x
> composeProjectionMetaphors = (.)

However, `wrap` becomes more involved:

> projectionWrap ::
>   Lens p q s t ->
>   ProjectionMetaphor s u t v ->
>   Lens w x u v ->
>   ProjectionMetaphor p w q x
> projectionWrap lpqst metaphor lwxuv (p, w) =
>    let (t, v) = metaphor (get lpqst p, get lwxuv w)
>        oneHand = set lpqst t p
>        otherHand = set lwxuv v w
>     in (oneHand, otherHand)

Overall, I like `ProjectionMetaphor` best. We avoid any mention to existential
quantification, simplify composition and avoid introducing new datatypes. Even
the new `wrap` implementation is clear enough (unlike the previous
`composeMetaphors` implementation).
