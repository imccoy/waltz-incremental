Consider:

> length [] = 0
> length (x:xs) = 1 + length xs

Given a change in the first argument, what happens to the result?

If we first call length [1,2,3], we get 3.
Next, a 5 arrives to be prepended. We want to pass in 5:[1,2,3].
Except that we really want to have length tell us what happens when
we cons a 5 onto the top of that list. Rather than calling

> length 5:[1,2,3]

We call

> length_incrementalised_cons 5 [1,2,3]

But that's actually no good, since our objective here is to not
recompute the whole function. We want to pass length's former output,
not the old input:

> length_incrementalised_cons 5 3

The implementation of length_incrementalised_cons looks something like

> length_incrementalised_cons x length_xs = 1 + length_xs

And it correctly returns 4. This is within the powers of rad.hs as it
currently stands. It's more exciting than it sounds, I promise.

It's time for level 2. To do useful things we need to be able to
compose functions, which means we need composable representations of
the change in the input.

This is immutable-everything, so "change" is not a well-formed notion.
For our purposes, a change is a new value such that there is substantial
sharing with a value for which we've already computed a result. The
shared portion is not referred to in the change in order to avoid the
awkard confusion above between the previous value input and the
corresponding output.

Lets look at a plain old list here. 

> data List a = LCons a (List a)
>             | ListEmpty
> 

The tail of the list could change.

> data InputChangeList a = InputChangeLCons1 (InputChange (List a))

(the 1 in the constructor name refers to the index of the relevant
field in the LCons constructor, starting at 0. We'll get to 0
shortly...)

This is a bit cheeky: I'm treating InputChange as a function from
types to types, so InputChange (List a) = InputChangeList a. In
general, InputChange takes a type and returns a representation of
changes in that type. This is completely illegal haskell, but that might
be okay here since this code will actually be generated.



The head of the list could change.

>                        | InputChangeLCons0 (InputChange a)

This use of the InputChange hypothetical function is a problem.



The list could be replaced by a new list, consing a new value on to
the top.
>                        | InputChangeNewList a

Just as a sanity check, let's check out some other structures.

> data SnocList a = Cons a (SnocList a)
>                 | Snoc (SnocList a) a
>                 | SnocEmpty
>

We change the head-elem of the list.
> data InputChangeSnocList a = InputChangeCons0 (InputChange a)

We change the tail-list of the list.
>                            | InputChangeCons1 (InputChange (SnocList a))

We change the head-list of the list.
>                            | InputChangeSnoc0 (InputChange (SnocList a))

We change the tail-elem of the list.
>                            | InputChangeSnoc1 (InputChange a)

We prepend.
>                            | InputChangeNewCons a

We append.
>                            | InputChangeNewSnoc a

There's a problem here. What if you try to change the tail-elem of a value
that has a head-elem and a tail-list? This problem may be the rock upon
which my project founders.


One more structure, for luck.
>
> data Tree a = Leaf a
>             | Tree (Tree a) (Tree a)
>

The leaf, the left branch, and the right branch change fairly obviously.
> data InputChangeTree a = InputChangeLeaf (InputChange a)
>                        | InputChangeTree0 (InputChange (Tree a))
>                        | InputChangeTree1 (InputChange (Tree a))
Construct a new tree, with the given tree on the left and the existing
tree on the right.
>                        | InputChangeNewTree0 (Tree a)
Construct a new tree, with the given tree on the right and the existing
tree on the left.
>                        | InputChangeNewTree1 (Tree a)
