EXPERIMENTAL. PROVISIONAL. NOT READY FOR ACTUAL USE.

FUN AND POSSIBLY MIND-EXPANDING, THOUGH.

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


Now, let's move from comp-sci-land into businessy-type-systems land. For
some reason, we've been asked to build a blogging application. Users (who
will appear out of the ether with type User) make Posts to Blogs.

> data BlogAppState = BlogAppState [Blog]
> data Blog = Blog String [BlogEntry]
> data BlogEntry = BlogEntry User String

At the top level, the input change in BlogAppState is entirely
characterized by the change in its first argument. This is as per the Leaf
example above.
> data InputChangeBlogAppState = InputChangeBlogAppState (InputChange [Blog])

The input change in a blog is either a change in the name, or a change in
the blogs' entries. Happily, this is like a SnocList.
> data InputChangeBlog = InputChangeBlog0 (InputChange String)
>                      | InputChangeBlog1 (InputChange [BlogEntry])

And in its turn, the blog entry can change either in its authorship or its
body, like nothing more nor less than itself. Or perhaps like the SnocList
again.
> data InputChangeBlogEntry = InputChangeBlogEntry0 (InputChange User)
>                           | InputChangeBlogEntry1 (InputChange String)


That's all well and good, but it'd be nice if it was actually useful.


THE FOLLOWING CONTENT IS EVEN MORE RIDICULOUSLY PROVISIONAL THAN THE
PRECEDING

As a prelude, let's see if we can incrementalize some old friends: map and
filter.

> map f (x:xs) = (f x):(map xs)
> map _ [] = []

As a sanity-preserving maneuver, we ignore changes in f. That gives us:

The first case is simplest, and actually the most important. When we stick
the new thing on the front, we return a function that takes an old list,
applies f to the new thing, and sticks the result on the front.
> mapIncrementalized f (InputChangeNewList v) = (\prior -> (f v):prior)

Now we start to stretch our luck. If the first element changes, then return
a function that updates it accordingly. If we're doing a complete
replacement of the value, it's not too bad.

> mapIncrementalized f (InputChangeLCons0 (Replacement v)) = (\(h:t) -> (f v):t)

But conceivably, we could just be adjusting it. For this we need an
incrementalized version of the function f.
> mapIncrementalized f (InputChangeLCons0 c) = (\(h:t) -> (fIncrementalized c h):t)

We can also deal with changes deep in the bowels of the list. Perhaps.
> mapIncrementalized f (InputChangeLCons1 c) = (\(h:t) -> h:(mapIncrementalized f c t))


> filter f (x:xs)
>   | f x       -> x:(filter f xs)
>   | otherwise -> filter f xs
> filter _ [] = []


> filterIncrementalized f (InputChangeNewList v)
>   | f v       -> (\prior -> v:prior)
>   | otherwise -> id

Some things just don't work. In order to do the right thing for changes to
an existing element, we need to know if the old value of (f v) was true or
not - and in this model, we don't.
> filterIncrementalized f (InputChangeLCons0 (Replacement v)) = undefined
> filterIncrementalized f (InputChangeLCons0 c) = undefined

But we can still recurse, if we think that's a good idea.
> filterIncrementalized f (InputChangeLCons1 c) = (\(h:t) -> h:(filterIncrementalized f c t))

What if we want to map over the return value of a call to filter? So far,
an incrementalised function looks like this:

> f :: a -> b

> fIncrementalized :: InputChange a -> (b -> b)

That is, you pass in an InputChange, and you get a function that adapts the
output accordingly. In order to incrementalize a map over the return value
of filter without redoing the entire map, though, we need to have an
incrementalized version of filter that returns an InputChange for map:

> fIncrementalized' :: InputChange a -> InputChange b 

So how does that look for filter?

> filterIncrementalized' f (InputChangeNewList v)
>   | f v       -> InputChangeNewList v
>   | otherwise -> InputChangeNone



How do blogs and blog entries get created? Somehow, there's a stream of
events hangin' round:

> data BlogAppEvent = NewBlog String
>                   | NewBlogEntry String User String 
>
> isNewBlogEvent x = case x of (NewBlog _) -> True; otherwise -> False
> isNewBlogEntryEvent x = case x of (NewBlogEntry _ _ _) -> True; otherwise -> False
> eventBlogName (NewBlogEntry name _ _) = name
>

We can define a value of type BlogAppState as a function of a list of
events:

> blogAppState events = BlogAppState $ blogs events
>
> blogs events = map (blog events) $ filter isNewBlogEvent events
> blog events (NewBlog name) = Blog name (blogEntries events name)
> blogEntries events name = let
>     entryEvents = filter isNewBlogEntryEvent events
>     thisBlogEvents = filter ((==name) . eventBlogName) entryEvents
>   in map (\NewBlogEntry _ user body -> BlogEntry user body) thisBlogEvents

Syntactically, this is not how I want to work, but semantically it
definitely is.

The first line incrementalizes readily:

> blogAppStateIncrementalized (InputChangeBlogAppState blogsChange) =
>   (\(BlogAppState blogs) -> BlogAppState $ blogsIncrementalized blogsChange blogs)

The next is a bit more interesting. The previous state is a [Blog]. The
incrementalization has to achieve two things. First, any new blog events
have to cause new entries in that list; secondly, any new entry events have
to filter down and cause the relevant blog values to change.

The first of those is easy to achieve, with a definition like:
> blogsIncrementalized (InputChangeNewList event) =
>  \blogs -> (mapIncrementalized blog $ filterIncrementalized' isNewBlogEvent event) blogs
>   

But that does not, on face value, achieve the second.
