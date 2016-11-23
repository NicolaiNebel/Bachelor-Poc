# Bachelor-Poc
Language and implementation of memory coalescing strategies on a simple AST.

This is a small tool for helping me look at the central challenges in my bachelor's project, stripping away a lot of complications.

# Running the thing

`stack test`

This parses every program in `test/basic_programs` and runs `chooseStrategy` on it.

# The thing

This is a pet language that just represents kernel variables and array accesses. Examples
can be found in `test/basic_programs`.

The representation for the summary I've arrived at is
`
 { interchange in :: Maybe Var
 , interchange out :: [Var]
 , transposes :: Map (Array -> Index)
 }
`

The interchanges can either demand some variable be innermost, or a list to not be so.  
The transposes really only care which index is supposed to be the new innermost index.

# Strategy Representation

As stated above, the representation for a strategy choice is currently:
`
 { interchange in :: Maybe Var
 , interchange out :: [Var]
 , transposes :: Map (Array -> Index)
 }
`
The structure has two components first regarding interchanges. These are complimentary,
in that one forces a parallel variable into the innermost position, while the other forces
variables out in order to make coalesced memory access.  
`interchange in` is an option value indicating that either we don't care what parallel  variable 
is innermost, or we want one specific variable to be innermost after variable interchange.  
`interchange out` is a list of variables, which needs to be moved out of the innermost spot
during interchange.

It is quite easy to see that these two fields impose constraints on each other. Specifically,
a single loop variable can not be present in both fields at once.

The third field `transposes` represents the array transposes in the strategy. It is a map
from array names to indexing. Specifically, it maps which index needs to be innermost after a
transposition of the array, in order for accesses to be coalesced given interchanges above.

I will now explain how representations of this form can be combined. It should be noted that
combination of strategies `plus` is a very incomplete function `Strat X Strat -> Strat`. Still,
`Maybe Strategy` is closed under combination, if we make any combination involving `Nothing`
return `Nothing` as well.

Given two strategies `(in1, out1, trans1)` and `(in2, out2, trans2)` we combine them to a new
strategy in the following way:

  - First, we handle interchanges. `in1` and `in2` must constrain to zero or one loop variable.
    Furthermore, the union of `out1` and `out2` must not include that variable or all 
    loop variables. If this holds, we have our new interchange strategy, otherwise the 
    combination fails.
  - Then, we handle transposes. Each strategy must constrain every array to maximally one
    transpose. If they don't, we can't transpose an array to coalece accesses that fits the
    new interchange constraints, and thus the combination fails.

In this manner, the constraints are preserved and represented as a new strategy.

# Generating strategies

The way the combination is phrased above, as a bottom-up combination, lends itself easily to
this way of generating a strategy for a given program: For each access, make a list of potential
strategies involving interchanges and transposes. Now we have a list of lists of strategies.
Using `plus` described above, we fold over the list and tries combining every pair of strategies.

The result of the computation is a list of strategies that satisfies the constraints of the entire
program. From this list, we can pick the strategy that maximises interchanges and minimises
transposes.

So, how do we go about generating the initial strategies from a single access?
  - For every loop variable `v`, we can generate the strategy that has `Just v` in
    `interchange in`, and transposes the array to make this access be coalesced. This might be
    the identity transpose. If several indices in the access depend on `v`, then we discard
    the strategy
  - We also try interchanging every variable that the access is variant to out. This creates
    invariant access of the array. Obviously, this is not possible if the array is variant to
    all the loop variables.
