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

# Current state
There seems to be a bug right now, `bit_bigger_example` is not choosing the correct
strategy, opting for a strategy that transposes two arrays instead of just one.
