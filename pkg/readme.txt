Todo

- Unit Tests: tsts and its methods.
- Unit Tests: tradesys.frame.
- Unit Tests: signalmap, statemap, and phasemap.
- Document unit tests.
- Add tradesys check to call tradesys.frame with random data.
- Improve print.tsys.
- Write summary.tsts.
- Optimize equity, signalmap, statemap, and phasemap.
- Complete draft of vignette.

*** New Stuff ***

- Update or remove 'Optimal Leverage' code.
- Add splice code.
- Include some equity analysis tools?

Ideas and Problems 

- Evaluation in tradesys.frame and trustworthy programming. There is a
  problem here in that we want the tsys expressions to be flexible
  enough to include any function from any package, even user-defined
  functions not in package namespace. The current system creates a new
  environment and defines its enclosure to be the .GlobalEnv.. this is
  a hack, as we can get unintended evaluations of functions and names
  in our expressions. We'd like to make the enclosure the baseenv(),
  or a tradesys package namespace. That keeps the evaluation
  trustworthy as we control what gets searched, but user flexibility
  is of course clipped to the bone. What we need is a mechanism
  whereby user specifies certain packages he wants his expressions to
  see as a slot in the tsys object. Perhaps another slot for
  user-defined function definitions.

- Evaluation in tradesys.frame and lazy evaluation. Evaluation here
  currently follows a sequence, which implies a hierarchy of
  expression reference. For example, the entry/exit expressions can
  refer to any exprvars expression, but no exprvars expression can
  refer to any other expression. We would like to implement a scheme
  whereby all (some?) expressions are initially promises and get
  evaluated when needed. This comes at the cost of allowing circular
  references, but that just raises an R error. The benefit is that the
  expression encoding is much more flexible when the user can
  impliment his own dependency tree. 

- Roll adjustements. It might seem like roll.at is a complicating
  feature that is better off dealt with by manipulating the spliced
  price series itself. That is the standard practice, referred to in
  the systems literature as "smoothing continuous price series" or
  something like that. The most common technique is to cumulatively
  add the roll points backwards over the entire series to remove the
  bogus PnL effects of the price change over roll points. The
  disadvantage of this approach is that it (1) can result in negative
  prices and (2) reduces transparency, as the trade prices no longer
  match the actual price series. Other techniques of this strategy
  (like the "adjust by ratio" technique) remove the first problem at
  the cost of introducing another (distorting the PnL). Our approach
  avoids both, although in principle negative prices can still occur
  if the roll jumps are big enough and the entry/exit horizons are
  long enough. I'm still not sure whether we have the best solution
  and wonder whether the splice and splooth functions (not yet
  included here) can provide the basis for a better approach.
