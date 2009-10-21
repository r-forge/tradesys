Todo

- Unit Tests: tradesys.frame.
- Unit Tests: tsts class and its methods.
- Unit Tests: signalmap and phasemap.
- Document unit tests.
- Remove equity() and its dependencies in utils.R.
- Optimize statemap?
- Complete draft of vignette.
- Update or remove 'Optimal Leverage' code.
- Add splice code.

Ideas and Problems 

- Evaluation in tradesys.frame and trustworthy programming. There is a
  problem here in that we want the tradesys expressions to be flexible
  enough to include any function from any package, even user-defined
  functions not in package namespace. The current system creates a new
  environment and defines its enclosure to be the .GlobalEnv.. this is
  a hack, as we can get unintended evaluations of functions and names
  in our expressions. We'd like to make the enclosure the baseenv(),
  or a tradesys package namespace. That keeps the evaluation
  trustworthy as we control what gets searched, but user flexibility
  is of course clipped to the bone. What we need is a mechanism
  whereby user specifies certain packages he wants his expressions to
  see as a slot in the tradesys object. Perhaps another slot for
  user-defined function definitions.
