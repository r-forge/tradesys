Todo

- Unit Tests: tsts and its methods.
- Unit Tests: tradesys.frame.
- Unit Tests: signalmap, statemap, and phasemap.
- Document unit tests.
- Add tradesys check to call tsys.frame with random data.
- Improve print.tsys.
- Write summary.tsts.
- Optimize equity, signalmap, statemap, and phasemap.
- Complete draft of vignette.

*** New Stuff**

- Update or remove 'Optimal Leverage' code.
- Add splice code.
- Include some equity analysis tools?

Ideas and Problems 

- Evaluation in tsys.frame and trustworthy programming. There is a
  problem here in that we want the tsys expressions to be flexible
  enough to include any function from any package, even user-defined
  functions not in package. The current system creates a new
  environment and defines its enclosure to be the .GlobalEnv.. this is
  a hack, as we can get unintended evaluations of functions and names
  in our expressions. We'd like to make the enclosure the baseenv(),
  or a tradesys package namespace. That keeps the evaluation
  trustworthy as we control what gets searched, but user flexibility
  is of course clipped to the bone. What we need is a mechanism
  whereby user specifies certain packages he wants his expressions to
  see as a slot in the tsys object. Perhaps another slot for
  user-defined function definitions. 



