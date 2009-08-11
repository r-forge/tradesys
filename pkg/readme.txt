Todo

- Rewrite tsys.eval with new evaluation procedure.
- Rename 'tsys' classname to 'tradesys'?
- Make tsts a subclass of zoo and re-write its .Rd.
- Add tradesys check to call tsys.frame with random data.
- Optimize equity by vectorizing all calculations.
- Improve print.tsys.
- Write summary.tsts.

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



