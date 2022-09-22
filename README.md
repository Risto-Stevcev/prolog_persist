# prolog-persist

Persists facts into a file


## Usage

```prolog
?- use_module(library(persist)).
true.

?- use_module(fixtures/friends).
true.

?- find_term(friend(jill, mark), friends).
false.

?- add_term(friend(jill, mark), friends).
true.

?- find_term(friend(jill, mark), friends).
true.

?- remove_term(friend(jill, mark), friends).
true.

?- find_term(friend(jill, mark), friends).
false.
```


## License

See LICENSE
