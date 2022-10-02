:- use_module(prolog/persist).

:- begin_tests(persist).

restore_file :-
    shell('cp fixtures/friends-default.pl fixtures/friends.pl').

test(add_term, [setup([fixtures/friends]), cleanup(restore_file)]) :-
    add_term(friend(jill, mark), friends),
    shell('diff fixtures/friends.pl fixtures/friends-add.pl', 0).

test(replace_term, [setup([fixtures/friends]), cleanup(restore_file)]) :-
    replace_term(friend(nick, mark), friend(nick, brick), friends),
    shell('diff fixtures/friends.pl fixtures/friends-replace.pl', 0).

test(remove_term, [setup([fixtures/friends]), cleanup(restore_file)]) :-
    remove_term(friend(nick, mark), friends),
    shell('diff fixtures/friends.pl fixtures/friends-remove.pl', 0).

test(find_term, [setup([fixtures/friends])]) :-
    find_term(friend(nick, mark), friends),
    not(find_term(friend(nick, jimmy), friends)).

:- end_tests(persist).
