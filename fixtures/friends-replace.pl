:- module(friends,
          [ friend_of_a_friend/2
          ]).

friend(tim, jill).

friend(tim, ben).

friend(nick, brick).

friend(ben, mark).

friend(jill, mary).

friend_of_a_friend(A, B) :-
    friend(A, C),
    friend(C, B).
