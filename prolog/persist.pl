:- module(persist,
         [find_term/2,
          add_term/2,
          replace_term/3,
          remove_term/2]).
:- use_module(library(reif), [if_/3, (=)/3]).
:- use_module(library(settings), [set_setting/2, setting/4]).

/** <module> Persists facts into a file
 * - This is somewhat like a mix between `library(persistency)` and library(setting)
 * - It's (currently) less efficient, intended for small-ish files or one-at-a-time updates,
     such as standalone apps.
 * - It preserves the prolog terms as passed, not wrapping it in `assert`
 * - It's able to add clauses as well as facts, any prolog term
 * - The use case for this is to modify existing modules, preserving all clauses
 */

:- setting(separator_fn, callable, nl, "The separator function to use between terms").

internal:noop(_).

internal:remove_term(Term, InputStream, OutputStream, SeparatorFn) :-
    read(InputStream, InputTerm),
    setting(separator_fn, NewSeparatorFn),
    if_(InputTerm = Term, true,
        (InputTerm = end_of_file ->
         true ;
         (call(SeparatorFn, OutputStream), portray_clause(OutputStream, InputTerm)))),
    if_(InputTerm = end_of_file, true,
        internal:remove_term(Term, InputStream, OutputStream, NewSeparatorFn)).

internal:remove_term(Term, File) :-
    setup_call_cleanup(
        (open(File, read, InputStream), open(File, write, OutputStream)),
        internal:remove_term(Term, InputStream, OutputStream, internal:noop),
        (close(InputStream), close(OutputStream))
    ).

%! remove_term(+Term, +ModuleName) is semidet
%
% Removes a Term from the module
remove_term(Term, ModuleName) :-
    module_property(ModuleName, file(File)),
    internal:remove_term(Term, File).



internal:replace_term(OldTerm, NewTerm, InputStream, OutputStream, SeparatorFn) :-
    read(InputStream, InputTerm),
    setting(separator_fn, NewSeparatorFn),
    if_(InputTerm = OldTerm,
        (call(SeparatorFn, OutputStream), portray_clause(OutputStream, NewTerm)),
        (InputTerm = end_of_file ->
         true ;
         (call(SeparatorFn, OutputStream), portray_clause(OutputStream, InputTerm)))),
    if_(InputTerm = end_of_file, true,
        internal:replace_term(OldTerm, NewTerm, InputStream, OutputStream, NewSeparatorFn)).

internal:replace_term(OldTerm, NewTerm, File) :-
    setup_call_cleanup(
        (open(File, read, InputStream), open(File, write, OutputStream)),
        internal:replace_term(OldTerm, NewTerm, InputStream, OutputStream, internal:noop),
        (close(InputStream), close(OutputStream))
    ).

%! replace_term(+OldTerm, +NewTerm, +ModuleName) is semidet
%
% Replace a term in the module.
% This will replace all instances of OldTerm with NewTerm in place.
replace_term(OldTerm, NewTerm, ModuleName) :-
    module_property(ModuleName, file(File)),
    internal:replace_term(OldTerm, NewTerm, File).



internal:find_in_stream(Term, InputStream) :-
    read(InputStream, InputTerm),
    if_(InputTerm = Term,
        true,
        if_(InputTerm = end_of_file, false, internal:find_in_stream(Term, InputStream))
    ).

internal:find_term(Term, File) :-
    setup_call_cleanup(
        open(File, read, InputStream),
        internal:find_in_stream(Term, InputStream),
        close(InputStream)
    ).

%! find_term(+Term, +ModuleName) is semidet
%
% Finds the Term in the module, returning `true` if found.
find_term(Term, ModuleName) :-
    module_property(ModuleName, file(File)),
    internal:find_term(Term, File).



internal:add_term(Term, File) :-
    setup_call_cleanup(
        open(File, append, OutputStream),
        (internal:find_term(Term, File) -> false ;
         (nl(OutputStream), portray_clause(OutputStream, Term))),
        close(OutputStream)
    ).

%! add_term(+Term, +ModuleName) is semidet
%
% Adds a Term to the module
add_term(Term, ModuleName) :-
    module_property(ModuleName, file(File)),
    internal:add_term(Term, File).
