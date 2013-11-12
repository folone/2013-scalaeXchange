talk_about(dpp, webdev).

talk_about(milessabin, typelevel).
talk_about(larsr_h, typelevel).
talk_about(xeno_by, macros).
talk_about(milessabin, macros).
talk_about(sirthias, webdev).

works_in(dpp, industry).
works_in(milessabin, industry).
works_in(milessabin, academia).
works_in(larsr_h, academia).
works_in(xeno_by, academia).
works_in(sirthias, industry).
                                                                                
same_topic(Person1, Person2) :-
    talk_about(Person1, Topic),
    talk_about(Person2, Topic),
    Person1 \== Person2.

same_topic(Person1, Person2) :-
    works_in(Person1, Topic),
    works_in(Person2, Topic),
    Person1 \== Person2.

exactly_same_topic(Person1, Person2) :-
  talk_about(Person1, Topic),
  talk_about(Person2, Topic),
  works_in(Person1, Area),
  works_in(Person2, Area),
  Person1 \== Person2.

topic(Topic, Res) :-
  findall(Person, talk_about(Person, Topic), L1),
  Res = (Topic, L1).

environment(Area, Res) :-
  findall(Person, works_in(Person, Area), L1),
  Res = (Area, L1).

topics(L) :-
  findall(Topic, talk_about(_, Topic), L1),
  list_to_set(L1, L).

tracks(L) :-
  topics(L1),
  member(Topic, L1),
  topic(Topic, L).
