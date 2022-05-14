s(S) --> sentence(S).
s(W) --> who_pronoun_phrase(W).
s(W) --> what_pronoun_phrase(W).

%% Sentences %%
sentence(S) --> clause(S).
sentence(ds(S1,C,S2)) --> clause(S1), conjunction(C), clause(S2).

sentence(s(NP)) --> noun_phrase(NP).

sentence(s(A)) --> adjective(A).
sentence(s(A1,C,A2)) --> adjective(A1), conjunction(C), adjective(A2).

sentence(s(A)) --> adverb(A).
sentence(s(A1,C,A2)) --> adverb(A1), conjunction(C), adverb(A2).

sentence(s(PP)) --> prepositional_phrase(PP).

clause(s(NP,VP)) --> noun_phrase(NP), verb_phrase(VP).
clause(s(NP,AVP)) --> noun_phrase(NP), anded_verb_phrase(AVP).
%% Sentences %%

%% Noun Phrases %%
noun_phrase(NPI) --> noun_phrase_internal(NPI).
noun_phrase(NPC) --> noun_phrase_complex(NPC).

noun_phrase(dnp(NPI1,C,NPI2)) --> noun_phrase_internal(NPI1), conjunction(C), noun_phrase_internal(NPI2).
noun_phrase(dnp(NPI1,C,NPC2)) --> noun_phrase_internal(NPI1), conjunction(C), noun_phrase_complex(NPC2).
noun_phrase(dnp(NPC1,C,NPI2)) --> noun_phrase_complex(NPC1), conjunction(C), noun_phrase_internal(NPI2).
noun_phrase(dnp(NPC1,C,NPC2)) --> noun_phrase_complex(NPC1), conjunction(C), noun_phrase_complex(NPC2).

noun_phrase_complex(cnp(NPI,PP)) --> noun_phrase_internal(NPI), prepositional_phrase(PP).

noun_phrase_complex(cnp(NPI,W)) --> noun_phrase_internal(NPI), whom_obj_pronoun_phrase(W).
noun_phrase_complex(cnp(NPI,PP,W)) --> noun_phrase_internal(NPI), prepositional_phrase(PP), whom_obj_pronoun_phrase(W).

noun_phrase_internal(np(SN)) --> singular_noun(SN).
noun_phrase_internal(np(PN)) --> plural_noun(PN).
noun_phrase_internal(np(PP)) --> personal_pronoun(PP).

noun_phrase_internal(np(D,SN)) --> det(D), singular_noun(SN).
noun_phrase_internal(np(D,PN)) --> det(D), plural_noun(PN).

noun_phrase_internal(np(SD,SN)) --> singular_det(SD), singular_noun(SN).
noun_phrase_internal(np(PD,PN)) --> plural_det(PD), plural_noun(PN).

noun_phrase_internal(np(D,AP,SN)) --> det(D), adjective_phrase(AP), singular_noun(SN).
noun_phrase_internal(np(D,AP,PN)) --> det(D), adjective_phrase(AP), plural_noun(PN).

noun_phrase_internal(np(SD,AP,SN)) --> singular_det(SD), adjective_phrase(AP), singular_noun(SN).
noun_phrase_internal(np(PD,AP,PN)) --> plural_det(PD), adjective_phrase(AP), plural_noun(PN).

noun_phrase_internal(np(AP,SN)) --> adjective_phrase(AP), singular_noun(SN).
noun_phrase_internal(np(AP,PN)) --> adjective_phrase(AP), plural_noun(PN).
%% Noun Phrases %%

%% Verb Phrases %%
anded_verb_phrase(dvp(VP1,C,VP2)) --> verb_phrase(VP1), conjunction(C), verb_phrase(VP2).

verb_phrase(VPI) --> verb_phrase_internal(VPI).
verb_phrase(VPC) --> verb_phrase_complex(VPC).

verb_phrase(dvp(VPI1,C,VPI2)) --> verb_phrase_internal(VPI1), conjunction(C), verb_phrase_internal(VPI2).
verb_phrase(dvp(VPI1,C,VPC2)) --> verb_phrase_internal(VPI1), conjunction(C), verb_phrase_complex(VPC2).
verb_phrase(dvp(VPC1,C,VPI2)) --> verb_phrase_complex(VPC1), conjunction(C), verb_phrase_internal(VPI2).
verb_phrase(dvp(VPC1,C,VPC2)) --> verb_phrase_complex(VPC1), conjunction(C), verb_phrase_complex(VPC2).

verb_phrase_complex(cvp(VPI,PP)) --> verb_phrase_internal(VPI), prepositional_phrase(PP).

verb_phrase_complex(cvp(AP,VPI)) --> adverb_phrase(AP), verb_phrase_internal(VPI).
verb_phrase_complex(cvp(AP,VPI,PP)) --> adverb_phrase(AP), verb_phrase_internal(VPI), prepositional_phrase(PP).

verb_phrase_internal(vp(V)) --> past_verb(V).
verb_phrase_internal(vp(V)) --> past_double_verb(V).

verb_phrase_internal(vp(V,NP)) --> past_verb(V), noun_phrase(NP).
verb_phrase_internal(vp(V,NP)) --> past_double_verb(V), noun_phrase(NP).

verb_phrase_internal(vp(V,NP1,NP2)) --> past_double_verb(V), noun_phrase(NP1), noun_phrase(NP2).
%% Verb Phrases %%

%% Adjective Phrases %%
adjective_phrase(adj_p(A)) --> adjective(A).
adjective_phrase(adj_p(A,AP)) --> adjective(A), adjective_phrase(AP).
adjective_phrase(adj_p(A1,C,A2)) --> adjective(A1), conjunction(C), adjective(A2).
%% Adjective Phrases %%

%% Adverb Phrases %%
adverb_phrase(adv_p(A)) --> adverb(A).
adverb_phrase(adv_p(A1,C,A2)) --> adverb(A1), conjunction(C), adverb(A2).
%% Adverb Phrases %%

%% Prepositional Phrases %%
prepositional_phrase(pp(P,NP)) --> preposition(P), noun_phrase(NP).
prepositional_phrase(pp(P,NP,PP)) --> preposition(P), noun_phrase(NP), prepositional_phrase(PP).
%% Prepositional Phrases %%

%% Interrogative Pronoun Phrases %%
who_pronoun_phrase(who_q(W,VP)) --> who_pronoun(W), verb_phrase(VP).
who_pronoun_phrase(who_q(W,VP,AP)) --> who_pronoun(W), verb_phrase(VP), adverb_phrase(AP).
who_pronoun_phrase(who_q(W,V1,NP,V2)) --> who_pronoun(W), question_verb(V1), noun_phrase(NP), inf_verb(V2).
who_pronoun_phrase(who_q(W,V1,NP,V2,PP)) --> who_pronoun(W), question_verb(V1), noun_phrase(NP), inf_verb(V2), prepositional_phrase(PP).
who_pronoun_phrase(who_q(W,V1,NP,V2,AP,PP)) --> who_pronoun(W), question_verb(V1), noun_phrase(NP), inf_verb(V2), adverb_phrase(AP), prepositional_phrase(PP).

what_pronoun_phrase(what_q(W,VP)) --> what_pronoun(W), verb_phrase(VP).
what_pronoun_phrase(what_q(W,VP,AP)) --> what_pronoun(W), verb_phrase(VP), adverb_phrase(AP).
what_pronoun_phrase(what_q(W,V1,NP,V2)) --> what_pronoun(W), question_verb(V1), noun_phrase(NP), inf_verb(V2).
what_pronoun_phrase(what_q(W,V1,NP,V2,PP)) --> what_pronoun(W), question_verb(V1), noun_phrase(NP), inf_verb(V2), prepositional_phrase(PP).
what_pronoun_phrase(what_q(W,V1,NP,V2,AP,PP)) --> what_pronoun(W), question_verb(V1), noun_phrase(NP), inf_verb(V2), adverb_phrase(AP), prepositional_phrase(PP).
%% Interrogative Pronoun Phrases %%

%% Object Pronoun Phrases %%
whom_obj_pronoun_phrase(whom_p(W,NP,VP)) --> whom_pronoun(W), noun_phrase(NP), verb_phrase(VP).
whom_obj_pronoun_phrase(whom_p(W,NP,AVP)) --> whom_pronoun(W), noun_phrase(NP), anded_verb_phrase(AVP).
%% Object Pronoun Phrases %%

% 27 Nouns
singular_noun(n(boy)) --> [boy].
singular_noun(n(box)) --> [box].
singular_noun(n(room)) --> [room].
singular_noun(n(school)) --> [school].
singular_noun(n(woman)) --> [woman].
singular_noun(n(man)) --> [man].
singular_noun(n(envelope)) --> [envelope].
singular_noun(n(shed)) --> [shed].
singular_noun(n(building)) --> [building].
singular_noun(n(tree)) --> [tree].
singular_noun(n(girl)) --> [girl].
singular_noun(n(message)) --> [message].
singular_noun(n(ball)) --> [ball].
singular_noun(n(shop)) --> [shop].
singular_noun(n(car)) --> [car].
singular_noun(n(neighbor)) --> [neighbor].
singular_noun(n(family)) --> [family].
singular_noun(n(student)) --> [student].
singular_noun(n(professor)) --> [professor].
singular_noun(n(lecturer)) --> [lecturer].
singular_noun(n(scientist)) --> [scientist].
singular_noun(n(researcher)) --> [researcher].
plural_noun(n(students)) --> [students].
plural_noun(n(professors)) --> [professors].
plural_noun(n(lecturers)) --> [lecturers].
plural_noun(n(scientists)) --> [scientists].
plural_noun(n(researchers)) --> [researchers].

% 22 Verbs
past_double_verb(v(gave)) --> [gave].
past_double_verb(v(brought)) --> [brought].
past_double_verb(v(cooked)) --> [cooked].
past_double_verb(v(sold)) --> [sold].
past_verb(v(pushed)) --> [pushed].
past_verb(v(stored)) --> [stored].
past_verb(v(climbed)) --> [climbed].
past_verb(v(watched)) --> [watched].
past_verb(v(liked)) --> [liked].
past_verb(v(admired)) --> [admired].
past_verb(v(appreciated)) --> [appreciated].
past_verb(v(played)) --> [played].
past_verb(v(said)) --> [said].
past_verb(v(drove)) --> [drove].
past_verb(v(chose)) --> [chose].
past_verb(v(met)) --> [met].
past_verb(v(visited)) --> [visited].
past_verb(v(controlled)) --> [controlled].
past_verb(v(opened)) --> [opened].
past_verb(v(closed)) --> [closed].
past_verb(v(did)) --> [did].
inf_verb(v(do)) --> [do].

question_verb(v(did)) --> [did].

% 20 Adjectives
adjective(adj(young)) --> [young].
adjective(adj(big)) --> [big].
adjective(adj(large)) --> [large].
adjective(adj(empty)) --> [empty].
adjective(adj(old)) --> [old].
adjective(adj(poor)) --> [poor].
adjective(adj(white)) --> [white].
adjective(adj(brilliant)) --> [brilliant].
adjective(adj(talented)) --> [talented].
adjective(adj(bright)) --> [bright].
adjective(adj(dull)) --> [dull].
adjective(adj(old)) --> [old].
adjective(adj(small)) --> [small].
adjective(adj(fast)) --> [fast].
adjective(adj(slow)) --> [slow].
adjective(adj(long)) --> [long].
adjective(adj(short)) --> [short].
adjective(adj(rich)) --> [rich].
adjective(adj(huge)) --> [huge].
adjective(adj(tiny)) --> [tiny].

% 10 Adverbs
adverb(adv(quickly)) --> [quickly].
adverb(adv(slowly)) --> [slowly].
adverb(adv(easily)) --> [easily].
adverb(adv(hardly)) --> [hardly].
adverb(adv(happily)) --> [happily].
adverb(adv(sadly)) --> [sadly].
adverb(adv(loudly)) --> [loudly].
adverb(adv(silently)) --> [silently].
adverb(adv(urgently)) --> [urgently].
adverb(adv(nicely)) --> [nicely].

% 10 Prepositions
preposition(prep(in)) --> [in].
preposition(prep(after)) --> [after].
preposition(prep(behind)) --> [behind].
preposition(prep(on)) --> [on].
preposition(prep(at)) --> [at].
preposition(prep(over)) --> [over].
preposition(prep(below)) --> [below].
preposition(prep(above)) --> [above].
preposition(prep(with)) --> [with].
preposition(prep(without)) --> [without].

% 6 Determiners
singular_det(d(a)) --> [a].
singular_det(d(every)) --> [every].
det(d(the)) --> [the].
det(d(some)) --> [some].
plural_det(d(many)) --> [many].
plural_det(d(few)) --> [few].

% Personal Pronouns
personal_pronoun(pron(she)) --> [she].
personal_pronoun(pron(he)) --> [he].

% Interrogative Pronouns
who_pronoun(pron(who)) --> [who].
what_pronoun(pron(what)) --> [what].

% Object Pronoun
whom_pronoun(pron(whom)) --> [whom].

% Conjunction
conjunction(conj(and)) --> [and].
