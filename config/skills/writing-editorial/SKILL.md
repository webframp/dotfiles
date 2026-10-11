---
name: writing-editorial
description: >
  Editorial principles for prose, drawing on Benjamin Dreyer (grammar and
  correctness) and Verlyn Klinkenborg (the sentence: sound, economy, and what
  it actually says). Use when writing, editing, revising, or reviewing prose:
  blog posts, documentation, MR/PR/commit descriptions, issue comments, emails,
  release notes, report copy, or any request to "make this read better,"
  tighten wording, fix tone, or check style. Covers parallelism within lists,
  actor subjects and strong verbs, nominalizations, ambiguity, sentence
  variation, and anti-patterns (defensive framing, aphoristic restatement,
  logical indicators, transitional and throat-clearing sentences, template
  repetition, intensifiers, volunteer sentences and cliches, anthropomorphizing
  code). Not for code logic, only its prose.
---

# Editorial Principles

Two layers govern every revision. **Benjamin Dreyer** supplies correctness: the
grammar that keeps a sentence from saying something it does not mean. **Verlyn
Klinkenborg** (*Several Short Sentences About Writing*) supplies the sentence:
its sound, its economy, and the discipline of knowing exactly what it says.

The governing test, from Klinkenborg, applies to every sentence:

> Know what each sentence says, what it doesn't say, and what it implies. Of
> these, the hardest is knowing what each sentence actually says.

**Economy comes first.** When any rule below would produce a longer sentence
that carries no more meaning, the rule does not apply. Revise toward removing
words, not adding them.

## 1. Dreyer: The Correctness Layer

Correct grammar is the precondition for being sure a sentence says what you
think it says. Learn the rules so they never obtrude on a reader because you
ignored them.

### 1.1. Parallelism Within a List

When a list of three or more elements is introduced by an infinitive (`to` +
verb) or joined by a coordinating conjunction, every element shares the same
grammatical form.

* **Error:** `to **strengthen**, **encourage**, **solidifying**, and
  **fostering**`
* **Correction:** find the governing structure (here, the infinitive `to`) and
  convert every item to match: `to **strengthen**, **encourage**,
  **solidify**, and **foster**`.

This rule governs the inside of a list. It does not license parallel patterns
across sentences; see 3.4.

### 1.2. Choose the Form by the List's Function

Before fixing a list's form, identify its job in the sentence:

* If the list names **activities, qualities, or concepts** (functioning as a
  noun or adjective), use **gerunds/participles**: "*Encouraging*
  collaboration is difficult."
* If the list states **actions, commands, or purposes** (functioning as a verb
  or part of an infinitive), use **base verbs**: "We must *encourage*
  collaboration."

When a series states goals or actions, default to the base verb. It is more
direct and more economical.

### 1.3. Grammar That Causes Ambiguity

These errors create a second, wrong reading. Fix every one:

* **Dangling opening modifiers.** A modifying phrase at the start of a sentence
  must modify the subject. "After seven hours of zooming and twisting through
  the air, the day had come to its end" says the day was flying.
* **Pronouns with the wrong antecedent.** A pronoun points to the nearest
  plausible noun, whatever you intended. Check every `it`, `this`, `which`,
  and `they`.
* **Misplaced phrases.** Put every phrase next to what it modifies. "Displayed
  in glass cases and lit up like jewelry, tuna is cherished in Japan" reads
  better with the tuna first.
* **Shifted person or tense.** A sudden "you" in a first-person passage, or
  layered past tenses ("had started planning... since"), confuses the reader.
  Prefer the simple past and give clear time cues.
* **Wrong words.** "Culminated into," "distinguishable" for "distinctive,"
  "random" for "unfamiliar." Look up even familiar words when you have a
  doubt, and especially when you don't.

## 2. Klinkenborg: The Sentence Layer

### 2.1. Write for Both Readers

Every reader is two readers at once. One reads with overwhelming literalness:
if a sentence offers two paths, this reader takes the wrong one, and stumbles
over every misspelling and syntactic miscue. The other is intelligent,
curious, and will follow you anywhere as long as the prose is clear. Write for
both together. The literal reader catches ambiguity; the trusting reader frees
you from over-explaining.

### 2.2. Listen

The ear catches what the eye misses, because the eye cannot see rhythm or hear
unwanted repetition. Read the text aloud, plainly, and stop wherever something
sounds off.

That faint unease, a "small internal quaver," is the most useful evidence you
will get. You may not be able to name its cause at first. Find it anyway. The
cause may be ambiguity, awkward syntax, a weak verb, or the sentence's relation
to some *other* sentence, not the sentence itself.

### 2.3. Vary Length and Structure

"Variation is the life of prose, in length and in structure." Look for
sameness and work against it. You would not repeat the same word over and over,
so do not repeat the same sentence shape. Repetition is acceptable only when
the reader can feel the reason for it, and even then it should be less parallel
than it could be.

**Self-check:** put each sentence on its own line. Look at the column. Do the
lengths vary? How many sentences open the same way (with the subject, with
"When/Since/While/Because," with "There" or "It")? How many share the same
skeleton?

### 2.4. Short Sentences, Not Choppy Ones

Short sentences restore the directness of subject and verb and make ambiguity
easier to see. "Only a string of choppy sentences sounds choppy." The common
failure is the opposite: the overcrowded sentence, huddling related ideas
together because the writer fears the reader will get lost at a period. Most
overcrowded sentences break apart easily. Long sentences are welcome when they
are as clear and direct as short ones; a strong long sentence is strong short
sentences joined.

Most sentences need no preamble. Keep the space between the period and the
next subject empty.

### 2.5. Subjects That Act, Verbs That Fit

* **The subject should be an actor capable of the verb.** "Bread, rice, and
  bananas constituted my diet" becomes "I ate mostly bread, rice, and
  bananas." Passive constructions hide the actor and absolve everyone of
  responsibility; use them only when the actor truly does not matter.
* **Never substitute noun phrases for verbs.** "The watching of," "an
  appreciation of," "the realization that," "made a decision to" all drain
  energy from a sentence. Turn them back into verbs.
* **Prefer energetic verbs to ones that merely connect, arrange, present, or
  relate.** Watch "to be" verbs, "constitute," "reside," "serve as."
  "Is" is fine when it is the plain, honest verb; do not dodge it with
  "resides."
* **The verb must fit its subject literally.** Rinds don't squat; steeples
  don't sit; a school doesn't reside. In technical prose, "the function
  returns" fits. "The schema knows" does not; see 3.10.

### 2.6. Names and Metaphor

* **Use the names of things.** Category words hide the specific: "colorful
  flora," "an array of," "the architecture" (for the buildings). In technical
  prose, name the actual service, command, flag, or file.
* **A metaphor must be literally accurate,** and the less you use one, the
  stronger it is. Do not extend a metaphor across several sentences. Do not
  let a literal noun perform a metaphorical action mid-sentence.

### 2.7. Diagnostic Questions

Run these against a draft, sentence by sentence:

* Does the sentence open with "There is" or "It was"? ("There is an old man
  who lives there" becomes "An old man lives there.")
* Does an opening "While," "Since," or "Because" steer the reader before the
  sentence starts? ("Since" also carries an unwanted overtone of "because.")
* Are the nouns abstractions ending in "-ion," or the names of actual things?
* How far is the subject from its verb? Does an inserted phrase stall it?
* Is "as" forcing a false simultaneity? It is nearly always trouble; "and," a
  period, or nothing usually works better.
* Is "with" posing as a conjunction or relative pronoun? It can only be a
  preposition. ("Los Angeles is the largest city in California with a
  population of over four million" is about a set of cities.)
* Is "due to" or "from... to" doing work a verb or a plain list should do?
* Are there empty frequency or category adjectives ("occasional," "random,"
  "typical")? "Occasional cars flash past" becomes "Cars flash past now and
  then."
* Is an adverb redundant with its verb ("wobbled uncertainly")?
* Does the sentence make explicit what is already implicit ("the awning of
  branches that hung over us")?
* Is there false specificity ("decreasing exponentially") or false logic?
* Can the sentence do without "the"? Try removing it.
* Are words repeated unintentionally?
* Does a sentence promise something the next one fails to deliver? "If there is
  one landmark here, it is not..." promises the landmark.
* Can the sentence be broken in two or three?
* Can you imagine saying it aloud? Adjust it until you can.

### 2.8. How to Revise

* **The simplest revision is deletion.** Every word is optional until it proves
  essential. Remove words one by one and listen for what is lost or gained.
* **Reimagine; do not shuffle.** "You can almost never fix a sentence by using
  only the words it already contains." If they were the right words, the
  sentence would not need fixing. The same holds for paragraphs.
* **Ask, "What exactly am I trying to say?"** The answer is often the sentence
  to write down.
* **Revise toward** brevity (remove words), directness (nothing evasive or
  periphrastic), simplicity (in construction and word choice), clarity (watch
  for ambiguity), rhythm (where it is lacking), literalness (as an antidote to
  obscurity), implication (the sentence's silent utterance), variation
  (always), silence (leave some), the names of things, and presence (the quiet
  authority of the prose).

## 3. Anti-Patterns

Each pattern below violates a principle above. Avoid them while drafting and
flag them in review. When a fix would add words without adding meaning, the
economy rule wins.

### 3.1. Defending Against Criticism the Reader Has Not Made

"This is not a gimmick." If it is not, proceed. Preempting objections that do
not exist distrusts the reader (2.1).

### 3.2. Aphoristic Summaries and Restated Conclusions

Do not follow a concrete explanation with a one-line restatement; the concrete
version is always stronger. The same applies at the scale of a piece: do not
save the point for the end, and do not end by restating it. Lead with the
point. This matters most in PR/MR descriptions and reports.

### 3.3. Template Phrases Repeated Across Items

State a structural pattern once or twice, then trust the reader to carry it.
Three or four repetitions of "The same pattern you would use to..." read as
generated, not authored (2.3).

### 3.4. Parallel Patterns Across Sentences

Parallelism and contrast patterns that cast themselves forward into later
sentences "seem to offer structure and guidance, but they're tying your hands."
They also force weak verbs: "Mornings are had in loneliness, and evenings are
had in relief" sacrifices its verb to keep the parallel. When a sentence feels
obliged to take a predetermined shape, question the obligation.

### 3.5. Announcing Structure, Transitional Sentences, Throat-Clearing

"No sentence can afford to be merely transitional." Topic sentences and
journalistic nut grafs exist only to announce what comes next. "This is X
applied to Y:" or "The X metaphor has a natural endpoint" label a structure
instead of showing it. Openings that generalize before starting are the same
fault: begin the piece, do not introduce it. If the prose is clear, the reader
will not get lost between sentences.

### 3.6. Logical Indicators

*In fact, Indeed, Moreover, However, Therefore, Thus, Of course, On the one
hand, On the other hand, Whereas.* These words "insist upon logic whether it
exists or not." They usually come first, steering the reader from the front as
if the reader could not follow a turn mid-sentence. If the order is sound, it
needs no indicators; it will be obvious when one sentence negates or affirms
another.

* Remove "but" where you can and check whether the reversal still reads.
* Prefer "but" to "however," except where rhythm, formality, or context argue
  otherwise. A sentence may begin with "But" (or "And").

### 3.7. Intensifiers and Hyperbole

"Truly," "sacred," "utmost," "incredibly," "deeply." A sentence that insists
"dies by overemphasis." Let it relax and trust the reader to take the point.
Hyperbole used for humor almost never succeeds.

### 3.8. Stacked Short Declaratives That Restate One Point

"X did not do this. Y did not do that. Z produced it." Short sentences are
fine; restatement and identical shape are the fault. The three-beat staccato of
interchangeable sentences is the most recognizable AI cadence in technical
prose.

* **Test:** does each sentence say its own piece and leave the stage, or do
  they repeat one point with slight variation?
* **Fix:** cut the sentences that restate. Vary the shape of the ones that
  remain. Combine into one sentence only when the result is no longer and no
  more crowded (2.4).
* Short declaratives that each carry one complete idea, where the last breaks
  the rhythm or pivots the argument, earn their brevity.

### 3.9. Volunteer Sentences and Cliches

Some sentences "volunteer a shape of their own and supply their own words."
They are nearly always unacceptable: predictable in structure, built from
phrases that cluster together "naturally." A cliche is "the debris of someone
else's thinking," and irony or quotation marks do not rescue it. Generated
prose is especially prone to volunteer sentences. Treat any phrase that arrived
without a decision as a candidate for replacement.

### 3.10. Anthropomorphizing Code Artifacts

Models, functions, and schemas do not "know," "understand," "want," or
"care." Say what they are or what they do. This is a case of 2.5: the verb must
fit its subject literally.

## 4. House Style

These rules come from the user, not from Dreyer or Klinkenborg. Apply them
anyway:

* No "It is not X. It is Y." constructions, singly or repeated.
* No stacked declaratives restating one point, especially in closings (see
  3.8).
