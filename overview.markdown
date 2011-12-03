## Parameters

### Generic parameters

- `:BeliefNoise` (0-100) not used

- `:SensorNoise` (0-100) used only in words domain; a value *p* causes
  each letter to have a *p*/100 chance of switching to a random letter
  when reported by the sensor

- `:Knowledge` (0-100) used only in words domain; how much "world
  knowledge" the agent starts with; in the words domain, a value of
  *p* causes the agent to know about only a random *p*% subset of the
  true complete dictionary, and all n-grams that involve unknown words
  are likewise not known to the agent

- `:BelievedKnowledge` (0-100) used only in words domain; how much
  "world knowledge" the agent *believes* it possesses (the actual
  amount of world knowledge the agent possesses is decided by the
  `:Knowledge` parameter)

- `:Learn` (true/false) used only in words domain; whether
  domain-specific "learning" should be possible

- `:Steps` (1+) number of total simulation steps **(truth-changing)**

- `:StepsBetween` (1+) number of simulation steps to wait before the
  agent gathers sensor reports and generates hypotheses

- `:Threshold` (0-100) degree of caution; a value *p* causes the
  abduction engine to refuse to accept a hypothesis if its confidence
  does not surpass the next-most-confident rival by at least *p*/100

- `:TransitiveExplanation` (true or false) used only in words domain;
  whether or not transitive explanation is activated (see below for
  the transitive explanation algorithm)

- `:MetaReasoning` several options, see the 'Metareasoning' section
  of this document for more information:

  - "NoMetareasoning"

  - "BatchBeginning"

  - "Batch5", "Batch4", "Batch3", "Batch2", "Batch1"

### Words domain parameters

- `:MaxModelGrams` (1-10); word transition model size (i.e. unigram,
  bigram, trigram, etc.)

### Tracking domain parameters

- `:GridHeight`, `:GridWidth` (1+); size of grid **(truth-changing)**

- `:MaxWalk` (1+); maximum grid-steps (including diagonals) an entity
  can move in one time-step; a value greater than one means that an
  entity can move several times in one time-step; leave this parameter
  at 10 since the agent only knows the probability distribution of
  random movements if the max walk size is 10 **(truth-changing)**

- `:NumberEntities` (1+); number of (starting) entities in the grid
  **(truth-changing)**

- `:ProbNewEntities` (0-100); probability that a new entity will be
  generated each time step; a value *p* causes a *p*/100 chance, in a
  time step, that an entity will be created in a random location; for
  now, leave this parameter at 0 **(truth-changing)**

- `:SensorCoverage` (0-100); how much of the grid (as a percentage)
  the sensors can "see"; for now, leave this parameter at 100

- `:SensorSeesColor` (0-100); how much of the grid (as a percentage)
  the sensors can report the color of entities; a value *p* causes the
  middle *p*% of the grid to be "greyed-out"
  
## Metrics

### Non-comparative metrics

#### Generic non-comparative metrics

#### Tracking non-comparative metrics

#### Words non-comparative metrics

### Comparative metrics

#### Generic comparative metrics

#### Tracking comparative metrics

#### Words comparative metrics

## Abductive reasoner

### Definitions

#### Definition: Hypothesis

Both "facts" (data received from sensors) and explanations of facts
are "hypotheses," in order to maintain a common abstraction. Each
hypothesis has several attributes: an identifier (a prefix followed by
a number); a type (such as `:sensor` or `:movement`); a function that
identifies which other hypotheses conflict; a tag `:and` or `:or` (or
`nil`) indicating whether the hypothesis can only be accepted
(assuming transitive explanation is disabled) when either all the
hypotheses it explains have already been accepted (`:and`) or at least
one hypothesis it explains has already been accepted (`:or`) or
neither (`nil`, no restriction is enforced); a set of hypotheses that
this hypothesis explains; a description; and domain-specific data.

#### Definition: Hypothesis confidence

Every hypothesis has an associated confidence score (a floating point
number in the range [0.0, 1.0]); sensor data hypotheses typically have
a score of 1.0, while explaining hypotheses start with a
domain-specific *a priori* score. This score may be updated as
explainers are arranged into contrast sets.

#### Definition: Forced hypothesis

Facts, such as sensor data hypotheses, are marked as "forced" (and
simultaneously accepted).

#### Definition: Conflicting hypothesis

When a hypothesis is accepted, its conflicts detection function is
consulted to determine which not-yet-accepted hypotheses are
incompatible with the hypothesis just accepted. These incompatible
hypotheses are rejected when the original hypothesis is accepted.

#### Definition: Immediate explaining hypotheses

For some hypothesis, let *E* be the hypotheses that that could (if
true) directly explain the given hypothesis. The hypothesis's
immediate explainers are those in the set *E* that meet their
individual `:and` or `:or` restrictions (either all of the explainer's
explained hypotheses are already accepted at least one of them is).

#### Definition: Transitive explaining hypotheses

For some hypothesis, its transitive explainers are all those
hypotheses for which there is some path (along the directed graph
represented by explains relations) from the transitive explainer to
the given hypothesis. In other words, the transitive explainers of a
hypothesis are all immediate and distance potential explainers. Note
that `:and` and `:or` restrictions do not apply in this case.

#### Definition: Unexplained hypothesis

An unexplained hypothesis is either an accepted or forced hypothesis
that has potential explainers (possibly transitive explainers) but
none of which have been accepted.

#### Definition: Explainers set (contrast sets)

The explainers set or set of contrast sets is an ordered set of ordered
sets of hypotheses. For each unexplained hypothesis, there is (at
least one) set of explainers. Suppose *H* is unexplained, and that
*A*, *B*, *X*, and *Y* explain *H* (possibly transitively), and
furthermore that *A* and *B* share a "hypothesis type" and *X* and *Y*
share a different hypothesis type (also assume *A* has greater
confidence than *B* and *X* has greater confidence than *Y*). Then the
explainers set contains the ordered sets {*A*,*B*} and {*X*,*Y*}. Each
unexplained hypothesis is represented by (one or more) such sets,
assuming the unexplained hypothesis has available potential
explainers. These sets are contrast sets because the hypotheses in
each set compete for explaining; only explainers of the same type
compete. The contrast sets are ordered such that the set {*A*,*B*}
comes before {*X*,*Y*} if the difference in confidence (known as the
"delta") between *A* and *B* is greater than that between *X* and
*Y*. Thus, the contrast sets are ordered by delta, greatest delta
first.

#### Definition: Essential explainer

Whenever a contrast set contains a single explainer, that explainer is
an essential (for that hypothesis type).

#### Definition: Best explainer

A best explainer is either an essential explainer (which is preferred
over other kinds of explainers) or the explainer with the greatest
delta between it and its next most-confident rival of the same
hypothesis type (as read off the contrast set). Note that a best
explainer may still not be accepted if the delta does not surpass the
`:Threshold` parameter.

### Finding explanations

The abductive reasoner's task is complete when it is unable to find
any sufficiently-confident explainers for unexplained hypotheses, or
there are no unexplained hypotheses. The idea is that facts must be
explained, and if an explainer has its own explainers, then it
likewise should be explained if it is accepted.

Every "explains cycle" searches for potential explainers. For each
fact and any other unexplained hypotheses, a search is performed for
possible explainers. This search yields the explainers set. The best
explainer, if its delta is greater than or equal to the threshold, is
accepted.

When accepting a hypothesis, we determine which hypotheses are
explained, and thus removable from the explains graph, via the
following logic:

  - If transitive explanation is disabled, the accepted hypothesis and
    those hypotheses it explains that explain nothing (nothing that
    hasn't already been explained) are removed.

  - If transitive explanation is enabled, then we remove the
    hypotheses it (transitively) explains only when a certain
    condition is met. Whatever the transitive explainer explains
    "unambiguously" can be accepted and removed from the explains
    graph. A hypothesis is unambiguously explained by a transitive
    explainer if every path through the explains graph from the
    hypothesis to the transitive explainer "goes through" or "meets"
    the hypothesis. This implies the hypothesis is necessary for the
    transitive explainer, and thus can be accepted and considered
    explained. All hypotheses failing this condition are not accepted
    but are considered explained.
    
Hypotheses that conflict with the accepted explainer are rejected.

### Transitive explanation

Transitive explanation is a parameter. If `:TransitiveExplanation` is
`true`, the following logic is used:

  - Construct two sets of contrast sets: one containing only immediate
    explainers, the other containing transitive explainers (which
    includes immediate explainers).

  - Find the best explainer in each set of contrast sets. (Due to the
    way the contrast sets are ordered, the first contrast set has the
    greatest delta between the first explainer in the contrast set and
    the second).

  - If the best explainer in the "immediate" set is an essential,
    prefer that explainer.

  - Otherwise, if the best explainer in the "transitive" set is an
    essential, prefer that explainer.

  - Otherwise, if there is no best explainer in the "immediate" set
    (because, say, threshold was not reached), prefer the best
    explainer in the transitive set (there may be no such explainer;
    in which case, there is no explainer in either set, so of course
    there is nothing else to be done).

  - Otherwise, if there is no best transitive explainer, prefer the
    best immediate explainer.

  - Otherwise, if the best immediate explainer has a "delta" better
    than the best transitive explainer, prefer the immediate
    explainer.

  - Otherwise, finally, we arrive at a situation where the transitive
    explainer has a "delta" at least as good as the immediate
    explainer, so we prefer the transitive explainer.
    
## Epistemic states

### Doubt

The "doubt" manifest in an epistemic state is a function of the
confidence of the accepted explainers. Specifically, doubt is
calculated by averaging the doubt of each accepted hypothesis
(excluding forced hypotheses), where doubt of an accepted hypothesis
equals 1.0 minus the confidence of the hypothesis. Thus, doubt is a
measure between 0.0 and 1.0.

If the epistemic state contains no accepted hypotheses and nothing is
unexplained (because there were no "facts"), then doubt is
0.0. Otherwise, if there are no accepted hypotheses but some facts are
unexplained, then doubt is 1.0 (maximum).

## Metareasoning

The criteria for activating metareasoning is the following:

  - there are some hypotheses that have no explainers; OR

  - the number of unexplained hypotheses is greater than 10% of the
    total number of hypotheses; OR

  - the "doubt" of the workspace is greater than 0.10

### Metareasoning strategies

**BatchBeginning** -- go back to the root ep-state (time 0), clear
  hypotheses and "batch" all the way back to the current time.

**BatchN** (for *N*=1,2,3,4,5) -- go back *N* ep-states, clear
  hypotheses and "batch" all the way back to the current time.

### Metareasoning branch acceptance

A metareasoning branch *A* is accepted over another branch *B* if the
percent of unexplained hypotheses in branch *A* is less than the
percent of unexplained hypotheses in branch *B*; or, if the quantities
are equal, then branch *A* is accepted if its doubt is less than the
doubt of branch *B*.
