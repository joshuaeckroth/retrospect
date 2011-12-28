## Abductive reasoner

### Definitions

#### Definition: Hypothesis

Both "facts" (data received from sensors) and explanations of facts
are "hypotheses," in order to maintain a common abstraction. Each
hypothesis has several attributes: an identifier (a prefix followed by
a number); a type (such as `:sensor` or `:movement` or `:word`); a
subtype (such as `:learned-word`); a function that identifies which
other hypotheses conflict; a tag `:and` or `:or` (or `nil`) indicating
whether the hypothesis can only be accepted (assuming transitive
explanation is disabled) when either all the hypotheses it explains
have already been accepted (`:and`) or at least one hypothesis it
explains has already been accepted (`:or`) or neither (`nil`, no
restriction is enforced); a set of hypotheses that this hypothesis
explains; a description; and domain-specific data.

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

## Parameters

### Generic parameters

  - `:BeliefNoise` (0-100) not used

  - `:SensorNoise` (0-100) used only in words domain; a value *p*
    causes each letter to have a *p*/100 chance of switching to a
    random letter when reported by the sensor

  - `:Knowledge` (0-100) used only in words domain; how much "world
    knowledge" the agent starts with; in the words domain, a value of
    *p* causes the agent to know about only a random *p*% subset of
    the true complete dictionary, and all n-grams that involve unknown
    words are likewise not known to the agent

  - `:BelievedKnowledge` (0-100) used only in words domain; how much
    "world knowledge" the agent *believes* it possesses (the actual
    amount of world knowledge the agent possesses is decided by the
    `:Knowledge` parameter)

  - `:Learn` (true/false) used only in words domain; whether
    domain-specific "learning" should be possible

  - `:Steps` (1+) number of total simulation steps
    *(truth-changing)*

  - `:StepsBetween` (1+) number of simulation steps to wait before the
    agent gathers sensor reports and generates hypotheses

  - `:Threshold` (0-100) degree of caution; a value *p* causes the
    abduction engine to refuse to accept a hypothesis if its
    confidence does not surpass the next-most-confident rival by at
    least *p*/100

  - `:TransitiveExplanation` (true or false) used only in words
    domain; whether or not transitive explanation is activated (see
    below for the transitive explanation algorithm)

  - `:MetaReasoning` several options, see the 'Metareasoning' section
    of this document for more information:

    - "NoMetareasoning"

    - "BatchBeginning"

    - "Batch5", "Batch4", "Batch3", "Batch2", "Batch1"
    
  - `:AnalyzeSensitivity` (true or false); add extra metrics
    (domain-independent) that attempt to measure the sensitivity of
    hypotheses

### Words domain parameters

  - `:MaxModelGrams` (1-10); word transition model size (i.e. unigram,
    bigram, trigram, etc.)
    
  - `:LearnFeatureSize` (1+); letter transition size (like an n-gram
    model on words) that are recorded for each word in the dictionary;
    these transition frequencies are normalized and use to calculate
    word similarities between known words and a potential learned
    word
    
  - `:MinWordLength` (1+); the length of the shortest word that the
    agent will know about and will be found in the data (including the
    truedata) *(truth-changing)*
    
  - `:MinLearnLength` (1+); the length of the shortest word that the
    agent will consider learning; the agent is promised to know about
    all true words shorter than this length *(truth-changing)*
    
  - `:MaxLearnLength` (1+, greater than `:MinLearnLength`); the length
    of the longest word that the agent will consider learning; no
    words longer than this length will be in the truedata; this
    prevents combinatorial explosions *(truth-changing)*

### Tracking domain parameters

  - `:GridHeight`, `:GridWidth` (1+); size of grid
    *(truth-changing)*

  - `:MaxWalk` (1+); maximum grid-steps (including diagonals) an
    entity can move in one time-step; a value greater than one means
    that an entity can move several times in one time-step; leave this
    parameter at 10 since the agent only knows the probability
    distribution of random movements if the max walk size is 10
    *(truth-changing)*

  - `:NumberEntities` (1+); number of (starting) entities in the grid
    *(truth-changing)*
    
  - `:KnowBiases` (true or false); whether or not the agent knows the
    entities' biases (left/right/straight) and thus need not
    hypothesize them
    
  - `:PathBranches` (1+); how many branches to hypothesize at each
    point where more than one movement may continue a path; there may
    be a combinatorial explosion if this parameter is large

  - `:ProbNewEntities` (0-100); probability that a new entity will be
    generated each time step; a value *p* causes a *p*/100 chance, in
    a time step, that an entity will be created in a random location;
    for now, leave this parameter at 0 *(truth-changing)*

  - `:SensorCoverage` (0-100); how much of the grid (as a percentage)
    the sensors can "see"; for now, leave this parameter at 100

  - `:SensorSeesColor` (0-100); how much of the grid (as a percentage)
    the sensors can report the color of entities; a value *p* causes
    the middle *p*% of the grid to be "greyed-out"
  
## Metrics

### Non-comparative metrics

Description of what a non-comparative metric is...

#### Generic non-comparative metrics

  - *MetaActivations*: number of times the activate metareasoning
      strategy was activated (consulted); this value is always greater
      than or equal to *MetaAccepted*

  - *MetaAccepted*: number of times a metareasoning alternative
      reasoning history was accepted

  - *Milliseconds*: time required from start of agent's
      hypothesizing to the end of abductive reasoning and
      metareasoning

  - *Unexplained*: count of unexplained hypotheses in last abduction
      workspace

  - *UnexplainedPct*: percent of hypotheses that are unexplained;
      this is calculated by dividing the unexplained count
      (*Unexplained* metric; see above) by the sum of the number of
      forced hypotheses (sensor data) and the number of accepted
      hypotheses that have explainers but remain unexplained

  - *NoExplainers*: number of forced hypotheses (sensor data) that
      have no potential explainers; this means the agent never offered
      explainers of those forced hypotheses

  - *SharedExplains*: number of hypotheses that do not uniquely
      explain (regardless of acceptance)

  - *ExplainCycles*: number of accept/reject cycles in the most
      recent abduction workspace

  - *HypothesisCount*: number of hypotheses (including forced) in
      the most recent abduction workspace

  - *Compute*: provided by the agent

  - *Memory*: provided by the agent

  - *DeepestDep*: deepest depth in the dependency graph
  
  - *AvgTrueSensitivity*: average sensitivity of true hypotheses
  
  - *AvgFalseSensitivity*: average sensitivity of false hypotheses

#### Tracking non-comparative metrics

Definitions:

  - *true postive (TP)* movements: those that are both believed (accepted)
    and actually occurred

  - *false positive (FP)* movements: those that are believed but did not
    occur; *FP* = *count of believed* - *count of true positive*

  - *true negative (TN)* movements: those that are disbelieved (rejected)
     and actually did not occur; *TN* = *count of disbelieved* -
     *count of false negative*

  - *false negative (FN)* movements: those that are disbelieved but did
     occur

Metrics:

  - *PEC*: percent of true movements that are believed movements

  - *PEW*: percent of true movements that are not believed movements
      (but not necessarily disbelieved movements)

  - *Prec*: *TP* / (*TP* + *FP*)

  - *Recall*: *TP* / (*TP* + *FN*)

  - *Spec*: *TN* / (*TN* + *FP*)

  - *Acc*: (*TP* + *TN*) / (*TN* + *TP* + *FN* + *FP*)

  - *IDCorrect*: number of believed entity locations that are correct

#### Words non-comparative metrics

  - *LD*:
  
  - *Correct*:
  
  - *LearnedCount*:
  
  - *LearnedCorrect*:

### Comparative metrics

#### Generic comparative metrics

*MetaActivations*, *MetaAccepted*, *Milliseconds*,
*SharedExplains*, *Unexplained*, *UnexplainedPct*, *NoExplainers*,
*ExplainCycles*, *HypothesisCount*, *Compute*, *Memory*, *DeepestDep*

#### Tracking comparative metrics

*PEC*, *PEW*, *Prec*, *Recall*, *Spec*, *Acc*, *IDCorrect*

#### Words comparative metrics

*LD*, *Correct*, *LearnedCount*, *LearnedCorrect*
    
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

Metareasoning is tasked with detecting when a reasoning mistake has
(likely) been committed, choosing what will be done about it, and
ultimately constructing an alternative reasoning history (which may
include a new branch off of some prior epistemic state).

### Metareasoning strategies

The parameter `:Metareasoning` chooses which strategy will be active.

  - *BatchBeginning* -- go back to the root ep-state (time 0), clear
    hypotheses and "batch" all the way back to the current time.

  - *BatchN* (for *N*=1,2,3,4,5) -- go back *N* ep-states, clear
    hypotheses and "batch" all the way back to the current time.

### Metareasoning activation

The criteria for activating metareasoning is the following:

  - there are some hypotheses that have no explainers; OR

  - the number of unexplained hypotheses is greater than 10% of the
    total number of hypotheses; OR

  - the "doubt" of the workspace is greater than 0.10

### Metareasoning branch acceptance

A metareasoning branch *A* is accepted over another branch *B* if the
percent of unexplained hypotheses in branch *A* is less than the
percent of unexplained hypotheses in branch *B*; or, if the quantities
are equal, then branch *A* is accepted if its doubt is less than the
doubt of branch *B*.

## Dependency graph

Every hypothesis depends on zero or more prior hypotheses (typically
only accepted hypotheses and hypotheses under consideration when the
depending hypothesis was generated). For example, a tracking domain's
"entity location" hypothesis commonly depends on a prior location
hypothesis (the entity's prior location) plus the path hypothesis or
hypotheses that form the basis of the location hypothesis.

These dependencies are stored in the epistemic state as a directed
graph. The agent has access to the dependency graph. The agent may
ask questions of this graph such as the following:

  - For some hypothesis, what does it depend on?
  
  - For some hypothesis, which hypotheses (possibly from the past) of
    the same type share dependencies with this hypothesis, yet are not
    members of the dependency hierarchy starting from this hyp? (That
    is, which hypotheses of the same type share children with this
    hypothesis's children yet are included in this hypothesis's
    children?)

  - 

### Tracking domain

  - A sensor hypothesis depends on nothing.
  
  - A movement hypothesis depends on its first and second detection
    (sensor hypotheses).

  - A path hypothesis depends on its movements.

  - A location hypothesis depends on its paths and the prior accepted
    location hypothesis (if any) that placed the entity at the prior
    location, and its bias hypothesis if any.

  - A bias hypothesis depends on its location hypotheses.
  
## Tracking domain

The goal of the tracking domain is to identify entities' beginning &
end positions. At some time t, the tracker is scored by evaluating how
many of the entities are believed to hold their true locations at time
t. At time 0, the tracker is told the true locations of all the
entities. Then, as time progresses, the tracker tries to "follow" the
entities (utilizing its knowledge, which may be incomplete, about
possible entity motions). At a later time t, the tracker is evaluated
in terms of how many of its believed entity positions are accurate.

To do its job, the tracker offers three levels of hypotheses for every
collection of sensor data. The first level is movement hypotheses:
something moves from position x to position y, across one time
click. The second level is entity paths: some entity moved from its
old location through one or more movements to another location. The
third level is entity locations: after a series of movments, the
entity is at some location at some time. The location hypotheses
explain one or more path hypotheses. The path hypotheses explain one
or more movement hypotheses; the movement hypotheses explain sensor
detections. If transitive explanation is employed, then in cases where
a location hypothesis can be realized by several incompatible path
hypotheses, the location hypothesis can still be accepted (without
first disambiguating the path hypotheses). The tracker is only
ultimately interested in location hypotheses, and only when it has
accepted some location hypotheses will it update its beliefs (with new
location beliefs).

The tracker reports its beliefs as "Entity 0 is at 5,2 at time 8," for
example, for each entity.

## Words domain

Sensor detections in the words domain come in the form of a string of
letters (each detection is an individual letter). These letters
correspond to letters of the true words, although the letters sensed
may not be the true letters (depending on the parameter
`:SensorNoise`). Each sensed letter corresponds to a single true
letter (there are no additions or deletions).

The task of the words domain hypothesizer is to offer hypotheses about
which words the letters represent (i.e. find word boundaries). Finding
the true words is somewhat ambiguous because letter sequences like
"looking" may come from the words "look in g---" (for some word
starting with "g") or "looking." The task is much more ambiguous when
`:SensorNoise` is positive, so the true words "look in glass" may
appear, for example, as "lokking" (with only the "g" of "glass" being
presented, as before). Since the first "lok" does not match the start
of any word, the hypothesizer may consider it to be noise, and offer
"locking" and "looking" and "look in g---" and "lock in g---," etc. as
alternative explanations of the letter sequence.

### Algorithm

A high-level overview of the hypothesizer follows. Assume the agent
has no beliefs resulting from past reasoning (i.e. the simulation just
started). The sensors are queried for all detections up to "now." All
possible word extractions are found, assuming no noise. Each word is
established as a hypothesis (that explains the letters, i.e. sensor
detections, that make up the word). The score of each word (between
0.0 and 1.0) is a function of how closely the word matches the sensor
detections and the word's *a priori* score (from the unigram model of
the text). Word hypotheses conflict with other word hypotheses that
explain the same sensor detections (words that use the same letters
from the same positions in the stream).

Then composite hypotheses are constructed from these word
hypotheses. A composite hypothesis is a sequence of words and explains
each of the word hypotheses. The word sequence must not have any gaps
in order to be a composite. Composite hypotheses are scored by
referring to the appropriate *n*-gram model (where *n* is the length
of the composite). Note that composite hypothesis scores do not take
into account how closely the words match the sensor detections---the
word hypotheses' scores already account for that. Composite hypotheses
conflict with other composite hypotheses and word hypotheses that do
not overlap (i.e. two composites conflict if they cover some common
part of the stream but not using the same words, and a composite
hypothesis and word hypothesis conflict if the two hypotheses overlap
in some part of the stream and the composite hypothesis does not
explain the word hypothesis, meaning the word is not part of the
composite).

### Hypothesis scoring

  - Word hypothesis: let *w* be the word and *S* be a set of similar
    words, where a similar word is a word in the agent's dictionary
    that contains *w*; the probability of the hypothesis is the
    maximum of 0.001 and *(P(w)/sum(P(s))((|w|-c)/|w|)* where *P(w)*
    is the unigram probability of word *w*, *P(s)* is the probability
    of each *s* in the similar word set *S*, *|w|* is the length of
    *w* and *c* is the number of changes (accounting for noise) that
    the sensor data underwent to match *w*.
    
  - Learned word hypothesis for word *w*: *1.0 - B^(|w|/4)* where *B*
    is the percent of knowledge the agent believes it possesses
    (`:BelievedKnowledge` parameter) and *|w|* is the length of the
    word *w*.
  
  - Word sequence hypothesis:
  
### Learning

A learned words score is <i>B * S</i> where *B* is the agent's
estimate of its own knowledge (a value in the range [0.0, 1.0]) and
*S* is the similarity of the word with known words, based on letter
transition frequences and the cosine similarity metric. Letter
transitions are essentially stored in normalized vectors, one such
feature vector for each known word. The potential learned word's
feature vector is compared with a centroid of known word feature
vectors using the cosine similarity calculation. The value for *S* is
in the range [0.0, 1.0].

Additionally, a word will not be learned if it is within three steps
from the end of a collection of sensor reports (since there is a high
chance the letters at the cutoff will be part of a different word).

Finally, learned words must have length at least `:MinLearnLength`.


## Robustness analysis

### Interactive analysis

Choose a hypothesis in the Player logs, click its "Analyze"
button. Depending on the nature of the hypothesis, a particular
robustness analysis occurs and the results are displayed.

  - *The hypothesis was originally accepted*: Let *H* be the set of
     hypotheses that this hypothesis directly explains.
     
  - *The hypothesis h was originally rejected*: Let *H* be the
     intersection of the set of accepted hypotheses and the set of
     hypotheses that conflict with *h* (the hypothesis being
     analyzed).
  
Let *P* be a set of combinations of *H* with sizes 1-4 (so it includes
each hypothesis in *H*, each pair of hypotheses in *H*, etc.).

For each set *p* in *P*, first clear the abduction workspace (clear
all accepted/rejected hypotheses except sensor data, which retains its
accepted state), and then reject the hypotheses in *p*. Perform
abduction (find explainers for the sensor data).

The result of the analysis depends on the nature of the hypothesis
being analyzed:

  - *The hypothesis h was originally accepted*: if the hypothesis is
    now rejected, add *p* to the set of hypothesis groups that cause
    *h* to be rejected.

  - *The hypothesis h was originally rejected*: if the hypothesis is
     now accepted, add *p* to the set of hypothesis groups that cause
     *h* to be accepted.
     
  - In either case, if the hypothesis is now unaccepted, add *p* to
    the set of hypothesis groups that cause the hyp to be unaccepted.

### Batch analysis

#### Sensitivity to sensor reports

For each epistemic state, after the abductive process has completed, a
sensitivity analysis may be automatically performed (if the parameter
`:AnalyzeSensitivity` is `true`). For each hypothesis (except "forced"
hypotheses; i.e. sensor data), a number is calculated to determine the
hypothesis's "sensitivity." This number is calculated in the following
way:

  - Create an epistemic state branch off the previous epistemic state
    and re-introduce a random 50% of sensor reports as *perturbed*
    sensor reports (the other half is not perturbed); the domain
    provides the means to perturb a sensor report.
    
  - Simulate hypothesis generation and abductive reasoning in this
    branched epistemic state.
    
  - Determine if the hypothesis retains its status
    (accepted/rejected/unaccepted); the domain provides the means to
    compare hypotheses (with distinct identifiers) for equality.
    
  - If the hypothesis retains its state, its sensitivity is 0.
    Otherwise, its sensitivity is 1.
    
Obviously, this process is performed only once for all hypotheses, not
once for each hypothesis. The metrics *AvgTrueSensitivity* and
*AvgFalseSensitivity* find the average of the sensitivities for true
and false hypotheses.
