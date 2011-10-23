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

- `:Learn` (true/false) used only in words domain; whether
  domain-specific "learning" should be possible

- `:Steps` (1+) number of total simulation steps

- `:StepsBetween` (1+) number of simulation steps to wait before the
  agent gathers sensor reports and generates hypotheses

- `:Threshold` (0-100) degree of caution; a value *p* causes the
  abduction engine to refuse to accept a hypothesis if its confidence
  does not surpass the next-most-confident rival by at least *p*/100

- `:TransitiveExplanation` (true or false) used only in words domain;
  whether or not transitive explanation is activated (see below for
  the transitive explanation algorithm)

- `:MetaReasoning` several options, see the 'Meta-reasoning' section
  of this document for more information:

  - "NoMetaReasoning"

  - "BatchBeginning"

  - "Batch5", "Batch4", "Batch3", "Batch2", "Batch1"

### Words domain parameters

- `:MaxModelGrams` (1-10); word transition model size (i.e. unigram,
  bigram, trigram, etc.)

### Tracking domain parameters

- `:GridHeight`, `:GridWidth` (1+); size of grid

- `:MaxWalk` (1+); maximum grid-steps (including diagonals) an entity
  can move in one time-step; a value greater than one means that an
  entity can move several times in one time-step; leave this parameter
  at 10 since the agent only knows the probability distribution of
  random movements if the max walk size is 10

- `:NumberEntities` (1+); number of (starting) entities in the grid

- `:ProbNewEntities` (0-100); probability that a new entity will be
  generated each time step; a value *p* causes a *p*/100 chance, in a
  time step, that an entity will be created in a random location; for
  now, leave this parameter at 0

- `:SensorCoverage` (0-100); how much of the grid (as a percentage)
  the sensors can "see"; for now, leave this parameter at 100

- `:SensorSeesColor` (0-100); how much of the grid (as a percentage)
  the sensors can report the color of entities; a value *p* causes the
  middle *p*% of the grid to be "greyed-out"

## Abductive reasoner

### Transitive explanation

Transitive explanation is a parameter. If `TransitiveExplanation` is
`true`, the following logic is used:

  - Construct two sets of contrast sets: one containing only immediate
    explainers, the other containing *only* transitive explainers.

  - Find the best explainer in each set of contrast sets. (Due to the
    way the contrast sets are ordered in the larger set, the first
    contrast set has the greatest delta between the first explainer in
    the contrast set and the second).

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

## Meta-reasoner

The criteria for activating meta-reasoning is the following:

  - there are some hypotheses that have no explainers; OR

  - the number of unexplained hypotheses is greater than 10% of the
    total number of hypotheses; OR

  - the "doubt" of the workspace is greater than 0.10

### Strategies

**BatchBeginning** -- go back to the root ep-state (time 0), clear
  hypotheses and "batch" all the way back to the current time.

**BatchN** (for N=1,2,3,4,5) -- go back N ep-states, clear hypotheses
  and "batch" all the way back to the current time.
