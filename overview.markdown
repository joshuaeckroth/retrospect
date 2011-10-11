
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

Only one kind of meta-reasoning is active in this version of the
code. This meta-reasoner branches off exactly three epistemic states
prior and "batches" from there.

The criteria for activating meta-reasoning is the following:

  - there are some hypotheses that have no explainers; OR

  - the number of unexplained hypotheses is greater than 10% of the
    total number of hypotheses; OR

  - the "doubt" of the workspace is greater than 0.10