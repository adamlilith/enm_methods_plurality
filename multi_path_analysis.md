# I have a dataset where independent teams have all done a similar task. I am now trying to understand how the combination of decisions they made in doing their task affects the outcomes of their tasks. Decisions can be numerical, binary, or categorical. I want to know what decisions drive differences between teams. Each team produced 100K outcomes, and these are comparable, so I can calculate a distance matrix. What options do I have?

Nice, a distance-based view fits this setup very naturally. With 26 teams and matched outcomes, you can treat each team as a point in a 100,000-dimensional outcome space and then ask: *which decisions explain distances between those points?*

Here’s a concrete **distance-based workflow** that matches what you’re after.

---

## 1. Define a distance between teams based on outcomes

You want a (26 \times 26) distance matrix (D_Y), where (D_Y(i,j)) measures how different team (i) and team (j) are in terms of their 100k outcomes.

Reasonable choices:

* **Euclidean distance** on the outcome vectors:
  [
  D_Y(i,j) = \sqrt{\sum_{t=1}^{100000} (y_{it} - y_{jt})^2}
  ]
  Sensitive to overall scale and large local differences.

* **1 − correlation** between outcome vectors:

  * Compute corr((\mathbf{y}_i, \mathbf{y}_j)), then
    (D_Y(i,j) = 1 - \text{corr}(\mathbf{y}_i, \mathbf{y}_j)).
  * Focuses on *pattern similarity* across tasks rather than mean level.

* **Mean absolute difference**:
  [
  D_Y(i,j) = \frac{1}{T}\sum_{t=1}^{T} |y_{it} - y_{jt}|
  ]
  Very interpretable (“average difference in outcome per task”).

Pick one that matches your substantive notion of “difference between teams.” The last one is often easiest to explain.

---

## 2. Define a distance between teams based on decisions

Now you need a (26 \times 26) decision-distance matrix (D_X), where each team has a 25-dimensional mixed-type decision profile.

Because you have numeric, binary, and categorical decisions, use a **mixed-type distance**, e.g.:

* **Gower distance** (handles numeric, binary, categorical):

  * In R: `cluster::daisy(decision_data, metric = 'gower')`.

This gives you a single matrix (D_X) summarizing *overall* decision difference between teams.

Optionally, you can also build **separate distance matrices** for subsets of decisions:

* (D^{(1)}): distances only on “early-step” decisions
* (D^{(2)}): distances on “middle-step” decisions
* (D^{(3)}): distances on “late-step” decisions
* or one per conceptually grouped set (e.g., parameter choices, data cleaning choices, etc.)

This will be useful when you want to see *which part* of the decision-making pipeline explains outcome differences.

---

## 3. First link: Mantel tests (overall association)

To answer “do decision differences explain outcome differences *at all*?” use a **Mantel test**:

* Correlate the entries of (D_Y) and (D_X).
* Permute rows/columns of one matrix to get a p-value.

In R (conceptually):

```r
library(vegan)
mantel(D_Y, D_X, method = 'pearson', permutations = 9999)
```

You can also run Mantel tests with different decision subsets (e.g., early vs late decisions) to see which stages have stronger association with outcome dissimilarity.

Interpretation:

* Large Mantel r: teams that are more different in decisions tend to be more different in outcomes.
* But it’s global and doesn’t tell you *which* decisions are driving that association.

---

## 4. Multiple regression on distance matrices (which decisions matter?)

To move from “there’s a relationship” to “which decisions are leading to differences,” use **multiple regression on distance matrices** (MRDM / MRM):

1. Build **one distance matrix per decision (or decision group)**:

   * For a binary decision: distance between teams is 0 if same choice, 1 if different.
   * For a categorical decision: 0 if same category, 1 if different; or use simple matching / appropriate distance.
   * For a numeric decision: absolute difference scaled (e.g., rescaled to [0,1]).

   Call these (D_1, D_2, \dots, D_{25}) (or fewer if grouped).

2. Regress the outcome distance matrix on these decision-distance matrices:

   Conceptually:
   [
   D_Y(i,j) = \beta_0 + \sum_{k=1}^{K} \beta_k D_k(i,j) + \varepsilon_{ij}
   ]

   In practice, you vectorize the upper triangle of each matrix and fit a regression with permutation tests.

   In R, the **ecodist** package’s `MRM()` is commonly used:

   ```r
   library(ecodist)
   # Example with 3 decisions; in practice you’d be parsimonious or use grouping
   mrm_result <- MRM(D_Y ~ D_dec1 + D_dec2 + D_dec3, nperm = 9999)
   ```

3. Interpret:

   * (\beta_k > 0): pairs of teams that differ more on decision (k) tend to differ more in outcomes.
   * The size and significance (via permutation) give you a sense of **which decisions (or decision sets) are most strongly associated with outcome differences, controlling for the others**.

Because you only have 26 teams → (\binom{26}{2} = 325) pairwise distances, you **can** fit a model with a fair number of predictors, but:

* Many decisions may be correlated (e.g., certain choices tend to go together).
* I’d strongly recommend **grouping decisions conceptually** (e.g., 5–10 “decision blocks”) or using regularization/selection logic rather than throwing all 25 in unconstrained.

This MRDM step is often the cleanest distance-based answer to your question:

> which decisions (or bundles of decisions) are associated with differences between teams’ outcome patterns?

---

## 5. Ordination + overlay decisions (visual explanation of differences)

To understand the *structure* of differences, you can:

1. **Ordinate the outcome distances** (D_Y):

   * PCoA: `vegan::cmdscale()` or `vegan::capscale()` with no constraints.
   * NMDS: `vegan::metaMDS()`.

   This gives you a 2D or 3D map where each point is a team, and geometric distances approximate (D_Y).

2. **Overlay decisions on this ordination**:

   * For numeric decisions: fit vectors via `vegan::envfit()`.
   * For factors: plot centroids/groupings, also via `envfit()` or `ordiellipse()`.

   This reveals which decisions point along the main axes of outcome variation:

   * e.g., “Teams that used Decision A = 1 are up/right, Decision A = 0 are down/left.”

3. Optionally:

   * Also ordinate the **decision distance** matrix (D_X) and compare it to the outcome ordination (e.g., Procrustes analysis, `vegan::procrustes()` + `protest()`).

This gives you an interpretable, visual answer to “what is leading to differences between teams?”: you literally see which decisions line up with the main gradients of outcome dissimilarity.

---

## 6. PERMANOVA / dbRDA (distance-based ANOVA / regression)

Another route is to treat the 100k outcomes as a “community data matrix” (teams × tasks) and use **PERMANOVA** or **distance-based redundancy analysis**:

* **PERMANOVA** (`vegan::adonis2`):

  * Response: the outcome matrix or precomputed distance (D_Y),
  * Predictors: the decisions (numeric + factors).
  * Test each decision’s contribution via permutations.

  Conceptually:

  ```r
  adonis2(D_Y ~ dec1 + dec2 + dec3 + ..., permutations = 9999)
  ```

  This is distance-based ANOVA: which decisions explain variance in the outcome distances?

* **dbRDA** (`vegan::capscale`):

  * Ordination constrained directly by the decision variables.
  * You get axes that are *linear combinations of decisions* that best explain outcome structure.

These methods are kind of the **multivariate analogue** to regression, but fully distance-based.

---

## 7. Causal caution

Distance-based methods are excellent at **answering your central descriptive question**:

> Which decisions / decision bundles are most associated with differences in team outcomes?

But they are still fundamentally **associational**:

* Teams may differ in unobserved characteristics (experience, data quality, etc.) that both influence decisions and outcomes.
* So I’d frame the results as:
  “Teams that differ in decisions X, Y, Z tend to have substantially different outcome profiles,”
  and then bring in subject-matter knowledge to argue why those decisions are plausibly causal (or not).

If you want to *nudge* toward a more causal interpretation, you can:

* Group decisions into stages (early vs middle vs late) and see whether later-stage decisions still explain distances after accounting for earlier ones in MRDM or PERMANOVA.
* Look for near-matched pairs of teams differing mainly in specific decisions and check whether their outcome distance is large.

---

## 8. If you’d like, next step

If you tell me:

* Rough types of your decisions (e.g., “5 preprocessing choices, 10 modeling choices, 10 tuning choices”),
* And what software you’re in (I’m assuming R),

I can sketch a concrete, R-flavored workflow like:

1. How to build (D_Y) and communicate its meaning.
2. How to construct decision-distance matrices for individual decisions or blocks.
3. Example `MRM` / `adonis2` / `envfit` calls and what their outputs mean in your context.
