## Summary

Digital initiatives compete for the same budget and people. This methodology decides which ones deserve to be executed, with an **evaluation effort proportional to what is at stake**, and checks afterwards whether the promised value was actually delivered. Its results are used to make future estimates better.

The lifecycle has two views and six steps:

| View | Step | Purpose | Who | When |
|---|---|---|---|---|
| **Appraisal** (pre-execution) | 1 · Registration & RICE | Describe the idea and score it qualitatively | Everyone | Always |
| | 2 · 4MC valuation | Quantify value and cost with traceable formulas | Superuser | Above the valuation gate |
| | 3 · Expert review | Independent evaluation, with its own 4MC figures | Superuser | Above the review gate |
| | 4 · Decision | Prioritize or reject | Superuser | When the appraisal is complete |
| **Realisation** (post-execution) | 5 · Execution | Deliver the initiative | Superuser | After prioritisation |
| | 6 · Value audit | Measure the value actually delivered and its adoption | Superuser | After closure |

### Principles

1. **Proportionality.** Every initiative gets a quick RICE score. A monetary valuation is required only above the *valuation gate*, and an expert review only above the *review gate*. Small ideas move fast; large bets get scrutiny.
2. **One language across the lifecycle.** The same five metrics (**4MC**: Production, Reserves, Monetary, Time saved and Cost) are used for the estimate, the expert review and the audit, so the three figures are directly comparable.
3. **Value and cost are kept apart.** Cost is never subtracted from or added to value. Both are reported, together with the value/cost ratio.
4. **Traceability.** Every figure is stored with its formula, parameters, comment, author and date. Any valuation can be re-read and recomputed.
5. **Learning loop.** Two calibrated regressions anticipate the value from the RICE inputs, and the realised value from the ex-ante evaluation. They are recalibrated as audits accumulate and published as versions.
6. **Open registration, controlled evaluation.** Anyone can propose an initiative. Evaluations and decisions are made by superusers.
7. **Transparent process.** Every step is visible to everyone, and missing evaluations are highlighted. User activity is logged so the process itself can be analysed and improved.

## 1 · Registration & RICE

The initiative is registered with a name, a brief description, the owner, the business unit, an estimated cost and planned dates. The **RICE inputs are recorded in the same form**.

**RICE** is a qualitative score used to compare and rank initiatives:

`RICE score = log10(users reached) × Impact × Confidence ÷ Effort`

* **Reach** is the number of users impacted. Its base-10 logarithm is used, so going from 10 to 100 users counts as much as going from 1,000 to 10,000. This keeps very large audiences from dominating the ranking.
* **Impact** and **Effort** are t-shirt sizes. **Confidence** goes from *Moonshot* to *Certain*.

{{rice_weights_table}}

*Reach × Impact × Confidence* is the qualitative **value**. Dividing it by the effort weight gives the score. The Portfolio chart plots the value against effort, so a high and to the left position is a quick win.

**Locking.** Once saved, the registration and its RICE are locked for the contributor who created them. Only a superuser can edit them. This keeps the initial assessment honest and comparable.

**Anticipated value.** While the RICE inputs are entered, the form shows the ex-ante value expected for similar initiatives, with a range. It comes from the published calibrated model (see *Calibrated value models*).

## Evaluation gates

Two gates decide how much evaluation an initiative needs. They are configuration parameters.

| Gate | Triggered when | Requires |
|---|---|---|
| **Valuation gate** | Effort ≥ **{{gate2_effort_min}}** or RICE score ≥ **{{gate2_score_min}}** | 4MC valuation |
| **Review gate** | Ex-ante value ≥ **{{gate3_value_min_mm_usd}} mm USD** or cost ≥ **{{gate3_cost_min_mm_usd}} mm USD** | Expert review |

* The **ex-ante value** is the expert-review value when the review approved the initiative, otherwise the 4MC estimate.
* The **planned cost** is the approved review's cost, otherwise the 4MC cost estimate, otherwise the cost entered at registration.

The status always shows the next pending step: *Registered → 4MC valuation → Expert review → Ready*. When a required step is missing, it is highlighted in amber on the Initiative tab and listed on the Portfolio.

## 2 · 4MC valuation

The 4MC valuation quantifies the initiative with five metrics:

| Metric | Unit | Converted to mm USD by | Horizon |
|---|---|---|---|
| **P** · Production | Average incremental BOPD over the year | BOPD × {{days_per_year}} days × net back {{netback_usd_bbl}} USD/bbl | Annual |
| **R** · Reserves | MMbbl, by category | MMbbl × value per barrel of the category (table below) | One-off |
| **M** · Monetary | mm USD per year | As entered | Annual |
| **T** · Time saved | khours per year | khours × 1,000 × {{time_value_usd_h}} USD/h | Annual |
| **C** · Cost | mm USD | As entered; **kept separate from value** | Implementation period |

{{reserve_table}}

**Time saved** is valued at the average salary ({{avg_salary_usd_year}} USD/year over {{work_hours_year}} h) multiplied by a productivity coefficient of **{{time_productivity_coef}}**. The coefficient reflects that time freed from routine work is redeployed to more productive tasks.

**Calculation lines.** A valuation is made of lines. Each line uses a method of one metric, for example:

* Production: direct estimate; jobs × rate per job × success-rate uplift; uptime gain; decline mitigation.
* Reserves: direct estimate; recovery-factor uplift on the oil in place.
* Monetary: direct estimate; events × saving per event; avoided cost × reduction of its probability.
* Time saved: direct estimate; users × hours per week × weeks; tasks × minutes saved.
* Cost: capex + opex over the period; effort in person-months × rate + licences.

Every line stores its parameters, the human-readable formula and a comment. Example: *10 workovers per year, each producing 30 BOPD, with the success rate increased from 60% to 70%*:

`10 jobs × 30 BOPD × (70% − 60%) = {{example_workover_bopd}} BOPD → {{example_workover_mm}} mm USD`

The **4MC value** is P + R + M + T (annual metrics plus one-off reserves). Cost is shown next to it, with the value/cost ratio. A valuation with cost lines only does not complete the step: at least one value metric is needed.

## 3 · Expert review

Above the review gate, an expert (a superuser, typically from planning) evaluates the initiative independently and records:

* a **decision**:
  * **Approve**: the review figures become the ex-ante value and cost;
  * **Rework**: the initiative stays pending until a new review;
  * **Reject**: the initiative is rejected;
* the expert's **own 4MC figures** (P, R, M, T and C). These are pre-filled with the 4MC estimate and can be adjusted, e.g. with a risk haircut or a cost contingency;
* notes: assumptions, NPV, risks, conditions.

The form shows the resulting value, the value/cost ratio, and the **anticipated realised value**. That is what similar initiatives actually delivered relative to their ex-ante value, according to the published model.

## 4 · Decision

When the appraisal is complete (status *Ready*), a superuser **prioritizes** or **rejects** the initiative, with an optional comment. A prioritized initiative can be moved back to *Ready* or rejected before execution starts.

The **Portfolio** tab supports the decision:

* the value-vs-effort chart: Reach × Impact × Confidence, ex-ante value, or value/estimate, against effort. Gates are drawn as dashed lines; the score gate appears as a staircase;
* KPIs on pipeline, committed value, value/cost and realisation;
* the register with the RICE rank, values, costs and missing evaluations.

## 5 · Execution

A prioritized initiative is **started** and later **closed** from the decision bar. Closing the execution opens the value audit.

## 6 · Value audit

After closure, ideally when the solution has been in use long enough to measure its effect (typically 6–12 months), a superuser records:

* the **actual 4MC figures** (P, R, M, T) and the **actual cost** (C);
* the **adoption**: the share of the intended users actually using the solution;
* a conclusion and lessons learned.

The audit compares estimate, expert review and actual, metric by metric. It reports the **realisation rate** (audited value ÷ ex-ante value) and the cost overrun (actual ÷ planned cost). Low adoption often explains a low realisation rate.

## Calibrated value models

Two log-linear regressions connect the lifecycle stages:

| Model | Predicts | From |
|---|---|---|
| RICE → ex-ante value | Ex-ante value | log10(users), log Impact, log Confidence, log Effort |
| Ex-ante → realised value | Audited value | log ex-ante value, log Confidence, log Effort |

* **Why logarithms.** RICE is multiplicative, so a log-linear model gives *elasticities*. For example, doubling the impact multiplies the value by 2 raised to the impact coefficient.
* **Range.** The estimate is a median with a {{p_low}}–{{p_high}} range: the {{interval_pct}}% prediction interval of the regression, transformed back to mm USD. A wide range means the RICE inputs explain little of the value so far.
* **Few data points.** Below **{{value_model_min_n}}** observations a simpler one-predictor model is used. Below 4, no estimate is given.
* **Calibration.** In *Admin → Value models* a superuser fits a candidate on the current data. It is compared with the published version on R², range width, interval coverage and coefficients, then published with a comment. Every version is kept and can be re-activated. Estimates change only when a new version is published.

The models are a learning tool, not a substitute for the valuation: they show how good past estimates have been and how much to trust a new one.

## Roles and responsibilities

| Action | Contributor | Superuser |
|---|:-:|:-:|
| Register an initiative with its RICE | ✓ | ✓ |
| Edit a registration or its RICE after saving | | ✓ |
| 4MC valuation, expert review, decisions, value audit | | ✓ |
| Calibrate and publish value models; administration | | ✓ |

Superusers are identified by their Posit Connect group or user name.

## KPIs and continuous improvement

* **Pipeline, committed and realised value** (mm USD) and **value/cost** of the committed portfolio.
* **Realisation rate**: audited value ÷ ex-ante value. Measures the accuracy of the estimates.
* **Adoption**: mean adoption of the audited initiatives.
* **Gate compliance**: share of the required 4MC valuations and expert reviews actually done.
* **Decision lead time**: median days from registration to the prioritise or reject decision.
* **Open alerts**: pending process actions.

User activity is logged and grouped into the lifecycle steps (*Admin → Process log*). The resulting event log can be analysed with process-mining tools to find bottlenecks and rework.

## Glossary

* **BOPD**: barrels of oil per day.
* **MMbbl**: million barrels.
* **1P / 2P / 3P**: proved; proved + probable; proved + probable + possible reserves.
* **Contingent resources**: discovered volumes not yet commercial.
* **Net back**: net value per barrel after costs, used to monetise production.
* **Ex-ante value**: value expected before execution (expert review if approved, otherwise the 4MC estimate).
* **Realised value**: value measured by the audit.
* **Realisation rate**: realised value ÷ ex-ante value.
* **Adoption**: share of the intended users actively using the solution.
* **Gate**: a threshold that makes an additional evaluation step mandatory.
