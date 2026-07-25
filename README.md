# Methods of Survey

This directory contains applied survey methods projects developed from assignments in an undergraduate course on survey research taught by Professor Felipe Nunes, CEO of Quaest. 

The course introduced both design-based and model-based approaches to survey inference, covering sampling design, post-survey weighting, raking, and MRP.

The goal of this directory is to document the full survey workflow, from sampling design to post-survey correction methods.

The portfolio is divided into three main parts. The first project focuses on design-based survey methods, where the quality of inference depends primarily on the sampling design. The second and third projects focus on post-processing methods, which are used when the collected sample is not fully representative of the target population and needs to be adjusted after data collection.

---

## 1. Design-Based Survey

The first project studies design-based survey sampling methods using a population dataset from Macaé, Rio de Janeiro. The exercise compares different sampling designs and evaluates how each design affects the sampling distribution of an estimator.

The project simulates repeated samples using:

- Simple Random Sampling
- Stratified Simple Random Sampling
- Cluster Sampling

The main outcome analyzed is the literacy rate. Since the full population dataset is available, the simulations can compare the behavior of each sampling design against the true population value.

Project folder:

```text
design based survey/
```

---

## 2. Model-Based Survey

The second project focuses on model-based survey adjustment using Multilevel Regression and Poststratification, commonly known as MRP.

MRP is a post-processing method used to estimate population-level quantities when the sample is not directly representative of the target population. Instead of relying only on design weights or category-level weighting, MRP combines statistical modeling with population information.

MRP can produce more stable estimates than simple weighting in cases where the sample is sparse across many categories.


Project folder:

```text
model based survey/
```
