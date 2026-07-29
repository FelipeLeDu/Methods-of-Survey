# Design-Based Survey Sampling

This project applies design-based survey sampling methods using a population dataset from Macaé, Rio de Janeiro. 

The project focuses on three classical design-based sampling methods:

```text
AAS  — Simple Random Sampling
AASE — Stratified Simple Random Sampling
Cluster Sampling — Sampling by clusters
```
The main outcome analyzed is the literacy rate. Since the full population dataset is available, the project can simulate repeated samples and compare the behavior of each sampling design against the population structure.

---

## Design-Based Survey Inference

In design-based survey methods, the validity of the estimator comes primarily from the sampling design. The population values are treated as fixed, and the randomness comes from the process used to select the sample.

This is different from model-based survey adjustment methods, such as MRP, where the final estimate depends more directly on a statistical model. In design-based surveys, the sampling plan is the central source of statistical validity.

## Sampling Designs 

1. **AAS — Simple Random Sampling**

   In Simple Random Sampling, every individual in the population has the same probability of being selected into the sample. A fixed sample size is randomly drawn from the full population without replacement, and the sample literacy rate is computed.
   
   This process is repeated many times to generate the sampling distribution of the AAS estimator.

2. **AASE — Stratified Simple Random Sampling**

   In stratified sampling, the population is first divided into groups called strata. Then, a random sample is drawn within each stratum. Finally combine the stratum estimates into one population estimate
   
   A good stratification variable should create groups that are: homogeneous within groups and heterogeneous between groups. This means that individuals inside the same stratum should be relatively similar with respect to the outcome of interest, while different strata should have different average outcomes.

   The project compares geographic alternatives such as district, subdistrict, and neighborhood. Neighborhoods present the strongest statistical profile for stratification because they have relatively low within-group variance and higher between-group variance. However, neighborhoods also create many strata, and some of them have small population sizes. 
   
   Because the total simulated sample size is fixed at 1,200 observations, using too many small strata could generate very small sample sizes inside some strata under proportional allocation. For this reason, subdistrict is chosen as the stratification variable.

3. **Cluster Sampling**

    Cluster sampling follows a different logic from stratification. In stratified sampling, the researcher samples from every group. In cluster sampling, the researcher selects only some groups.

    The basic logic is: divide the population into clusters, randomly select some clusters and fixed number of individuals is sampled within each selected cluster.

    This is closer to how many real field surveys are conducted. Instead of randomly selecting individuals from the entire municipality, the researcher first selects geographic areas and then collects interviews inside those areas.

    However, cluster sampling can increase sampling variance if the clusters are very different from each other. Ideally, clusters should be heterogeneous within and similar between. 

## Design Effects 

The project also computes design effects.

The design effect compares the variance of an estimator under a given sampling design with the variance under Simple Random Sampling. If the design effect is below 1, the design is more efficient than Simple Random Sampling. If the design effect is above 1, the design produces more sampling variance than Simple Random Sampling.
