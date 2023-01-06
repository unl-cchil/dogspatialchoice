
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Do owners know how impulsive their dogs are?

- Created on 2022-03-22 by Jeffrey R. Stevens
  (<jeffrey.r.stevens@gmail.com>)
- Finalized on 2022-03-22

This repository provides the reproducible research materials for our
project that investigates the dog spatial impulsivity and owner
perceptions of dog impulsivity. This includes the following:

- Data
- R script for data analysis
- R Markdown file for the manuscript
- R Markdown file for supplementary materials

## Citation

If you use any of these materials, please cite:

Stevens, J.R., Mathias, M., Herridge, M., Hughes-Duval, K., Wolff, L.M.,
& Yohe, M. (2022). Do owners know how impulsive their dogs are? Animal
Behavior and Cognition, 9(3):261-286.
<https://doi.org/10.26451/abc.09.03.02.2022>

## Summary

Two data sets were collected. Data set 1 involved 117 dog-owner pairs
from Lincoln, Nebraska, USA between Nov 2018 - Jul 2021. Data set 2
involved 103 dog-owner pairs from Lincoln, Nebraska, USA between Aug
2020 - Oct 2021. In the first data file, each row represents behavioral
and survey responses from a single dog. In the second data file, each
row represents the responses of a single owner for a particular survey
scale.

## License

All materials presented here are released under the Creative Commons
Attribution 4.0 International Public License (CC BY 4.0). You are free
to:

- Share — copy and redistribute the material in any medium or format
- Adapt — remix, transform, and build upon the material for any purpose,
  even commercially. Under the following terms:
- Attribution — You must give appropriate credit, provide a link to the
  license, and indicate if changes were made. You may do so in any
  reasonable manner, but not in any way that suggests the licensor
  endorses you or your use.

No additional restrictions — You may not apply legal terms or
technological measures that legally restrict others from doing anything
the license permits.

## Files

### Data files

`stevens_etal_2022_data1.csv` (primary behavioral and survey responses)

- experiment - study number
- date - date of experimental session
- subject - subject ID
- max_distance - maximum distance traveled by dog
- dog_age - dog age in years
- dog_age_acquisition - dog age at time of acquisition in years
- dog_breed - dog breed
- dog_sex - dog sex
- dog_neutered - dog neuter status (Yes or No)
- dog_acquision - means of acquiring dog (Adopted, Bred yourself, Other,
  Purchased from breeder)
- dog_weight - dog weight in pounds
- akccgc - whether dog is AKC Canine Good Citizen certified
- rate_dog_trained - owner’s evaluation of dog’s level of training on a
  scale of 1-10, with 10 being best
- cbarq_training_score - mean owner’s response to CBARQ training scale
  (only Study 2)
- dog_behavior_bennett_disobedient_score - Bennett & Rohlf (2007)
  disobedience score
- dog_behavior_bennett_aggressive_score - Bennett & Rohlf (2007)
  aggression score
- dog_behavior_bennett_nervous_score - Bennett & Rohlf (2007)
  nervousness score
- dog_behavior_bennett_destructive_score - Bennett & Rohlf (2007)
  destructiveness score
- dog_behavior_bennett_excitable_score - Bennett & Rohlf (2007)
  excitability score
- dog_obedience_hiby_score - Hiby et al. (2004) obedience score
- dog_problematic_behaviors_hiby_score - Hiby et al. (2004) problematic
  behaviors score
- dias_behavioral_regulation_score - Wright et al., 2011 DIAS behavioral
  regulation score
- dias_aggression_score - Wright et al., 2011 DIAS aggression score
- dias_responsiveness_score - Wright et al., 2011 DIAS responsiveness
  score
- dias_overall_score - Wright et al., 2011 DIAS overall score (mean of
  other scores)
- mdors_score - Monash Dog Owner Relationship Scale (Dwyer et al., 2006)
- separation_anxiety_yesno - presence of separation anxiety (Yes or No)
- personality_extraversion_score - brief Big-Five personality
  extraversion scale (Gosling et al., 2003)
- personality_agreeableness_score - brief Big-Five personality
  agreeableness scale (Gosling et al., 2003)
- personality_conscientiousness_score - brief Big-Five personality
  conscientiousness scale (Gosling et al., 2003)
- personality_stability_score - brief Big-Five personality stability
  scale (Gosling et al., 2003)
- personality_openness_score - brief Big-Five personality openness scale
  (Gosling et al., 2003)
- crt_score - Cognitive Reflection Task score (Frederick, 2005)
- numeracy_score - Berlin Numeracy Test score (Cokely et al., 2012)
- owner_gender - owner gender
- owner_marital_status - owner marital status
- other_dogs - presence of other dogs in the home (Yes or No)
- household_income - annual household income category
- response - response to question

`stevens_etal_2022_data2.csv` (item-specific data for calculating
reliability)

- experiment - study number
- survey - name of survey
- item_1-item_13 - individual items (surveys differ on number of items,
  so NAs represent no items)

### R code

`stevens_etal_2022_rcode.R` - code for running computations and
generating figures

### R Markdown documents

`stevens_etal_2022.Rmd` - R Markdown document with R code embedded for
main manuscript `stevens_etal_2022_SM.Rmd` - R Markdown document with R
code embedded for supplementary materials

### Installation

To reproduce these results, first clone or unzip the Git repository into
a folder. Then, ensure that a subfolder named “figures” is in the
folder. Next, open `stevens_etal_2022_rcode.R` in
[RStudio](https://rstudio.com) or another R interface and ensure that
all packages mentioned at the top of the script are installed. Once all
packages are installed, run the script in R using
`source("stevens_etal_2022_rcode.R")`.

Once the script runs without errors, you can compile the R Markdown
document `stevens_etal_2022.Rmd.` Open this file in RStudio and ensure
that you have packages [{knitr}](https://yihui.org/knitr/) and
[{rmarkdown}](https://rmarkdown.rstudio.com/) installed. Once installed,
use {knitr} to render the document (control-shift-K). Use the same
process to render `stevens_etal_2022_SM.Rmd`.

# Dataset Metadata

The following table is necessary for this dataset to be indexed by
search engines such as <a href="https://g.co/datasetsearch">Google
Dataset Search</a>.

<div itemscope="" itemtype="http://schema.org/Dataset">

<table>
<tr>
<th>
property
</th>
<th>
value
</th>
</tr>
<tr>
<td>
name
</td>
<td>
<code itemprop="name">Dog spatial impulsivity and owner perceptions of
dog impulsivity dataset</code>
</td>
</tr>
<tr>
<td>
description
</td>
<td>
<code itemprop="description">The dataset from the paper [Do owners know
how impulsive their dogs
are?](https://doi.org/10.26451/abc.09.03.02.2022). Two data sets were
collected. Data set 1 involved 117 dog-owner pairs from Lincoln,
Nebraska, USA between Nov 2018 - Jul 2021. Data set 2 involved 103
dog-owner pairs from Lincoln, Nebraska, USA between Aug 2020 - Oct 2021.
In the first data file, each row represents behavioral and survey
responses from a single dog. In the second data file, each row
represents the responses of a single owner for a particular survey
scale.</code>
</td>
</tr>
</tr>
<tr>
<td>
url
</td>
<td>
<code itemprop="url">https://github.com/unl-cchil/dogspatialchoice</code>
</td>
</tr>
<tr>
<td>
sameAs
</td>
<td>
<code itemprop="sameAs">https://github.com/unl-cchil/dogspatialchoice</code>
</td>
</tr>
<tr>
<td>
citation
</td>
<td>
<code itemprop="citation">https://doi.org/10.26451/abc.09.03.02.2022</code>
</td>
</tr>
<tr>
<td>
license
</td>
<td>

<div itemscope="" itemtype="http://schema.org/CreativeWork"
itemprop="license">

<table>
<tr>
<th>
property
</th>
<th>
value
</th>
</tr>
<tr>
<td>
name
</td>
<td>
<code itemprop="name">CC BY-SA 4.0</code>
</td>
</tr>
<tr>
<td>
url
</td>
<td>
<code itemprop="url">https://creativecommons.org/licenses/by-sa/4.0/</code>
</td>
</tr>
</table>

</div>

</td>
</tr>
</table>

</div>
