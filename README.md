# Fairness in Winner-Take-All Markets

This repository contains replication files and data for [Fairness in
Winner-Take-All
Markets](https://swopec.hhs.se/nhheco/abs/nhheco2018_008.htm) authored
by Björn Bartling, Alexander W. Cappelen, Mathias Ekström, Erik
Ø. Sørensen and Bertil Tungodden.

## Looking at output
Browsing on github, it should be possible to click on the ".md" files
to look at rendered versions of the calculated results.

## Abstract

The paper reports the first experimental study on people’s fairness
views on extreme Income inequalities arising from winner-take-all
reward structures. We find that the majority of participants consider
extreme income inequality generated in winner-take-all situations as
fair, independent of the winning margin. Spectators appear to endorse
a “factual merit” fairness argument for no redistribution: the winner
deserves all the earnings because these earnings were determined by
his or her performance. Our findings shed light on the present
political debate on redistribution, by suggesting that people may
object less to certain types of extreme income inequality than
commonly assumed.


## Dataset

In the data subdirectory the dataset used for the paper is available in
Stata and .csv format, with 349 observations and 13 variables.

The variables and their coding:

1. id (numeric): Arbitrary subject id.

2. sessionid (string): Arbitrary session indicator.

3. T (categorical): Treatment. 
   1. WTA, no choice
   2. WTA
   3. WTA, no expectations
   4. Base

4. sex (categorical). Question asked. "What is your gender?"
   1. Male
   2. Female

5. age (numeric). Answer to "How old are you (in years)? With
censoring to groups, such that top group and lowest group has actual
age replaced by group averages. See data/scripts/anonymization.log for
details on this procedure.

6. political (categorical). Answer to "Would you describe yourself as
politically on the "left-wing" or on the "right-wing"? Alternatives:
   1. very left-wing
   2. somewhat left-wing
   3. neutral
   4. somewhat right-wing
   5. very right-wing

7. x1 (numeric): The number of problems solved by worker X. In Base treatment, this has no payment relevance.

8. x2 (numeric): The number of problems solved by worker Y. In Base treatment, this has no payment relevance, and outside of the Base treatment, x2>= x1.

9. e1 (numeric): Earnings made by worker X.

10. e2 (numeric): Earnings made by worker Y.

11. y1 (numeric): Income allocated to worker X.

12. y2 (numeric): Income allocated to worker Y.

13. shareX (numerical): Calculated share of earnings/income allocated
to worker X.
