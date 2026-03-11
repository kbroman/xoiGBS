## xoiGBS 0.1.8 (2026-03-11)

- Add function inferXOloc() to infer crossover locations from genotype
  probabilities.

- Add function trimXO() for trimming tight double-crossovers from the
  output of inferXOloc().

- Add function randXOloc() for randomizing the estimated crossover
  locations, uniform within their intervals.


## xoiGBS 0.1.7 (2025-12-12)

- Revise sort_ids() and order_ids() to handle IDs with _dup at the end.


## xoiGBS 0.1.5 (2025-11-10)

- Added calc_genoprob_gbs to calculate genotype probabilities from
  genotyping-by-sequencing (GBS) data (counts of alleles at SNPs) for
  a backcross.


## xoiGBS 0.1.3 (2025-03-05)

- A new package, to analyze recombination and crossover interference
  from genotyping-by-sequencing (GBS) data on a backcross.
